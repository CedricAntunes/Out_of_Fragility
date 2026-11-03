# ==============================================================================
# 02_vdem_index.R: the V-Dem fragility index (PCA) and annual status -----------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Builds the first principal component of four V-Dem indicators, oriented so
# that HIGHER VALUES MEAN LESS FRAGILITY:
#   v2xnp_regcorr  regime corruption               (reversed)
#   v2caviol       political violence by non-state actors (reversed)
#   v2x_rule       rule of law
#   v2clrspct      rigorous and impartial public administration
#
# Four variants, each with and without the rule-of-law component:
#   VDEM_INDEX        fixed scale (PRIMARY). Components standardized over the
#                     pooled 1971-2022 universe, so a given value means the same
#                     level of institutional weakness in every year.
#   VDEM_INDEX_WY     within-year scale. Components standardized within each
#                     year before the pooled PCA. This is the recipe of the
#                     original (Excel) index: year-relative, mean zero each year.
#   VDEM_INDEX_LEGACY_V14   the original index as delivered (V-Dem v14, Excel),
#                     kept only to reproduce results reported before Nov 2026.
#
# Status (Fragile / Transitioning / Non Fragile) uses annual P30 / P35 over the
# universe (see annual_status() in 00_config.R).
#
# Outputs: data/intermediate/vdem_index.rds
#          data/final/pca_summary.csv (loadings and variance explained)
# ==============================================================================

source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

universe <- read_intermediate("universe")
vdem <- load_vdem()

comp <- vdem |>
  as_tibble() |>
  transmute(ISO_CODE_3 = country_text_id, YEAR = as.integer(year),
            VDEM_REGIME_CORRUPTION  = v2xnp_regcorr,
            VDEM_POLITICAL_VIOLENCE = v2caviol,
            VDEM_RULE_OF_LAW        = v2x_rule,
            VDEM_IMPARTIAL_ADMIN    = v2clrspct) |>
  inner_join(universe, by = c("ISO_CODE_3", "YEAR"))

stopifnot(nrow(comp) == nrow(universe), !anyNA(comp))

# Orientation: higher = less fragile. Checked empirically: corruption and
# violence correlate negatively with rule of law in the raw V-Dem scale.
stopifnot(cor(comp$VDEM_REGIME_CORRUPTION, comp$VDEM_RULE_OF_LAW) < 0,
          cor(comp$VDEM_POLITICAL_VIOLENCE, comp$VDEM_RULE_OF_LAW) < 0)
oriented <- with(comp, data.frame(
  corruption = -VDEM_REGIME_CORRUPTION,
  violence   = -VDEM_POLITICAL_VIOLENCE,
  rule_of_law = VDEM_RULE_OF_LAW,
  admin      = VDEM_IMPARTIAL_ADMIN
))

# PCA helper: first component, oriented so all loadings are positive, then
# standardized to mean 0 and SD 1 over the pooled universe.
pc1_index <- function(X, label) {
  p <- stats::prcomp(X, center = TRUE, scale. = TRUE)
  s <- sign(sum(p$rotation[, 1]))
  load1 <- p$rotation[, 1] * s
  if (any(load1 <= 0)) stop(label, ": first component does not load positively on every input.")
  score <- p$x[, 1] * s
  list(
    score = (score - mean(score)) / stats::sd(score),
    summary = data.frame(index = label, component = names(load1),
                         loading = unname(load1),
                         variance_explained_pc1 = summary(p)$importance[2, 1],
                         n = nrow(X))
  )
}

within_year_z <- function(X, year) {
  as.data.frame(lapply(X, function(x) ave(x, year, FUN = function(u) (u - mean(u)) / stats::sd(u))))
}

no_rol <- setdiff(names(oriented), "rule_of_law")
fits <- list(
  VDEM_INDEX            = pc1_index(oriented,                                "VDEM_INDEX"),
  VDEM_INDEX_NORL       = pc1_index(oriented[no_rol],                        "VDEM_INDEX_NORL"),
  VDEM_INDEX_WY         = pc1_index(within_year_z(oriented, comp$YEAR),        "VDEM_INDEX_WY"),
  VDEM_INDEX_NORL_WY    = pc1_index(within_year_z(oriented[no_rol], comp$YEAR), "VDEM_INDEX_NORL_WY")
)

index <- comp
for (nm in names(fits)) index[[nm]] <- fits[[nm]]$score

# Annual status for each current variant ---------------------------------------
index <- index |>
  mutate(
    VDEM_STATUS            = annual_status(VDEM_INDEX,         YEAR),
    VDEM_STATUS_NORL       = annual_status(VDEM_INDEX_NORL,    YEAR),
    VDEM_STATUS_WY         = annual_status(VDEM_INDEX_WY,      YEAR),
    VDEM_STATUS_NORL_WY    = annual_status(VDEM_INDEX_NORL_WY, YEAR)
  )

# Legacy index (V-Dem v14, Excel PCA) -------------------------------------------
# Delivered as country-name rows; mapped to ISO3 with explicit exceptions.
legacy_iso <- function(name) {
  countrycode(name, "country.name", "iso3c", warn = FALSE,
              custom_match = c("Kosovo" = "XKX", "Turkey" = "TUR",
                               "German Democratic Republic" = "DDR",
                               "German dem_newocratic Republic" = "DDR"))
}
legacy_full <- read_csv(raw_path("legacy_index", "vdem_new_normalized.csv"), show_col_types = FALSE) |>
  transmute(ISO_CODE_3 = legacy_iso(`Country Name`), YEAR = as.integer(Year),
            VDEM_INDEX_LEGACY_V14 = `Loading Factor 1 Normalized`)
legacy_norl <- read_excel(raw_path("legacy_index", "PCA-VDEM without RoL.xlsx")) |>
  transmute(ISO_CODE_3 = legacy_iso(`Country Name`), YEAR = as.integer(Year),
            VDEM_INDEX_NORL_LEGACY_V14 = `Loading Factor 1 Normalized`)
for (lg in list(legacy_full, legacy_norl)) {
  if (anyNA(lg$ISO_CODE_3)) stop("Unmapped legacy country names.")
  assert_unique_keys(lg, label = "legacy index")
}

# The legacy statuses are computed on the legacy panel only (the universe minus
# Bahrain 1971-2001 and three Papua New Guinea years), which reproduces the
# annual percentiles used in every result reported before November 2026.
legacy_panel_keys <- read_csv(raw_path("legacy_index", "legacy_panel_keys.csv"),
                              show_col_types = FALSE) |>
  mutate(YEAR = as.integer(YEAR), IN_LEGACY_PANEL = 1L)

index <- index |>
  left_join(legacy_full, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(legacy_norl, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(legacy_panel_keys, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(IN_LEGACY_PANEL = coalesce(IN_LEGACY_PANEL, 0L))

legacy_rows <- index$IN_LEGACY_PANEL == 1L
index$VDEM_STATUS_LEGACY_V14 <- NA_character_
index$VDEM_STATUS_NORL_LEGACY_V14 <- NA_character_
index$VDEM_STATUS_LEGACY_V14[legacy_rows] <-
  annual_status(index$VDEM_INDEX_LEGACY_V14[legacy_rows], index$YEAR[legacy_rows])
index$VDEM_STATUS_NORL_LEGACY_V14[legacy_rows] <-
  annual_status(index$VDEM_INDEX_NORL_LEGACY_V14[legacy_rows], index$YEAR[legacy_rows])

assert_unique_keys(index, label = "vdem_index")

# PCA summary (Table A5) ---------------------------------------------------------
pca_summary <- do.call(rbind, lapply(fits, `[[`, "summary"))
write_csv(pca_summary, file.path(FINAL_DIR, "pca_summary.csv"))
print(pca_summary, row.names = FALSE, digits = 3)

save_intermediate(index |> select(-COUNTRY_NAME), "vdem_index")
