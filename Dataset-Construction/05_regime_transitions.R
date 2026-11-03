# ==============================================================================
# 05_regime_transitions.R: democracy and regime transitions (BMR, ERT, V-Dem) --
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Sources
#   BMR  Boix, Miller and Rosato, democracy-v4.0.csv (1800-2020).
#   ERT  Episodes of Regime Transformation v16 (Maerz et al.), episodes.rda,
#        from github.com/vdeminstitute/ERT (built on V-Dem v16).
#   V-Dem v16: v2x_polyarchy (electoral democracy), v2x_regime (Regimes of
#        the World).
#
# BMR territory mapping follows Polity (10_polity.R): Yugoslavia (to 1991) and
# the Federal Republic of Yugoslavia (1992-2005) -> SRB; Czechoslovakia -> CZE;
# Yemen North -> YEM; Ethiopia incl. Eritrea -> ETH. For duplicated years the
# successor unit is kept. V-Dem's copy of BMR leaves Serbia 1992-2005 missing;
# the original file does not, so no separate Serbia patch is needed.
#
# Windows: *_RECENT5 = any event in t-4 .. t (computed on the source panel, so
# the hazard pipeline's one-year lag yields t-5 .. t-1). NA when any year in the
# window is unobserved (BMR ends in 2020).
#
# Output: data/intermediate/regime_transitions.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

SOURCE_YEARS <- (YEAR_START - 4L):YEAR_END

# BMR ---------------------------------------------------------------------------
BMR_TERRITORY <- c(`345` = "SRB", `347` = "SRB", `315` = "CZE", `678` = "YEM",
                   `530` = "ETH", `816` = "VNM", `260` = "DEU")
BMR_PRIORITY <- c(`347` = 2, `260` = 2, `678` = 2, `530` = 2, `816` = 2)  # predecessor loses ties

bmr_raw <- read_csv(raw_path("bmr", "democracy-v4.0.csv"), show_col_types = FALSE)

bmr <- bmr_raw |>
  filter(year %in% SOURCE_YEARS) |>
  mutate(
    ccode = as.integer(ccode), YEAR = as.integer(year),
    ISO_CODE_3 = coalesce(unname(BMR_TERRITORY[as.character(ccode)]),
                          countrycode(ccode, "cown", "iso3c", warn = FALSE),
                          countrycode(country, "country.name", "iso3c", warn = FALSE)),
    priority = coalesce(unname(BMR_PRIORITY[as.character(ccode)]), 1)
  ) |>
  filter(!is.na(ISO_CODE_3)) |>
  group_by(ISO_CODE_3, YEAR) |>
  arrange(priority, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(
    ISO_CODE_3, YEAR,
    BOIX_DEMOCRACY      = as.integer(democracy),
    BMR_DEMOCRATIZATION = as.integer(democracy_trans == 1),
    BMR_BREAKDOWN       = as.integer(democracy_trans == -1),
    BMR_ANY_TRANSITION  = as.integer(democracy_trans != 0)
  ) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(across(c(BMR_DEMOCRATIZATION, BMR_BREAKDOWN, BMR_ANY_TRANSITION),
                ~ recent_any(.x, YEAR, 5L, prebirth_zero = TRUE), .names = "{.col}_RECENT5")) |>
  ungroup()

assert_unique_keys(bmr, label = "bmr")

# ERT v16 -----------------------------------------------------------------------
e <- new.env(); load(raw_path("ert", "ert_v16_episodes.rda"), envir = e)
ert <- e$episodes |>
  as_tibble() |>
  transmute(ISO_CODE_3 = country_text_id, YEAR = as.integer(year),
            reg_trans, aut_ep = as.integer(aut_ep), dem_ep = as.integer(dem_ep)) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  # Onsets and windows are computed on the full ERT history (from 1900), so an
  # episode already under way in the late 1960s is not mistaken for an onset.
  mutate(
    ERT_DEMOCRATIZATION = as.integer(reg_trans == 1),
    ERT_DEM_BREAKDOWN   = as.integer(reg_trans == -1),
    ERT_ANY_TRANSITION  = as.integer(reg_trans != 0),
    ERT_AUT_EPISODE     = aut_ep,
    ERT_AUT_ONSET       = as.integer(aut_ep == 1L & coalesce(lag(aut_ep), 0L) == 0L),
    ERT_DEM_EPISODE     = dem_ep,
    ERT_DEM_ONSET       = as.integer(dem_ep == 1L & coalesce(lag(dem_ep), 0L) == 0L)
  ) |>
  mutate(across(c(ERT_DEMOCRATIZATION, ERT_DEM_BREAKDOWN, ERT_ANY_TRANSITION,
                  ERT_AUT_ONSET, ERT_DEM_ONSET),
                ~ recent_any(.x, YEAR, 5L, prebirth_zero = TRUE), .names = "{.col}_RECENT5")) |>
  ungroup() |>
  filter(YEAR %in% SOURCE_YEARS) |>
  select(-reg_trans, -aut_ep, -dem_ep)

assert_unique_keys(ert, label = "ert")

# V-Dem regime measures -----------------------------------------------------------
vdem_regime <- load_vdem() |>
  as_tibble() |>
  transmute(ISO_CODE_3 = country_text_id, YEAR = as.integer(year),
            ELECTORAL_DEMOCRACY_SCORE = v2x_polyarchy,
            VDEM_REGIME_TYPE = as.integer(v2x_regime)) |>
  filter(YEAR %in% SOURCE_YEARS)

# Combine on the universe -----------------------------------------------------------
universe <- read_intermediate("universe")
regime <- universe |>
  select(ISO_CODE_3, YEAR) |>
  left_join(bmr, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(ert, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(vdem_regime, by = c("ISO_CODE_3", "YEAR"))

binary <- setdiff(names(regime), c("ISO_CODE_3", "YEAR", "ELECTORAL_DEMOCRACY_SCORE", "VDEM_REGIME_TYPE"))
assert_binary(regime, binary, label = "regime_transitions")
assert_unique_keys(regime, label = "regime_transitions")

save_intermediate(regime, "regime_transitions")
