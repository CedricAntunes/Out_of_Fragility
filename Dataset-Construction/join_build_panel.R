# ==============================================================================
# build_panel.R: assemble standardized_data_final ------------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Joins every module onto the universe (01) and keeps exactly the variables in
# codebook.R, in that order. Values are in raw units: nothing is standardized
# here.
#
# Missing-value rules at merge time:
#   * UCDP modules list only countries that ever had a conflict; universe
#     country-years absent from them are coded 0 (no conflict), within UCDP
#     coverage (through 2022).
#   * Every other source keeps NA where it has no observation (coverage gaps
#     are real missing data, not zeros).
#
# Outputs: data/final/standardized_data_final.rds / .csv
#          data/final/codebook.csv
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/codebook.R")

universe <- read_intermediate("universe")
by_keys <- c("ISO_CODE_3", "YEAR")

zero_fill <- function(df, vars) mutate(df, across(all_of(vars), ~ coalesce(as.integer(.x), 0L)))

panel <- universe |>
  left_join(read_intermediate("vdem_index"),         by = by_keys) |>
  left_join(read_intermediate("wgi_index"),          by = by_keys) |>
  left_join(read_intermediate("polity"),             by = by_keys) |>
  left_join(read_intermediate("regime_transitions"), by = by_keys) |>
  left_join(read_intermediate("gwf"),                by = by_keys) |>
  left_join(read_intermediate("fh_dd"),              by = by_keys) |>
  left_join(read_intermediate("reign"),              by = by_keys) |>
  left_join(read_intermediate("navco"),              by = by_keys) |>
  left_join(read_intermediate("ucdp_war"),           by = by_keys) |>
  left_join(read_intermediate("ucdp_chronic"),       by = by_keys) |>
  left_join(read_intermediate("ucdp_termination"),   by = by_keys) |>
  left_join(read_intermediate("ucdp_troops"),        by = by_keys) |>
  left_join(read_intermediate("un_pko"),             by = by_keys) |>
  left_join(read_intermediate("wdi"),                by = by_keys) |>
  left_join(read_intermediate("state_capacity"),     by = by_keys) |>
  left_join(read_intermediate("ccp"),                by = by_keys)

ucdp_zero <- c("WAR", "WAR_3PLUS", "WAR_5PLUS", "WAR_RECENT3", "WAR_RECENT5",
               "WAR_SHORT", "WAR_CHRONIC", "CONFLICT_ACTIVE", "CONFLICT_INTENSITY_YEAR",
               "CONFLICT_EPISODE_END_RECENT3", "POST_CONFLICT_5",
               grep("^UCDP_.*_RECENT3$", codebook$variable, value = TRUE),
               "N_TOTAL_TROOPS")
panel <- zero_fill(panel, ucdp_zero)

missing_cols <- setdiff(codebook$variable, names(panel))
if (length(missing_cols)) stop("Codebook variables not produced: ", paste(missing_cols, collapse = ", "))

standardized_data_final <- panel |>
  select(all_of(codebook$variable)) |>
  arrange(ISO_CODE_3, YEAR)

assert_unique_keys(standardized_data_final, label = "standardized_data_final")
stopifnot(nrow(standardized_data_final) == nrow(universe))

saveRDS(standardized_data_final, file.path(FINAL_DIR, "standardized_data_final.rds"))
write_csv(standardized_data_final, file.path(FINAL_DIR, "standardized_data_final.csv"), na = "")
write_csv(codebook, file.path(FINAL_DIR, "codebook.csv"))

message(sprintf("standardized_data_final: %d country-years, %d countries, %d variables",
                nrow(standardized_data_final), n_distinct(standardized_data_final$ISO_CODE_3),
                ncol(standardized_data_final)))
