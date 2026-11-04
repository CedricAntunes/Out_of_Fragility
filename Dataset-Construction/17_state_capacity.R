# ==============================================================================
# 17_state_capacity.R: Hanson and Sigman (2021) state capacity index -----------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: StateCapacityDataset_v1.dta,
# 1960-2015. Used for the discriminant-validity checks in Appendix C.
#
# Variables: STATE_CAPACITY (Capacity), STATE_CAPACITY_SD (Capacity_sd)
# Output: data/intermediate/state_capacity.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

tmp <- tempfile("sc_")
dir.create(tmp)
utils::unzip(raw_path("state_capacity", "StateCapacityDataset_v1.dta.zip"), exdir = tmp)
dta <- list.files(tmp, pattern = "\\.dta$", recursive = TRUE, full.names = TRUE)
stopifnot(length(dta) == 1L)

state_capacity <- haven::read_dta(dta) |>
  transmute(ISO_CODE_3 = as.character(iso3), YEAR = as.integer(year),
            STATE_CAPACITY = as.numeric(Capacity),
            STATE_CAPACITY_SD = as.numeric(Capacity_sd)) |>
  filter(!is.na(ISO_CODE_3), YEAR >= YEAR_START)

assert_unique_keys(state_capacity, label = "state_capacity")
save_intermediate(state_capacity, "state_capacity")
