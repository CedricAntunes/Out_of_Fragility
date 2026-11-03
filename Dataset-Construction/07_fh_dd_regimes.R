# ==============================================================================
# 07_fh_dd.R: Freedom House and Democracy-Dictatorship indicators --------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: data-raw/fh_dd/fh_dd_final.RDS
#   * Freedom House, Country and Territory Ratings and Statuses, FIW 1973-2025
#   * Cheibub, Gandhi and Vreeland, DD revisited (ddrevisited_data_v1.xls)
# The two raw files are not in data-raw. TODO: add them and port
# archive/legacy/Dataset/FH_cleaning.R so this step starts from raw data.
#
# Coding notes:
# Coding (from FH_cleaning.R):
#   FH_DEMOCRACY  1 if the Freedom House status is Free OR Partly Free (based 
#   on Acemoglu et al. 2019)
#   DD_DEMOCRACY  DD "democracy" (ends 2008).
#
# Output: data/intermediate/fh_dd.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

e <- new.env()
load(raw_path("fh_dd", "fh_dd_final.RDS"), envir = e)

fh_dd <- e$fh_dd_final_1 |>
  as_tibble() |>
  transmute(ISO_CODE_3, YEAR = as.integer(YEAR),
            FH_DEMOCRACY = as.integer(FH_DEMOCRACY),
            DD_DEMOCRACY = as.integer(DD_DEMOCRACY))

assert_unique_keys(fh_dd, label = "fh_dd")
assert_binary(fh_dd, c("FH_DEMOCRACY", "DD_DEMOCRACY"), label = "fh_dd")
save_intermediate(fh_dd, "fh_dd")
