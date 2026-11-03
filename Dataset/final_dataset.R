# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Loading packages -------------------------------------------------------------
library(here)
library(countrycode)
library(coefplot)
library(modelsummary)
library(corrplot)
library(corrtable)
library(fixest)
library(pROC)
library(texreg)
library(dynlm)
library(lfe)
library(tibble)
library(tidyverse)
library(glmnet)
library(lubridate)
library(dplyr)
library(broom)
library(plm)
library(car)
library(nnet)
library(scatr)
library(zoo)
library(kableExtra)
library(lmtest)
library(sandwich)
library(margins)
library(tidyr)
library(dplyr)
library(caret)
library(stargazer)
library(VGAM)
library(knitr)
library(xfun)
library(factoextra)
library(cluster)
library(ggplot2)
library(vip)
library(themis)
library(janitor)
library(WDI)
library(brglm2)        
library(glmmTMB)
library(margins)
library(splines)
library(stats)
library(scales)
library(isotone)
library(margins)
library(marginaleffects)
library(naniar)
#library(rlang)
library(coda)
#library(rjags)
library(vdemdata)

# Setting seed for replication -------------------------------------------------
set.seed(247)

# Load data, functions, and scripts --------------------------------------------
load("final_new_clean_output_data.RDS")
load("un_pko_country_year.RDS")
load("reign_leadership_change.RDS")
load("fh_dd_final.RDS")
load("gwf_clean.RDS")
load("wgi_revised.RDS")
load("wgi_norl_revised.RDS")
load("vdem_norl.RDS")
load("state_capacity.RDS")
load("ert_transitions.RDS")
load("ef_clean.RDS")
load("ucdp_chronic_clean.RDS")
load("navco_1_3_country_year.RDS")
load("ucdp_war_clean.RDS")
load("ccp_new_constitution_country_year.RDS")
load("ert_bmr_transitions.RDS")
load("ucdp_conflict_termination_country_year.rds")
source("WGI_30th_missing_estimates.R")
source("WGI_35th_missing_estimates.R")
source("WGI_loading_factor_estimates.R")
source("fragility_breakthrough_identifier.R")
source("VDEM_never_fragile.R")

# ------------------------------------------------------------------------------
# Sanity check in case of country-year duplicates ------------------------------
# ------------------------------------------------------------------------------
audit_country_year_keys <- function(data, stage) {

  keys <- data |>
    ungroup() |>
    transmute(
      ISO_CODE_3 = as.character(ISO_CODE_3),
      YEAR = as.character(YEAR)
    ) |>
    count(
      ISO_CODE_3,
      YEAR,
      name = "n_rows"
    )

  tibble(
    stage = stage,
    rows = nrow(data),
    distinct_country_years = nrow(keys),

    duplicate_key_groups = sum(keys$n_rows > 1L),

    excess_duplicate_rows = sum(
      pmax(keys$n_rows - 1L, 0L)
    ),

    maximum_rows_per_key = max(keys$n_rows),

    missing_country_year_keys = sum(
      is.na(keys$ISO_CODE_3) |
        is.na(keys$YEAR)
    )
  )
}


show_duplicate_keys <- function(data, stage) {

  data |>
    ungroup() |>
    transmute(
      ISO_CODE_3 = as.character(ISO_CODE_3),
      YEAR = as.character(YEAR)
    ) |>
    count(
      ISO_CODE_3,
      YEAR,
      name = "n_rows"
    ) |>
    filter(n_rows > 1L) |>
    mutate(stage = stage) |>
    arrange(desc(n_rows), ISO_CODE_3, YEAR)
}

audit_country_year_keys(
  final_clean_percentiles_data,
  "00_base_final_clean_percentiles_data"
)

show_duplicate_keys(
  final_clean_percentiles_data,
  "00_base_final_clean_percentiles_data"
) |>
  print(n = Inf)

key_audit <- tibble()

record_key_audit <- function(data, stage) {

  result <- audit_country_year_keys(
    data = data,
    stage = stage
  )

  print(result)

  result
}

key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "00_base"
  )
)

# ------------------------------------------------------------------------------
# WORLD BANK DATA --------------------------------------------------------------
# ------------------------------------------------------------------------------
# Last minute input: extracting final GDP per capita data ----------------------
gdp_per_capita_data <- WDI(
  country = "all",
  indicator = c("NY.GDP.PCAP.KD"),
  start = 1970,
  end = 2023,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

# Cleaning the data ------------------------------------------------------------
gdp_per_capita_data <- gdp_per_capita_data |>
  # Renaming variables for match
  rename(COUNTRY_NAME = country,
         ISO_CODE_3 = iso3c,
         YEAR = year,
         GDP_PER_CAPITA = NY.GDP.PCAP.KD) |>
  # Selecting only key variables for match 
  select(COUNTRY_NAME,
         ISO_CODE_3,
         YEAR,
         GDP_PER_CAPITA) |>
  # Dropping 2023
  filter(YEAR != 2023) |>
  # Setting YEAR as character
  mutate(YEAR = as.character(YEAR)) |>
  # Selecting only country-rows
  slice(-(1:2597))

# Preparing mother-dataset
final_clean_percentiles_data <- final_clean_percentiles_data |>
  mutate(YEAR = as.character(YEAR))

# Last minute input: extracting final GDP per capita data ----------------------
oil_data <- WDI(
  country = "all",
  indicator = c("NY.GDP.PETR.RT.ZS"),
  start = 1970,
  end = 2023,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

# Cleaning the data ------------------------------------------------------------
oil_data <- oil_data |>
  # Renaming variables for match
  rename(COUNTRY_NAME = country,
         ISO_CODE_3 = iso3c,
         YEAR = year,
         OIL_RENTS = NY.GDP.PETR.RT.ZS) |>
  # Selecting only key variables for match 
  select(COUNTRY_NAME,
         ISO_CODE_3,
         YEAR,
         OIL_RENTS) |>
  # Dropping 2023
  filter(YEAR != 2023) |>
  # Setting YEAR as character
  mutate(YEAR = as.character(YEAR)) |>
  # Selecting only country-rows
  slice(-(1:2646))
  
# Performing the join ----------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          gdp_per_capita_data,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = FALSE)
# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "01_after_gdp_join"
  )
)

# Performing the join ----------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          oil_data,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = FALSE)

# Duplicate audit 
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "02_after_oil_join"
  )
)

# FH & DD join -----------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          fh_dd_final_1,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Final cleanning --------------------------------------------------------------
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-COUNTRY_NAME.x.x,
         -COUNTRY_NAME.y,
         -COUNTRY_NAME.y.y,
         -YEAR.y,
         -ISO_CODE_3.y) |>
  # Dropping duplicate columns 
  rename(COUNTRY_NAME = COUNTRY_NAME.x,
         YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x) |>
  # Allocating GDP per capita along with economic indicators 
  relocate(82, .after = 3) |>
  # Log-trnaforming GDP per capita 
  mutate(LOG_GDP_PER_CAPITA = if_else(GDP_PER_CAPITA > 0, log(GDP_PER_CAPITA), NA_real_),
         LOG_ODA_RECEIVED_PER_CAPITA = if_else(ODA_RECEIVED_PER_CAPITA > 0, log(ODA_RECEIVED_PER_CAPITA), NA_real_))

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "03_after_fh_dd_join"
  )
)

# ------------------------------------------------------------------------------
# V-Dem and Boix et al. data ---------------------------------------------------
# ------------------------------------------------------------------------------
vdem_df <- vdem |>
  transmute(
    ISO_CODE_3 = toupper(country_text_id),  
    year       = as.character(year),
    v2svstterr,
    e_boix_regime
  ) |>
  filter(year >= 1970, year <= 2022) |>
  rename(YEAR = year,
         STATE_CONTROL_TERRITORY = v2svstterr,
         BOIX_DEMOCRACY = e_boix_regime)

# V-Dem and Boix et al. join----------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          vdem_df,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Final cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "04_after_vdem_boix_join"
  )
)

# ------------------------------------------------------------------------------
# GWF --------------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          gwf_final,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Final cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "05_after_gwf_join"
  )
)

# ------------------------------------------------------------------------------
# WGI Loading factors ----------------------------------------------------------
# ------------------------------------------------------------------------------
wgi_revised_clean <- wgi_revised_clean |>
  mutate(YEAR = as.character(YEAR))

final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          wgi_revised_clean,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "06_after_wgi_join"
  )
)

# ------------------------------------------------------------------------------
# Democratic transitions -------------------------------------------------------
# ------------------------------------------------------------------------------
#vdem_transitions <- vdem |>
#  transmute(
#    ISO_CODE_3 = toupper(country_text_id),  
#    year       = as.character(year),
#    e_democracy_trans
#  ) |>
#  filter(year >= 1970, year <= 2020) |>
#  rename(YEAR = year,
#         DEMOCRACY_TRANSITION = e_democracy_trans)

#final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
#                                          vdem_transitions,
#                                          by = c("ISO_CODE_3", 
#                                                 "YEAR"),
#                                          keep = TRUE)

# Final cleaning
#final_clean_percentiles_data <- final_clean_percentiles_data |>
#  select(-YEAR.y,
#         -ISO_CODE_3.y) |>
#  rename(YEAR = YEAR.x,
#         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
#key_audit <- bind_rows(
#  key_audit,
#  record_key_audit(
#    final_clean_percentiles_data,
#    "07_after_democracy_transition_join"
#  )
#)

# ------------------------------------------------------------------------------
# V-Dem No Rule of Law ---------------------------------------------------------
# ------------------------------------------------------------------------------
vdem_norl <- vdem_norl |>
  mutate(YEAR = as.character(YEAR))

final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          vdem_norl,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "08_after_vdem_norl_join"
  )
)

# ------------------------------------------------------------------------------
# WGI No Rule of Law ---------------------------------------------------------
# ------------------------------------------------------------------------------
wgi_norl <- wgi_revised_norl_clean |>
  mutate(YEAR = as.character(YEAR))

final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          wgi_norl,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x,
         COUNTRY_NAME = COUNTRY_NAME.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "09_after_wgi_norl_join"
  )
)

# ------------------------------------------------------------------------------
# ERT --------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
#                                          ert,
#                                          by = c("ISO_CODE_3", 
#                                                 "YEAR"),
#                                          keep = TRUE)

# Cleaning
#final_clean_percentiles_data <- final_clean_percentiles_data |>
#  select(-YEAR.y,
#         -ISO_CODE_3.y) |>
#  rename(YEAR = YEAR.x,
#         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate join
#key_audit <- bind_rows(
#  key_audit,
#  record_key_audit(
#    final_clean_percentiles_data,
#    "10_after_ert_join"
#  )
#)

# ------------------------------------------------------------------------------
# State capacity ---------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          sc,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "11_after_state_capacity_join"
  )
)

# ------------------------------------------------------------------------------
# Ethnic fractionalization -----------------------------------------------------
# ------------------------------------------------------------------------------
#final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
#                                          ef,
#                                          by = c("ISO_CODE_3", 
#                                                 "YEAR"),
#                                          keep = TRUE)

# Cleaning
#final_clean_percentiles_data <- final_clean_percentiles_data |>
#  select(-YEAR.y,
#         -ISO_CODE_3.y) |>
#  rename(YEAR = YEAR.x,
#         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
#key_audit <- bind_rows(
#  key_audit,
#  record_key_audit(
#    final_clean_percentiles_data,
#    "12_after_ethnic_fractionalization_join"
#  )
#)

# ------------------------------------------------------------------------------
# REIGN leadership data --------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          reign_for_merge,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "13_after_reign_join"
  )
)

# ------------------------------------------------------------------------------
# UN PKO data ------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(
  final_clean_percentiles_data,
  un_pko_country_year,
  by = c("ISO_CODE_3", "YEAR"),
  keep = TRUE
)

# Cleaning join artifacts
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(
    -YEAR.y,
    -ISO_CODE_3.y
  ) |>
  rename(
    YEAR = YEAR.x,
    ISO_CODE_3 = ISO_CODE_3.x
  )

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "14_after_un_pko_join"
  )
)

# Now replace PKO missingness with zero
# Missing after this merge means no mapped UN PKO exposure in that country-year.
pko_vars <- c(
  "UN_PKO_PRESENT_ANY",
  "UN_PKO_PRESENT_NONLEGACY",
  "UN_PKO_ONSET_YEAR",
  "UN_PKO_ONSET_RECENT3",
  "UN_PKO_ONSET_RECENT5",
  "UN_PKO_ONSET_NONLEGACY_RECENT3",
  "UN_PKO_ONSET_NONLEGACY_RECENT5",
  "N_UN_PKO_MISSIONS",
  "N_UN_PKO_MISSIONS_NONLEGACY",
  "N_UN_PKO_ONSETS_YEAR",
  "N_UN_PKO_ONSETS_NONLEGACY_YEAR"
)

pko_vars <- intersect(pko_vars, names(final_clean_percentiles_data))

final_clean_percentiles_data <- final_clean_percentiles_data |>
  mutate(
    across(
      all_of(pko_vars),
      ~ replace_na(.x, 0)
    )
  )

# Optional: force binary PKO variables to integer 0/1
pko_binary_vars <- c(
  "UN_PKO_PRESENT_ANY",
  "UN_PKO_PRESENT_NONLEGACY",
  "UN_PKO_ONSET_YEAR",
  "UN_PKO_ONSET_RECENT3",
  "UN_PKO_ONSET_RECENT5",
  "UN_PKO_ONSET_NONLEGACY_RECENT3",
  "UN_PKO_ONSET_NONLEGACY_RECENT5"
)

pko_binary_vars <- intersect(pko_binary_vars, names(final_clean_percentiles_data))

final_clean_percentiles_data <- final_clean_percentiles_data |>
  mutate(
    across(
      all_of(pko_binary_vars),
      ~ as.integer(.x)
    )
  )

# ------------------------------------------------------------------------------
# NAVCO ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          navco_country_year,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "15_after_navco_join"
  )
)

# ------------------------------------------------------------------------------
# CCP --------------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          ccp_new_constitution_country_year,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "16_after_ccp_join"
  )
)

# ------------------------------------------------------------------------------
# UCDP -------------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          ucdp_conflict_termination_country_year,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "17_after_ucdp_join"
  )
)

# ------------------------------------------------------------------------------
# UCDP: WAR ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          war_panel,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "18_after_ucdp_war_join"
  )
)

# ------------------------------------------------------------------------------
# ERT-BMR Transitions ----------------------------------------------------------
# ------------------------------------------------------------------------------
final_clean_percentiles_data <- left_join(final_clean_percentiles_data,
                                          transitions,
                                          by = c("ISO_CODE_3", 
                                                 "YEAR"),
                                          keep = TRUE)

# Cleaning
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-YEAR.y,
         -ISO_CODE_3.y) |>
  rename(YEAR = YEAR.x,
         ISO_CODE_3 = ISO_CODE_3.x)

# Duplicate audit
key_audit <- bind_rows(
  key_audit,
  record_key_audit(
    final_clean_percentiles_data,
    "19_after_ert_bmr_join"
  )
)

# ------------------------------------------------------------------------------
# UCDP: Final Revision ---------------------------------------------------------
# ------------------------------------------------------------------------------
# Dropping deprecated UCPD variables 
final_clean_percentiles_data <- final_clean_percentiles_data |>
  select(-MAX_CONFLICT_INTENSITY,
         -MIN_CONFLICT_INTENSITY,
         -AVG_CONFLICT_INTENSITY,
         -CONFLICT_INTENSITY_YEAR)

# Helper -----------------------------------------------------------------------
merge_ucdp_conflict_variables <- function(
    master_data,
    conflict_data = conflict_panel,
    master_iso = "ISO_CODE_3",
    master_year = "YEAR") {
  
  # 1. Check required keys
  required_master_keys <- c(master_iso, master_year)
  
  if (!all(required_master_keys %in% names(master_data))) {
    stop(
      "Master data must contain: ",
      paste(required_master_keys, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!all(c("ISO_CODE_3", "YEAR") %in% names(conflict_data))) {
    stop(
      "Conflict data must contain ISO_CODE_3 and YEAR.",
      call. = FALSE
    )
  }
  
  # 2. Create working copies and harmonize join-key types
  master_copy <- master_data
  master_copy[[master_iso]] <- as.character(
    master_copy[[master_iso]]
  )
  master_copy[[master_year]] <- as.integer(
    master_copy[[master_year]]
  )
  
  conflict_copy <- conflict_data |>
    dplyr::select(-dplyr::any_of("UCDP_LOCATION_NAMES"))
  
  conflict_copy[["ISO_CODE_3"]] <- as.character(
    conflict_copy[["ISO_CODE_3"]]
  )
  conflict_copy[["YEAR"]] <- as.integer(
    conflict_copy[["YEAR"]]
  )
  
  if (
    anyNA(master_copy[[master_year]]) ||
    anyNA(conflict_copy[["YEAR"]])
  ) {
    stop(
      "YEAR contains missing or non-numeric values after conversion.",
      call. = FALSE
    )
  }
  
  # 3. Confirm that conflict data are unique by country-year
  if (anyDuplicated(
    conflict_copy[c("ISO_CODE_3", "YEAR")]
  ) > 0L) {
    stop(
      "Conflict data are not unique by ISO_CODE_3 and YEAR.",
      call. = FALSE
    )
  }
  
  # 4. Identify the revised conflict variables
  conflict_variables <- setdiff(
    names(conflict_copy),
    c("ISO_CODE_3", "YEAR")
  )
  
  # 5. Remove their older versions from the master dataset.
  # Also remove AVG_CONFLICT_INTENSITY, which is now retired.
  master_copy <- master_copy |>
    dplyr::select(
      -dplyr::any_of(
        c(conflict_variables, "AVG_CONFLICT_INTENSITY")
      )
    )
  
  # 6. Define the corresponding join keys
  join_keys <- stats::setNames(
    c("ISO_CODE_3", "YEAR"),
    c(master_iso, master_year)
  )
  
  # 7. Merge the revised conflict variables
  merged <- dplyr::left_join(
    master_copy,
    conflict_copy,
    by = join_keys,
    relationship = "many-to-one"
  )
  
  # 8. Countries absent from UCDP receive zeros
  merged <- merged |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(conflict_variables),
        ~ tidyr::replace_na(.x, 0)
      )
    )
  
  merged
}

# Sanity check before join
n_before_conflict_merge <- nrow(final_clean_percentiles_data)

# Join
final_clean_percentiles_data <- merge_ucdp_conflict_variables(
  master_data = final_clean_percentiles_data,
  conflict_data = conflict_panel,
  master_iso = "ISO_CODE_3",
  master_year = "YEAR"
)

# Building working dataframe ---------------------------------------------------
final_clean_percentiles_data_normalized_1 <- final_clean_percentiles_data |>
  select(-REVISED_COMBINED_POLITY_SCORE,
         -PRIOR_COMBINED_POLITY_SCORE,
         -POLITY_END_MONTH, 
         -POLITY_END_DAY,
         -POLITY_END_YEAR,
         -END_DATE_PRECISION,
         -INTERIM_POLITY_SCORE,
         -POLITY_BEGIN_YEAR,
         -POLITY_BEGIN_MONTH,
         -POLITY_BEGIN_DAY,
         -POLITYV_CONFIDENCE_INDEX,
         -BEGIN_DATE_PRECISION,
         -POST_POLITY_SCORE,
         -TOTAL_CHANGE_POLITY_SCORE,
         -REGIME_TRANSITION_COMPLETED,
         -REGIME_TRANSITION,
         -CONFLICT_ID,
         -CONFLICT_LOCATION,
         -CONFLICT_CAUSE,
         -CONFLICT_TYPE,
         -CONFLICT_INACTIVE,
         -COUNTRY_CONFLICT_SIDE_A,
         -STATES_SUPPORT_SIDE_A_WITH_TROOPS,
         -COUNTRY_OR_ACTOR_CONFLICT_SIDE_B,
         -STATES_SUPPORT_SIDE_B_WITH_TROOPS,
         -TERRITORY_UNDER_DISPUTE,
         -CONFLICT_START_DATE,
         -PRECISION_OF_CONFLICT_START_DATE,
         -PRECISION_OF_CONFLICT_END_DATE,
         -CONFLICT_DEATHS_THRESHOLD_DATE,
         -PRECISION_OF_CONFLICT_DEATHS_THRESHOLD_DATE,
         -CONFLICT_END_DATE,
         -PRECISION_OF_CONFLICT_START_DATE,
         -country,
         -regime_type,
         -NAVCO_ANY_GOVACTOR_TEXT_ACTIVE,
         -NAVCO_ANY_GOVACTOR_TEXT_ONSET,
         -NAVCO_SOURCE_COVERAGE
  ) |>
  rename(COUNTRY_NAME = COUNTRY_NAME.x) |>
  estimating_WGI_30th_missing_values() |>
  estimating_WGI_35th_missing_values() |>
  estimating_WGI_loading_factor_missing_values() |>
  filter(complete.cases(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y, NEW_VDEM_30TH_PERCENTILE)) |>
  group_by(COUNTRY_NAME) |>
  mutate(
    YEAR = as.numeric(YEAR),

    # Fragility DUMMIES (1 = Fragile, 0 = Not Fragile)
    VDEM_FRAGILE_BASELINE     = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE, 1, 0),
    VDEM_NON_FRAGILE_BASELINE = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y >= NEW_VDEM_25TH_PERCENTILE, 1, 0),

    VDEM_FRAGILE_IDEAL        = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_30TH_PERCENTILE, 1, 0),
    VDEM_NORL_FRAGILE_IDEAL   = if_else(NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED < NEW_VDEM_NORL_30TH_PERCENTILE, 1, 0),
    WGI_FRAGILE_IDEAL         = if_else(WGI_LOADING_FACTOR_1_NORMALIZED < WGI_30TH_PERCENTILE, 1, 0),
    WGI_NORL_FRAGILE_IDEAL    = if_else(WGI_NORL_LOADING_FACTOR_1_NORMALIZED < WGI_NORL_30TH_PERCENTILE, 1, 0),
    VDEM_NON_FRAGILE_IDEAL    = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y >= NEW_VDEM_35TH_PERCENTILE, 1, 0),

    VDEM_FRAGILE_ENDLINE      = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE, 1, 0),
    VDEM_NON_FRAGILE_ENDLINE  = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y >= NEW_VDEM_40TH_PERCENTILE, 1, 0),

    # Fragility LABELS
    # V-Dem Baseline
    VDEM_STATUS_BASELINE = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_25TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # WGI Baseline
    WGI_STATUS_BASELINE = case_when(
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_20TH_PERCENTILE ~ "Fragile",
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_25TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # V-Dem Ideal
    VDEM_STATUS_IDEAL = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_30TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_35TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # WGI Ideal
    WGI_STATUS_IDEAL = case_when(
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_30TH_PERCENTILE ~ "Fragile",
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_30TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # V-Dem Endline
    VDEM_STATUS_ENDLINE = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_40TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # WGI Endline
    WGI_STATUS_ENDLINE = case_when(
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_20TH_PERCENTILE ~ "Fragile",
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_40TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    # V-Dem Ideal NORL
    VDEM_STATUS_NORL_IDEAL = case_when(
      NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED < NEW_VDEM_NORL_30TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED < NEW_VDEM_NORL_35TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    WGI_STATUS_NORL_IDEAL = case_when(
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_NORL_30TH_PERCENTILE ~ "Fragile",
      WGI_LOADING_FACTOR_1_NORMALIZED < WGI_NORL_35TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    )
  ) |>
  rename(INSTITUTIONAL_DEMOCRACY_SCORE = INSTITUTIONAL_DEMOCRACY_SOCRE)

# Converting to character before standardizing ---------------------------------
final_data <- final_clean_percentiles_data_normalized_1 |>
  mutate(
    YEAR = as.character(YEAR),
    #across(35:55, as.character)
    across(where(is.factor), as.character)  
  )

# Computing means and SDs for numeric columns ----------------------------------
means_data <- sapply(final_data, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA)
sd_data    <- sapply(final_data, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA)

# Initializing standardized data
standardized_data <- final_data

# Defining dummies to exclude from standardization
dummy_vars <- c(
  "DEMOCRACY_TRANSITION",
  "VDEM_FRAGILE_BASELINE", 
  "VDEM_NON_FRAGILE_BASELINE",
  "VDEM_FRAGILE_IDEAL", 
  "VDEM_NON_FRAGILE_IDEAL",
  "VDEM_FRAGILE_ENDLINE", 
  "VDEM_NON_FRAGILE_ENDLINE",
  "VDEM_NORL_FRAGILE_IDEAL",
  "WGI_FRAGILE_IDEAL",
  "WGI_NORL_FRAGILE_IDEAL",
  "PARTIAL_DEMOCRACY_WITH_FACTIONALISM",
  "FH_DEMOCRACY",
  "FH_AUTOCRACY",
  "DD_DEMOCRACY",
  "BOIX_DEMOCRACY",
  "GWF_DEMOCRACY",
  "GWF_AUTOCRACY",
  "GWF_AUTOCRACY_PARTY",
  "GWF_MILITARY",
  "GWF_PERSONALIST",
  "STATE_CAPACITY_SD",
  "REIGN_LEADER_CHANGE_YEAR",
  "REIGN_LEADER_CHANGE_RECENT3",
  "REIGN_LEADER_CHANGE_RECENT5",
  "REIGN_LEADER_MILITARYCAREER",
  "UN_PKO_PRESENT_ANY",
  "UN_PKO_PRESENT_NONLEGACY",
  "UN_PKO_ONSET_YEAR",
  "UN_PKO_ONSET_RECENT3",
  "UN_PKO_ONSET_RECENT5",
  "UN_PKO_ONSET_NONLEGACY_RECENT3",
  "NAVCO_N_MASS_ACTIVE",
  "NAVCO_ANY_MASS_ACTIVE",
  "NAVCO_N_VIOLENT_ACTIVE",
  "NAVCO_ANY_VIOLENT_ACTIVE",
  "NAVCO_N_NONVIOLENT_ACTIVE",
  "NAVCO_ANY_NONVIOLENT_ACTIVE",
  "NAVCO_N_INCUMBENT_ACTIVE",
  "NAVCO_ANY_INCUMBENT_VIOLENT_ACTIVE",
  "NAVCO_N_INCUMBENT_NONVIOLENT_ACTIVE",
  "NAVCO_ANY_INCUMBENT_NONVIOLENT_ACTIVE",
  "NAVCO_N_MASS_ONSET",
  "NAVCO_ANY_MASS_ONSET",
  "NAVCO_N_VIOLENT_ONSET",
  "NAVCO_ANY_VIOLENT_ONSET",
  "NAVCO_N_NONVIOLENT_ONSET",
  "NAVCO_ANY_NONVIOLENT_ONSET",
  "NAVCO_N_INCUMBENT_ONSET",
  "NAVCO_ANY_INCUMBENT_ONSET",
  "NAVCO_N_INCUMBENT_VIOLENT_ONSET",
  "NAVCO_ANY_INCUMBENT_VIOLENT_ONSET",
  "NAVCO_N_INCUMBENT_NONVIOLENT_ONSET",
  "NAVCO_ANY_INCUMBENT_NONVIOLENT_ONSET",
  "NAVCO_INCUMBENT_NONVIOLENT_ONSET_L1",
  "NAVCO_INCUMBENT_VIOLENT_ONSET_L1",
  "NAVCO_ANY_MASS_ONSET_L1",
  "NAVCO_ANY_MASS_ACTIVE_L1",
  "NAVCO_INCUMBENT_NONVIOLENT_ACTIVE_L1",
  "NAVCO_INCUMBENT_VIOLENT_ACTIVE_L1",
  "NEW_CONSTITUTION",
  "UCDP_INTRASTATE_CONFLICT_ACTIVE",
  "UCDP_ANY_TERMINATION",
  "UCDP_SUBSTANTIVE_TERMINATION",
  "UCDP_NEGOTIATED_TERMINATION",
  "UCDP_PEACE_AGREEMENT_TERMINATION",
  "UCDP_CEASEFIRE_TERMINATION",
  "UCDP_VICTORY_TERMINATION",
  "UCDP_GOVERNMENT_VICTORY_TERMINATION",
  "UCDP_NONSTATE_VICTORY_TERMINATION",
  "UCDP_LOW_ACTIVITY_TERMINATION",
  "UCDP_ACTOR_CEASES_TERMINATION",
  "UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR",
  "UCDP_COUNTRY_CONFLICT_CESSATION",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION",
  "UCDP_ANY_TERMINATION_RECENT3",
  "UCDP_SUBSTANTIVE_TERMINATION_RECENT3",
  "UCDP_CEASEFIRE_TERMINATION_RECENT3",
  "UCDP_VICTORY_TERMINATION_RECENT3",
  "UCDP_COUNTRY_CONFLICT_CESSATION_RECENT3",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3",
  "WAR_SHORT",
  "WAR_CHRONIC",
  "CONFLICT_ACTIVE",
  "CONFLICT_EPISODE_END",
  "CHRONIC_SAME_CONFLICT_4PLUS",
  "CHRONIC_COUNTRY_CONFLICT_4PLUS",
  "CHRONIC_CONFLICT_4PLUS",
  "CONFLICT_EPISODE_END_RECENT3",
  "CONFLICT_EPISODE_END_RECENT5",
  "CONFLICT_EPISODE_END_RECENT7",
  "POST_CONFLICT_3",
  "POST_CONFLICT_5",
  "POST_CONFLICT_7", 
  "WAR",
  "WAR_3PLUS",
  "WAR_5PLUS",
  "WAR_RECENT3",
  "WAR_RECENT5",
  "ERT_DEMOCRATIZATION",
  "ERT_DEM_BREAKDOWN",
  "ERT_ANY_TRANSITION",
  "ERT_AUT_LIBERALIZATION",
  "ERT_AUT_EPISODE_ONSET",
  "ERT_DEMOCRATIZATION_RECENT5",
  "ERT_DEM_BREAKDOWN_RECENT5",
  "ERT_ANY_TRANSITION_RECENT5",
  "ERT_AUT_LIBERALIZATION_RECENT5",
  "BMR_DEMOCRACY",
  "BMR_DEMOCRATIZATION",
  "BMR_BREAKDOWN",
  "BMR_ANY_TRANSITION",
  "BMR_DEMOCRATIZATION_RECENT5",
  "BMR_BREAKDOWN_RECENT5",
  "BMR_ANY_TRANSITION_RECENT5"
)

# Applying standardization safely
for (col in names(final_data)) {
  if (is.numeric(final_data[[col]]) && !(col %in% dummy_vars)) {
    standardized_data[[col]] <- (final_data[[col]] - means_data[[col]]) / sd_data[[col]]
  }
}

# Converting relevant predictors back to numeric
standardized_data_final <- standardized_data |>
  mutate(across(35:55, as.numeric))

# Final correction pass: recalculating fragility dummies from scratch
standardized_data_final <- standardized_data_final |>
  mutate(
    VDEM_FRAGILE_BASELINE = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE, 1, 0),
    VDEM_FRAGILE_IDEAL    = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_30TH_PERCENTILE, 1, 0),
    VDEM_FRAGILE_ENDLINE  = if_else(NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE, 1, 0),

    VDEM_STATUS_BASELINE = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_25TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    VDEM_STATUS_IDEAL = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_30TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_35TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    ),
    VDEM_STATUS_ENDLINE = case_when(
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_20TH_PERCENTILE ~ "Fragile",
      NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y < NEW_VDEM_40TH_PERCENTILE ~ "Transitioning",
      TRUE ~ "Non Fragile"
    )
  )

# Saving data 
saveRDS(standardized_data_final, 
        file = "standardized_data_final.RDS")

write.csv(standardized_data_final,
          "standardized_data_final.csv")

# Setting the critical junctures
standardized_data_final <- standardized_data_final |>
  mutate(CRITICAL_JUNCTURE = case_when(
    (COUNTRY_NAME == "Angola" & YEAR == 2018) ~ 1,
    (COUNTRY_NAME == "Brazil" & YEAR == 1985) ~ 1,
    (COUNTRY_NAME == "Ethiopia" & YEAR == 1991) ~ 1,
    (COUNTRY_NAME == "Gabon" & YEAR == 1990) ~ 1,
    (COUNTRY_NAME == "Gambia, The" & YEAR == 2016) ~ 1,
    (COUNTRY_NAME == "Ghana" & YEAR == 1992) ~ 1,
    (COUNTRY_NAME == "Kenya" & YEAR == 2002) ~ 1,
    (COUNTRY_NAME == "Mauritania" & YEAR == 1992) ~ 1,
    (COUNTRY_NAME == "Sierra Leone" & YEAR == 2002) ~ 1,
    (COUNTRY_NAME == "Tunisia" & YEAR == 2010) ~ 1,
    (COUNTRY_NAME == "Uganda" & YEAR == 1986) ~ 1,
    (COUNTRY_NAME == "Madagascar" & YEAR == 1991) ~ 1,
    (COUNTRY_NAME == "Myanmar" & YEAR == 2008) ~ 1,
    (COUNTRY_NAME == "Indonesia" & YEAR == 1998) ~ 1,
    (COUNTRY_NAME == "Iran, Islamic Rep." & YEAR == 1988) ~ 1,
    (COUNTRY_NAME == "Nepal" & YEAR == 2006) ~ 1,
    (COUNTRY_NAME == "Philippines" & YEAR == 1986) ~ 1,
    (COUNTRY_NAME == "Sri Lanka" & YEAR == 2009) ~ 1,
    (COUNTRY_NAME == "China" & YEAR == 1977) ~ 1,
    (COUNTRY_NAME == "Thailand" & YEAR == 1991) ~ 1,
    (COUNTRY_NAME == "Bosnia and Herzegovina" & YEAR == 1995) ~ 1,
    (COUNTRY_NAME == "Serbia" & YEAR == 1999) ~ 1,
    (COUNTRY_NAME == "Georgia" & YEAR == 2004) ~ 1,
    (COUNTRY_NAME == "Kyrgyz Republic" & YEAR == 2010) ~ 1,
    (COUNTRY_NAME == "Armenia" & YEAR == 2018) ~ 1,
    (COUNTRY_NAME == "Argentina" & YEAR == 1983) ~ 1,
    (COUNTRY_NAME == "Bolivia" & YEAR == 1982) ~ 1,
    (COUNTRY_NAME == "Colombia" & YEAR == 1990) ~ 1,
    (COUNTRY_NAME == "Ecuador" & YEAR == 1976) ~ 1,
    (COUNTRY_NAME == "Dominican Republic" & YEAR == 1994) ~ 1,
    (COUNTRY_NAME == "El Salvador" & YEAR == 1990) ~ 1,
    (COUNTRY_NAME == "Mexico" & YEAR == 1990) ~ 1,
    (COUNTRY_NAME == "Nicaragua" & YEAR == 1979) ~ 1,
    (COUNTRY_NAME == "Panama" & YEAR == 1989) ~ 1,
    (COUNTRY_NAME == "Paraguay" & YEAR == 1989) ~ 1,
    (COUNTRY_NAME == "Peru" & YEAR == 2000) ~ 1,
    TRUE ~ 0
  )) |>
  mutate(CRITICAL_JUNCTURE = as.numeric(CRITICAL_JUNCTURE))

# Checking classification ------------------------------------------------------
sample <- standardized_data_final |>
  select(YEAR,
         COUNTRY_NAME,
         VDEM_STATUS_BASELINE,
         VDEM_STATUS_IDEAL,
         VDEM_STATUS_ENDLINE) |>
  rename(VDEM_STATUS_20_25_PERCENTILES = VDEM_STATUS_BASELINE,
         VDEM_STATUS_30_35_PERCENTILES = VDEM_STATUS_IDEAL,
         VDEM_STATUS_20_40_PERCENTILES = VDEM_STATUS_ENDLINE)

# Saving classified country-year observations ----------------------------------
write.csv(sample,
          "countries_classified.csv")
