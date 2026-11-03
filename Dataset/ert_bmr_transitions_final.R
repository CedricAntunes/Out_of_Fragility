# AUTHOR: Cedric Antunes (FGV) -------------------------------------------------
# DATE: March, 2026 ------------------------------------------------------------

# Cleaning my enrvironment 
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
library(dplyr)
library(vdemdata)

# If needed, install the current V-Dem package once with:
# install.packages("remotes")
# remotes::install_github("vdeminstitute/vdemdata")

# Analysis settings ------------------------------------------------------------
ANALYSIS_YEARS <- 1971L:2022L
RECENT_WINDOW <- 5L
SOURCE_YEARS <- (min(ANALYSIS_YEARS) - RECENT_WINDOW + 1L):max(ANALYSIS_YEARS)

# Calendar-aware rolling indicator. A value is returned only when all five
# country-years are observed; gaps or unavailable source coverage remain NA.
recent_any <- function(event, year, window = RECENT_WINDOW) {
  vapply(
    seq_along(year),
    function(i) {
      required_years <- seq.int(year[i] - window + 1L, year[i])
      values <- event[match(required_years, year)]
      
      if (anyNA(values)) {
        return(NA_integer_)
      }
      
      as.integer(any(values == 1L))
    },
    integer(1)
  )
}

# Loading data -----------------------------------------------------------------
load("D:/Users/cedric/Downloads/episodes.rda")

# Cleaning & preparing data ----------------------------------------------------
ert <- episodes |>
  select(country_text_id,
         year, 
         reg_trans,
         aut_ep) |>
  filter(year %in% SOURCE_YEARS) |>
  rename(ISO_CODE_3 = country_text_id,
         YEAR = year, 
         ERT_TRANSITION = reg_trans,
         AUTOCRATIZATION_EPISODE = aut_ep) |>
  mutate(ERT_ANY_TRANSITION = as.integer(ERT_TRANSITION != 0),
         ERT_DEMOCRATIZATION = if_else(ERT_TRANSITION == 1, 1, 0),
         ERT_DEM_BREAKDOWN = if_else(ERT_TRANSITION == -1, 1, 0)) 

if (anyDuplicated(ert[c("ISO_CODE_3", "YEAR")])) {
  stop("The ERT extract contains duplicate country-years.")
}

# Loading and preparing Boix-Miller-Rosato data --------------------------------
# e_democracy_trans is coded 1 for a democratic transition, -1 for a democratic
# breakdown, and 0 for no regime change. BMR_ANY_TRANSITION therefore equals one
# for either direction of regime transition. Missing BMR coverage remains NA.
bmr <- vdemdata::vdem |>
  select(country_text_id,
         year,
         e_boix_regime,
         e_democracy_trans) |>
  filter(year %in% SOURCE_YEARS) |>
  rename(ISO_CODE_3 = country_text_id,
         YEAR = year,
         BMR_DEMOCRACY = e_boix_regime,
         BMR_TRANSITION = e_democracy_trans)

# Reconstructing Serbia from the Federal Republic of Yugoslavia ----------------
# V-Dem's BMR fields are missing for Serbia in 1992-2005. BMR v4.0 codes the
# Federal Republic of Yugoslavia as autocratic in 1992-1999, democratic in
# 2000-2005, and records a democratic transition in 2000. Replacing these rows
# before computing the rolling measure preserves that transition in Serbia's
# five-year history and does not create artificial changes at either boundary.
bmr_serbia_fry <- tibble(
  ISO_CODE_3 = rep("SRB", length(1992L:2005L)),
  YEAR = 1992L:2005L,
  BMR_DEMOCRACY = c(rep(0, 8L), rep(1, 6L)),
  BMR_TRANSITION = as.numeric((1992L:2005L) == 2000L)
)

bmr <- bmr |>
  filter(!(ISO_CODE_3 == "SRB" & YEAR %in% 1992L:2005L)) |>
  bind_rows(bmr_serbia_fry) |>
  mutate(BMR_ANY_TRANSITION = as.integer(BMR_TRANSITION != 0)) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(
    BMR_ANY_TRANSITION_RECENT5 = recent_any(BMR_ANY_TRANSITION, YEAR)
  ) |>
  ungroup() |>
  filter(YEAR %in% ANALYSIS_YEARS)

if (anyDuplicated(bmr[c("ISO_CODE_3", "YEAR")])) {
  stop("The BMR extract contains duplicate country-years.")
}

bmr_serbia_check <- bmr |>
  filter(ISO_CODE_3 == "SRB", YEAR %in% 1992L:2005L)

stopifnot(
  nrow(bmr_serbia_check) == 14L,
  !anyNA(bmr_serbia_check$BMR_DEMOCRACY),
  !anyNA(bmr_serbia_check$BMR_TRANSITION),
  all(bmr_serbia_check$BMR_DEMOCRACY[bmr_serbia_check$YEAR <= 1999L] == 0),
  all(bmr_serbia_check$BMR_DEMOCRACY[bmr_serbia_check$YEAR >= 2000L] == 1),
  sum(bmr_serbia_check$BMR_ANY_TRANSITION) == 1L,
  bmr_serbia_check$BMR_ANY_TRANSITION[bmr_serbia_check$YEAR == 2000L] == 1L
)

# Setting autocratization episodes ---------------------------------------------
# In source year s, ERT_ANY_TRANSITION_RECENT5 covers s-4 through s. Because the
# hazard pipeline later applies a one-year lag, its value in model year t covers
# exactly t-5 through t-1, excluding the contemporaneous transition in year t.
ert <- ert |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(
    IN_AUTOCRATIZATION = AUTOCRATIZATION_EPISODE != 0 & !is.na(AUTOCRATIZATION_EPISODE),
    ERT_AUTOCRATIZATION = as.integer(IN_AUTOCRATIZATION & c(TRUE, !head(IN_AUTOCRATIZATION, -1))),
    ERT_ANY_TRANSITION_RECENT5 = recent_any(ERT_ANY_TRANSITION, YEAR)
  ) |>
  ungroup() |>
  select(-IN_AUTOCRATIZATION) |>
  filter(YEAR %in% ANALYSIS_YEARS) |>
  left_join(bmr, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(YEAR = as.character(YEAR))

# Basic checks -----------------------------------------------------------------
stopifnot(
  all(na.omit(ert$ERT_ANY_TRANSITION) %in% c(0L, 1L)),
  all(na.omit(ert$BMR_ANY_TRANSITION) %in% c(0L, 1L)),
  all(na.omit(ert$ERT_ANY_TRANSITION_RECENT5) %in% c(0L, 1L)),
  all(na.omit(ert$BMR_ANY_TRANSITION_RECENT5) %in% c(0L, 1L))
)

# Saving the data --------------------------------------------------------------
save(ert,
     file = "ert_transitions.RDS")