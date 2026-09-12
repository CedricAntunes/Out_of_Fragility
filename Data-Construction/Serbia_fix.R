# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: August 2026 ------------------------------------------------------------
#
# Purpose: fixing final analysis sample by reconstructing unobseverd political
# variables for Serbia based on the Federal Republic of Yugoslavia 

# Packages ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(readxl)

# Required input data ----------------------------------------------------------
BMR_PATH    <- "democracy-v4.0.csv" # Boix-Rosatto-Miller
POLITY_PATH <- "p5v2018.xls"        # Polity-V

# Unobserved years for Serbia (since the dissolution of the Yugoslavia)
SERBIA_FILL_YEARS <- 1992:2005

# ------------------------------------------------------------------------------
# Source data ------------------------------------------------------------------
# ------------------------------------------------------------------------------
bmr_raw <- read_csv(
  BMR_PATH,
  show_col_types = FALSE
)

polity_raw <- readxl::read_excel(
  POLITY_PATH,
  sheet = "p5v2018"
)

# In both supplied files:
#   ccode 342 = Serbia
#   ccode 347 = Federal Republic of Yugoslavia / Serbia and Montenegro

# ------------------------------------------------------------------------------
# Extracting the BMR democracy values ------------------------------------------
# ------------------------------------------------------------------------------
bmr_fry <- bmr_raw |>
  filter(
    ccode == 347,
    year %in% SERBIA_FILL_YEARS
  ) |>
  transmute(
    YEAR_JOIN = as.integer(year),
    BOIX_PATCH = as.numeric(democracy)
  )

# ------------------------------------------------------------------------------
# Putting raw Polity durability on the scale already used in the master data ---
# ------------------------------------------------------------------------------
# Using Serbia's observed 2006-2018 values to recover the transformation between
# raw Polity DURABLE and REGIME_DURABILITY_YEARS in standardized_data_final.
durability_bridge <- standardized_data_final |>
  transmute(
    COUNTRY_NAME,
    YEAR_JOIN = as.integer(YEAR),
    REGIME_DURABILITY_YEARS
  ) |>
  filter(
    COUNTRY_NAME == "Serbia",
    YEAR_JOIN %in% 2006:2018,
    !is.na(REGIME_DURABILITY_YEARS)
  ) |>
  inner_join(
    polity_raw |>
      filter(
        ccode == 342,
        year %in% 2006:2018
      ) |>
      transmute(
        YEAR_JOIN = as.integer(year),
        DURABLE_RAW = as.numeric(durable)
      ),
    by = "YEAR_JOIN"
  )

if (nrow(durability_bridge) < 2L) {
  stop("Insufficient overlapping Serbian observations to calibrate durability.")
}

durability_fit <- lm(
  REGIME_DURABILITY_YEARS ~ DURABLE_RAW,
  data = durability_bridge
)

# The transformation should be essentially perfectly linear.
if (max(abs(residuals(durability_fit)), na.rm = TRUE) > 1e-6) {
  stop("Raw Polity durability does not map cleanly onto the master-data scale.")
}

# ------------------------------------------------------------------------------
# Extracting and constructing the Polity variables -----------------------------
# ------------------------------------------------------------------------------
polity_fry <- polity_raw |>
  filter(
    ccode == 347,
    year %in% SERBIA_FILL_YEARS
  ) |>
  transmute(
    YEAR_JOIN = as.integer(year),
    DURABLE_RAW = as.numeric(durable),
    
    # PITF/Polity construction
    PARTIAL_FACTIONALISM_PATCH = as.numeric(
      exrec %in% 6:8 & parcomp == 3
    )
  ) |>
  mutate(
    DURABILITY_PATCH = as.numeric(
      predict(
        durability_fit,
        newdata = data.frame(DURABLE_RAW = DURABLE_RAW)
      )
    )
  ) |>
  select(-DURABLE_RAW)

serbia_patch <- bmr_fry |>
  inner_join(polity_fry, by = "YEAR_JOIN")

stopifnot(
  nrow(serbia_patch) == length(SERBIA_FILL_YEARS),
  !anyDuplicated(serbia_patch$YEAR_JOIN),
  all(complete.cases(serbia_patch))
)

# ------------------------------------------------------------------------------
# Filling only missing Serbian values ------------------------------------------
# ------------------------------------------------------------------------------
standardized_data_final <- standardized_data_final |>
  mutate(YEAR_JOIN = as.integer(YEAR)) |>
  left_join(serbia_patch, by = "YEAR_JOIN") |>
  mutate(
    SERBIA_FRY_COVARIATE_FILL = as.integer(
      COUNTRY_NAME == "Serbia" &
        YEAR_JOIN %in% SERBIA_FILL_YEARS
    ),
    
    BOIX_DEMOCRACY = if_else(
      SERBIA_FRY_COVARIATE_FILL == 1L,
      coalesce(as.numeric(BOIX_DEMOCRACY), BOIX_PATCH),
      as.numeric(BOIX_DEMOCRACY)
    ),
    
    REGIME_DURABILITY_YEARS = if_else(
      SERBIA_FRY_COVARIATE_FILL == 1L,
      coalesce(
        as.numeric(REGIME_DURABILITY_YEARS),
        DURABILITY_PATCH
      ),
      as.numeric(REGIME_DURABILITY_YEARS)
    ),
    
    PARTIAL_DEMOCRACY_WITH_FACTIONALISM = if_else(
      SERBIA_FRY_COVARIATE_FILL == 1L,
      coalesce(
        as.numeric(PARTIAL_DEMOCRACY_WITH_FACTIONALISM),
        PARTIAL_FACTIONALISM_PATCH
      ),
      as.numeric(PARTIAL_DEMOCRACY_WITH_FACTIONALISM)
    )
  ) |>
  select(
    -YEAR_JOIN,
    -BOIX_PATCH,
    -DURABILITY_PATCH,
    -PARTIAL_FACTIONALISM_PATCH
  )

# ------------------------------------------------------------------------------
# Verifying the lag year for Serbia's 2000 exit --------------------------------
# ------------------------------------------------------------------------------
serbia_1999_check <- standardized_data_final |>
  filter(
    COUNTRY_NAME == "Serbia",
    as.integer(YEAR) == 1999L
  ) |>
  select(
    COUNTRY_NAME,
    YEAR,
    BOIX_DEMOCRACY,
    REGIME_DURABILITY_YEARS,
    PARTIAL_DEMOCRACY_WITH_FACTIONALISM,
    SERBIA_FRY_COVARIATE_FILL
  )

print(serbia_1999_check)

stopifnot(
  nrow(serbia_1999_check) == 1L,
  all(complete.cases(serbia_1999_check))
)

# Saving the data --------------------------------------------------------------
saveRDS(
   standardized_data_final,
   "standardized_data_final_serbia_fixed.RDS"
 )
