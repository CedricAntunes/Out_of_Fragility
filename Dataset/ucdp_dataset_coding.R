# Author: Cedric Antunes (FGV-CEPESP) -------------------------------------
# Date: January, 2026 -----------------------------------------------------
# Attaching my packages ---------------------------------------------------
library(here)
library(tidyverse)
library(data.table)
library(countrycode)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

# Cleaning my environment
rm(list = ls())

# Loading data ------------------------------------------------------------

# Unzipping UCDP/PRIO Armed Conflict files
unzip(
  here("data", "ucdp-prio-acd-231-xlsx.zip"),
  exdir = here("data"))

# Cleaning data -----------------------------------------------------------

# Extracting only years of interest (starting with 1970 and ending with 2018)
ucdp <- read_excel("data/UcdpPrioConflict_v23_1.xlsx")|>
  # Setting year var as numeric
  mutate(year = as.numeric(year)) |>
  # Filtering for years of interest
  filter(year >= 1970 & year <= 2018)

# Adding Isoc2c (iso-code) variable to the dataset
ucdp$iso_code2 <- countrycode(sourcevar = ucdp$location, 
                              origin = "country.name", 
                              destination = "iso2c")

## For sure, this is abit problematic

# Checking for NAs: cases in which setting the right ISO-code did not work
cbind(lapply(lapply(ucdp, is.na), sum)) # ISO-code did not work for ~5% of cases

# Checking those cases
missing_iso <- ucdp[is.na(ucdp$iso_code2), ] # The ´location´ consist of more
                                             # than one country

# Let's fix this: create a string
missing_countries <- missing_iso$location

# Splitting countries: creating a single observation for 
# each individual country
split_countries <- strsplit(missing_countries, ", ")

# Fixing this headeach: create a single row for each individual country
# and repeat the data entries
missing_iso <- data.frame(
  location = rep(missing_iso$location, sapply(split_countries, length)),
  # Repeat all other variables (for each indivudla country) with lapply
  do.call(cbind, lapply(missing_iso[, -c(2)], 
                        function(x) rep(x, 
                                        sapply(split_countries, length)))),
  # Creating a single observation for each individual country
  individual_country = unlist(split_countries),
  stringsAsFactors = FALSE
)

# Assignning ISO-code to the new split observations
missing_iso$iso_code2 <- countrycode(sourcevar = missing_iso$individual_country, 
                                     origin = "country.name", 
                                     destination = "iso2c")

cbind(lapply(lapply(missing_iso, is.na), sum)) # We still have a problem for
                                               # 23 observations


different_vars <- setdiff(names(ucdp), names(missing_iso))

missing_iso_still <- missing_iso[is.na(missing_iso$iso_code2), ]
# The problem is Yemen!

# Assigning the Yemen ISO-code
missing_iso$iso_code2[is.na(missing_iso$iso_code2)] <- "YE"

# We are finally ready to go!
cbind(lapply(lapply(missing_iso, is.na), sum))

ucdp <- ucdp |> 
  filter(!is.na(iso_code2)) |>
  mutate(individual_country = location)

# Final df 
ucdp_clean <- rbind(ucdp, missing_iso)

## Adding iso-code 3 to the final df
ucdp_clean$iso_code3 <- countrycode(sourcevar = ucdp_clean$individual_country, 
                                    origin = "country.name", 
                                    destination = "iso3c")

ucdp_clean$iso_code3[is.na(ucdp_clean$iso_code3)] <- "YEM"

# Saving data -------------------------------------------------------------
# Saving filves
save(ucdp_clean, file = "ucdp_clean.RDS")
write.csv(ucdp_clean, "ucdp_clean.csv")
