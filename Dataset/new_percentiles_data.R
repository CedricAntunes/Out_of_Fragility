# Attaching my packages --------------------------------------------------------
library(here)
library(tidyverse)
library(countrycode)
library(lubridate)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)

# Cleaning my environment
rm(list = ls())


# Loading data  ----------------------------------------------------------------

# v-dem_new data 
vdem_new_normalized <- read.csv("vdem_new_normalized.csv")

vdem_new_percentiles <- read.csv("vdem_new_percentiles.csv") |>
  tail(-1) |>
  row_to_names(1)|>
  # Selecting only percentiles of interest 
  select(Year, P30, P35, P40) |>
  # Renaming variables for match
  rename(YEAR = Year,
         NEW_VDEM_30TH_PERCENTILE = P30,
         NEW_VDEM_35TH_PERCENTILE = P35,
         NEW_VDEM_40TH_PERCENTILE = P40)

# wgi_new data
wgi_new_normalized <- read.csv("wgi_new_normalized.csv")

wgi_new_percentiles <- read.csv("wgi_new_percentiles.csv") |>
  tail(-1) |>
  row_to_names(1)|>
  # Selecting only percentiles of interest 
  select(Year, P30, P35) |>
  # Renaming variables for match
  rename(YEAR = Year,
         NEW_WGI_30TH_PERCENTILE = P30,
         NEW_WGI_35TH_PERCENTILE = P35)

# Cleaning Dependent variables -------------------------------------------------

# v-dem_new data 
vdem_new_normalized$iso_code3 <- countrycode(sourcevar = vdem_new_normalized$Country.Name, 
                                         origin = "country.name", 
                                         destination = "iso3c")

# Checking for NAs: cases in which setting the right ISO-code did not work
cbind(lapply(lapply(vdem_new_normalized, is.na), sum)) 

# Checking missing countries
missing_iso_vdem_new <- vdem_new_normalized[is.na(vdem_new_normalized$iso_code3), ]

# Subsetting missing countries 
missing_countries <- unique(missing_iso_vdem_new$Country.Name) |>
  print()

# East Germany 
vdem_new_normalized$iso_code3[vdem_new_normalized$Country.Name == "German dem_newocratic Republic" & 
                            is.na(vdem_new_normalized$iso_code3)] <- "DEU"

# Kosovo
vdem_new_normalized$iso_code3[vdem_new_normalized$Country.Name == "Kosovo" & 
                            is.na(vdem_new_normalized$iso_code3)] <- "XKX"

vdem_new_normalized_final <- vdem_new_normalized |>
  # Dropping Zanzibar and Somaliland 
  na.omit() |> 
  # Renaming vars for match 
  rename(COUNTRY_NAME = Country.Name,
         YEAR = Year,
         ISO_CODE_3 = iso_code3,
         NEW_VDEM_LOADING_FACTOR_1 = Loading.Factor.1,
         NEW_VDEM_LOADING_FACTOR_2 = Loading.Factor.2,
         NEW_VDEM_LOADING_FACTOR_1_NORMALIZED = Loading.Factor.1.Normalized,
         NEW_VDEM_LOADING_FACTOR_2_NORMALIZED = Loading.Factor.2.Normalized) |>
  filter(COUNTRY_NAME != "German dem_newocratic Republic")

# wgi_new data 
wgi_new_normalized$iso_code3 <- countrycode(sourcevar = wgi_new_normalized$Country.Name, 
                                        origin = "country.name", 
                                        destination = "iso3c")

# Checking for NAs: cases in which setting the right ISO-code did not work
cbind(lapply(lapply(wgi_new_normalized, is.na), sum)) 

# Checking missing countries
missing_iso_wgi_new <- wgi_new_normalized[is.na(wgi_new_normalized$iso_code3), ]

# Subsetting missing countries 
missing_countries_wgi_new <- unique(missing_iso_wgi_new$Country.Name) |>
  print()

# Sao Tome and Principe
wgi_new_normalized$iso_code3[wgi_new_normalized$Country.Name == "SÃ£o TomÃ© and Principe" & 
                           is.na(wgi_new_normalized$iso_code3)] <- "STP"

# Kosovo
wgi_new_normalized$iso_code3[wgi_new_normalized$Country.Name == "Kosovo" & 
                           is.na(wgi_new_normalized$iso_code3)] <- "XKX"

# La Réunion
wgi_new_normalized$iso_code3[wgi_new_normalized$Country.Name == "RÃ©union" & 
                               is.na(wgi_new_normalized$iso_code3)] <- "REU"

wgi_new_normalized_final <- wgi_new_normalized |>
  # Dropping La Réunion and the Dutch Antilles
  na.omit() |>
  # Renaming vars for match 
  rename(COUNTRY_NAME = Country.Name,
         YEAR = Year,
         ISO_CODE_3 = iso_code3, 
         NEW_WGI_LOADING_FACTOR_1 = Loading.Factor.1,
         NEW_WGI_LOADING_FACTOR_2 = Loading.Factor.2,
         NEW_WGI_LOADING_FACTOR_1_NORMALIZED = Loading.Factor.1.Normalized,
         NEW_WGI_LOADING_FACTOR_2_NORMALIZED = Loading.Factor.2.Normalized)

# Saving data -------------------------------------------------------------

# Saving filves

# v-dem_new data 
save(vdem_new_normalized_final, file = "output-data/vdem_new_output_final.RDS")
save(vdem_new_percentiles, file = "output-data/vdem_new_output_percentiles_final.RDS")
write.csv(vdem_new_normalized_final, "output-data/vdem_new_output_final.csv")
write.csv(vdem_new_percentiles, file = "output-data/vdem_new_output_percentiles_final.csv")

# wgi_new data 
save(wgi_new_normalized_final, file = "output-data/wgi_new_output_final.RDS")
save(wgi_new_percentiles, file = "output-data/wgi_new_output_percentiles_final.RDS")
write.csv(wgi_new_normalized_final, "output-data/wgi_new_output_final.csv")
write.csv(wgi_new_percentiles, file = "output-data/wgi_new_output_percentiles_final.csv")