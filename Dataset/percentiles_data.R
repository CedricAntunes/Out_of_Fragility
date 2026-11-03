# Attaching my packages --------------------------------------------------------
library(here)
library(tidyverse)
library(countrycode)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)

# Cleaning my environment
rm(list = ls())


# Loading data  ----------------------------------------------------------------

# V-Dem data 
vdem_normalized <- read.csv("output-data/VDem_normalized.csv")

vdem_percentiles <- read.csv("output-data/VDem_percentiles.csv") |>
  # Selecting only percentiles of interest 
  select(Year, P30, P35, P40) |>
  # Renaming variables for match
  rename(YEAR = Year,
         VDEM_30TH_PERCENTILE = P30,
         VDEM_35TH_PERCENTILE = P35,
         VDEM_40TH_PERCENTILE = P40)

# WGI data
wgi_normalized <- read.csv("output-data/WGI_normalized.csv")

wgi_percentiles <- read.csv("output-data/WGI_percentiles.csv") |>
  # Selecting only percentiles of interest 
  select(Year, P30, P35) |>
  # Renaming variables for match
  rename(YEAR = Year,
         WGI_30TH_PERCENTILE = P30,
         WGI_35TH_PERCENTILE = P35)

# Cleaning Dependent Variables -------------------------------------------------

# V-Dem data 
vdem_normalized$iso_code3 <- countrycode(sourcevar = vdem_normalized$Country.Name, 
                                         origin = "country.name", 
                                         destination = "iso3c")

# Checking for NAs: cases in which setting the right ISO-code did not work
cbind(lapply(lapply(vdem_normalized, is.na), sum)) 

# Checking missing countries
missing_iso_vdem <- vdem_normalized[is.na(vdem_normalized$iso_code3), ]

# Subsetting missing countries 
missing_countries <- unique(missing_iso_vdem$Country.Name) |>
  print()

# East Germany 
vdem_normalized$iso_code3[vdem_normalized$Country.Name == "German Democratic Republic" & 
                            is.na(vdem_normalized$iso_code3)] <- "DEU"

# Kosovo
vdem_normalized$iso_code3[vdem_normalized$Country.Name == "Kosovo" & 
                            is.na(vdem_normalized$iso_code3)] <- "XKX"

vdem_normalized_final <- vdem_normalized |>
  # Dropping Zanzibar and Somaliland 
  na.omit() |> 
  # Renaming vars for match 
  rename(COUNTRY_NAME = Country.Name,
         YEAR = Year,
         ISO_CODE_3 = iso_code3,
         VDEM_LOADING_FACTOR_1 = Loading.Factor.1,
         VDEM_LOADING_FACTOR_2 = Loading.Factor.2,
         VDEM_LOADING_FACTOR_1_NORMALIZED = Loading.Factor.1.Normalized,
         VDEM_LOADING_FACTOR_2_NORMALIZED = Loading.Factor.2.Normalized) |>
  filter(COUNTRY_NAME != "German Democratic Republic")

# WGI data 
wgi_normalized$iso_code3 <- countrycode(sourcevar = wgi_normalized$Country.Name, 
                                        origin = "country.name", 
                                        destination = "iso3c")

# Checking for NAs: cases in which setting the right ISO-code did not work
cbind(lapply(lapply(wgi_normalized, is.na), sum)) 

# Checking missing countries
missing_iso_wgi <- wgi_normalized[is.na(wgi_normalized$iso_code3), ]

# Subsetting missing countries 
missing_countries_wgi <- unique(missing_iso_wgi$Country.Name) |>
  print()

# Sao Tome and Principe
wgi_normalized$iso_code3[wgi_normalized$Country.Name == "SÃ£o TomÃ© and Principe" & 
                         is.na(wgi_normalized$iso_code3)] <- "STP"

# Kosovo
wgi_normalized$iso_code3[wgi_normalized$Country.Name == "Kosovo" & 
                         is.na(wgi_normalized$iso_code3)] <- "XKX"

wgi_normalized_final <- wgi_normalized |>
  # Dropping La Réunion and the Dutch Antilles
  na.omit() |>
  # Renaming vars for match 
  rename(COUNTRY_NAME = Country.Name,
         YEAR = Year,
         ISO_CODE_3 = iso_code3, 
         WGI_LOADING_FACTOR_1 = Loading.Factor.1,
         WGI_LOADING_FACTOR_2 = Loading.Factor.2,
         WGI_LOADING_FACTOR_1_NORMALIZED = Loading.Factor.1.Normalized,
         WGI_LOADING_FACTOR_2_NORMALIZED = Loading.Factor.2.Normalized)

# Saving data -------------------------------------------------------------

# Saving filves

# V-Dem data 
save(vdem_normalized_final, file = "output-data/vdem_output_final.RDS")
save(vdem_percentiles, file = "output-data/vdem_output_percentiles_final.RDS")
write.csv(vdem_normalized_final, "output-data/vdem_output_final.csv")
write.csv(vdem_percentiles, file = "output-data/vdem_output_percentiles_final.csv")

# WGI data 
save(wgi_normalized_final, file = "output-data/wgi_output_final.RDS")
save(wgi_percentiles, file = "output-data/wgi_output_percentiles_final.RDS")
write.csv(wgi_normalized_final, "output-data/wgi_output_final.csv")
write.csv(wgi_percentiles, file = "output-data/wgi_output_percentiles_final.csv")