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
p5v2018 <- read_excel("p5v2018.xls") |>
  # Filtering for years of interest
  filter(year >= 1970 & year <= 2023)

p5v2018$iso_code2 <- countrycode(sourcevar = p5v2018$country, 
                                 origin = "country.name", 
                                 destination = "iso2c")

# Renaming vars
p5v2018 <- p5v2018 |>
  rename(iso_code3 = scode)

# Checking for NAs: cases in which setting the ISO-code did not work
cbind(lapply(lapply(p5v2018, is.na), sum)) # ISO-code did not work for 
# ~0.8% of cases

# Some noise again
missing_iso <- p5v2018[is.na(p5v2018$iso_code2), ] 

# Saving data -------------------------------------------------------------
save(p5v2018, file = "polV.RDS")
write.csv(p5v2018, "polv.csv")