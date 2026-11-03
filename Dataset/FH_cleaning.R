# Author: Cedric Antunes -------------------------------------------------------
# Date: January, 2026 ----------------------------------------------------------

# Cleaning my environment
rm(list = ls())

# Managing memory 
gc()

# Required packages ------------------------------------------------------------
library(tidyverse)
library(tidyxl)
library(unpivotr)
library(countrycode)
library(readxl)

# ------------------------------------------------------------------------------
# Helper -----------------------------------------------------------------------
# ------------------------------------------------------------------------------
parse_period <- function(x) {
  x <- str_squish(x)
  
  # Plain year (i.e., full calendar year)
  if (str_detect(x, "^\\d{4}$")) {
    y <- as.integer(x)
    return(tibble(
      start = ymd(sprintf("%04d-01-01", y)),
      end   = ymd(sprintf("%04d-12-31", y))
    ))
  }
  
  # Mapping month-year pairs inside the string
  m <- str_match_all(x, "([A-Za-z]{3})\\.?\\s*(\\d{4})")[[1]]
  
  # If we got 2+ matches, we treat first as start, second as end
  if (nrow(m) >= 2) {
    sm <- match(str_to_title(m[1,2]), month.abb)
    sy <- as.integer(m[1,3])
    em <- match(str_to_title(m[2,2]), month.abb)
    ey <- as.integer(m[2,3])
    
    start <- ymd(sprintf("%04d-%02d-01", sy, sm))
    end0  <- ymd(sprintf("%04d-%02d-01", ey, em))
    end   <- ceiling_date(end0, "month") - days(1)
    
    return(tibble(start = start, end = end))
  }
  
  # Sainity fallback: if it’s weirdly formatted, 
  # we use last year as full year
  y <- as.integer(tail(str_extract_all(x, "\\d{4}")[[1]], 1))
  tibble(
    start = ymd(sprintf("%04d-01-01", y)),
    end   = ymd(sprintf("%04d-12-31", y))
  )
}

# ------------------------------------------------------------------------------
# Loading the data: Freedom House ----------------------------------------------
# ------------------------------------------------------------------------------
file  <- "D:/Users/cedric/Downloads/Country_and_Territory_Ratings_and_Statuses_FIW_1973-2025_0.xlsx"
sheet <- "Country Ratings, Statuses "

# Reading the data
cells <- xlsx_cells(file, 
                    sheet = sheet)

# ------------------------------------------------------------------------------
# Preparing the data -----------------------------------------------------------
# ------------------------------------------------------------------------------
ans <- cells |>
  # Dropping empty cells
  filter(!is_blank) |>
  # Setup headers from top and left side
  behead("up", "Survey_Edition") |>
  behead("up", "year") |>
  behead("up", "item") |>
  behead("left", "country") |>
  # Triming spaces
  mutate(item = trimws(item)) |>
  mutate(value = ifelse(item == "Status", character, numeric)) |>
  # Dropping unneeded data
  select(country, year, item, value) |>
  # Filling down missing year info
  fill(year, .direction = "down") |>
  # Setting to wide
  pivot_wider(names_from = item, values_from = value)

ans$Status <- trimws(ans$Status)

# ------------------------------------------------------------------------------
# Data Cleaning ----------------------------------------------------------------
# ------------------------------------------------------------------------------
ans <- ans |>
  mutate(
    FH_DEMOCRACY = if_else(Status %in% c("F", "PF"), 1L, 0L),
    FH_AUTOCRACY = if_else(Status == "NF", 1L, 0L)
  )

ans_B <- ans |>
  mutate(year_raw = year) |>
  rowwise() |>
  mutate(period = list(parse_period(year_raw))) |>
  unnest(period) |>
  ungroup() |>
  mutate(
    y0 = year(start),
    y1 = year(end)
  ) |>
  rowwise() |>
  mutate(year_clean = list(seq(y0, y1))) |>
  unnest(year_clean) |>
  ungroup() |>
  mutate(
    y_start = ymd(sprintf("%04d-01-01", year_clean)),
    y_end   = ymd(sprintf("%04d-12-31", year_clean)),
    overlap_days = as.integer(pmax(
      0,
      as.numeric(pmin(end, y_end) - pmax(start, y_start) + 1)
    ))
  ) |>
  group_by(country, year_clean) |>
  slice_max(overlap_days, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-y0, -y1, -y_start, -y_end, -overlap_days, -start, -end)

# Cleaned data -----------------------------------------------------------------
fh_clean <- ans_B |>
  select(country, 
         FH_DEMOCRACY,
         FH_AUTOCRACY,
         year_clean) |>
  rename(COUNTRY_NAME = country,
         YEAR = year_clean)

# Adding iso-code3 for matching 
fh_clean$iso_code3 <- countrycode(sourcevar = fh_clean$COUNTRY_NAME, 
                                   origin = "country.name", 
                                   destination = "iso3c")

# Manual cleaning of problematic countries -------------------------------------
fh_clean |>
  filter(is.na(iso_code3)) |>
  distinct(COUNTRY_NAME) |>
  arrange(COUNTRY_NAME)

manual_iso3 <- c(
  "Kosovo"               = "XKX",
  "Micronesia"           = "FSM",
  "Czechoslovakia"       = "CSK",
  "Yugoslavia"           = "YUG",
  "Serbia and Montenegro"= "SCG"
)

fh_clean <- fh_clean |>
  mutate(
    iso_code3 = if_else(
      is.na(iso_code3),
      manual_iso3[COUNTRY_NAME] %>% unname(),
      iso_code3
    )
  ) |>
  rename(ISO_CODE_3 =iso_code3) |>
  filter(YEAR <= 2022) |>
  mutate(YEAR = as.character(YEAR))

# Final sanity check: we are good to go! ---------------------------------------
fh_clean |>
  filter(is.na(ISO_CODE_3)) |>
  count(COUNTRY_NAME) |>
  arrange(desc(n)) 

# ------------------------------------------------------------------------------
# Loading the data: Democracy-Dictatorship -------------------------------------
# ------------------------------------------------------------------------------
dd_clean <- read_excel("D:/Users/cedric/Downloads/ddrevisited_data_v1.xls") |>
  filter(year >= 1970) |>
  select(ctryname,
         year, 
         dpicode,
         democracy) |>
  mutate(year = as.character(year)) |>
  rename(COUNTRY_NAME = ctryname,
         YEAR = year, 
         ISO_CODE_3 = dpicode, 
         DD_DEMOCRACY = democracy)

# ------------------------------------------------------------------------------
# JOIN -------------------------------------------------------------------------
# ------------------------------------------------------------------------------
fh_dd_clean <- fh_clean |>
  left_join(dd_clean, 
            by = c("YEAR",
                   "ISO_CODE_3"))

fh_dd_final <- fh_dd_clean |>
  select(-6) |>
  rename(COUNTRY_NAME = COUNTRY_NAME.x)

# ------------------------------------------------------------------------------
# Saving the data --------------------------------------------------------------
# ------------------------------------------------------------------------------
save(fh_dd_final, 
     file = "fh_dd_final.RDS")

# Keys duplicated in Freedom House data (right table)
fh_dups <- fh_dd_final %>%
  count(ISO_CODE_3, YEAR, name = "n_fh") %>%
  filter(n_fh > 1) %>%
  arrange(desc(n_fh))

fh_dups

left_dups <- final_clean_percentiles_data %>%
  count(ISO_CODE_3, YEAR, name = "n_left") %>%
  filter(n_left > 1) %>%
  arrange(desc(n_left))

left_dups

problem <- final_clean_percentiles_data %>%
  inner_join(fh_dd_final %>% count(ISO_CODE_3, YEAR, name = "n_fh"),
             by = c("ISO_CODE_3","YEAR")) %>%
  filter(n_fh > 1) %>%
  count(ISO_CODE_3, YEAR, n_fh, sort = TRUE)

problem

fh_dd_final %>%
  filter(ISO_CODE_3 %in% c("DEU","RUS","VNM","YEM")) %>%
  distinct(ISO_CODE_3, COUNTRY_NAME) %>%
  arrange(ISO_CODE_3, COUNTRY_NAME)

library(dplyr)

canonical <- tibble::tibble(
  ISO_CODE_3      = c("DEU","RUS","VNM","YEM"),
  canonical_name  = c("Germany","Russia","Vietnam","Yemen")
)

fh_dd_final_1 <- fh_dd_final %>%
  left_join(canonical, by = "ISO_CODE_3") %>%
  group_by(ISO_CODE_3, YEAR) %>%
  arrange(desc(COUNTRY_NAME == canonical_name)) %>%  # keep canonical if present
  slice(1) %>%
  ungroup() %>%
  select(-canonical_name)

# sanity check: should now be unique
stopifnot(nrow(fh_dd_final_1) == nrow(distinct(fh_dd_final_1, ISO_CODE_3, YEAR)))

save(fh_dd_final_1, 
     file = "fh_dd_final.RDS")
