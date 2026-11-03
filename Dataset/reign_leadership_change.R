# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: June, 2026 -------------------------------------------------------------

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(countrycode)
  library(slider)
  library(janitor)
})

# ------------------------------------------------------------------------------
# Loading REIGN leader-list data -----------------------------------------------
# ------------------------------------------------------------------------------
reign_raw <- readr::read_csv(
  "D:/Users/cedric/Downloads/leader_list_8_21.csv",
  locale = readr::locale(encoding = "Latin1"),
  show_col_types = FALSE
) |>
  clean_names()

# Inspecting columns
names(reign_raw)

# ------------------------------------------------------------------------------
# 4. Manual ISO patches for historical / non-standard REIGN-COW entities -------
# ------------------------------------------------------------------------------
manual_reign_iso <- tribble(
  ~stateabb, ~ccode, ~ISO_CODE_3_manual,
  "GFR", 260L, "DEU", # West Germany / Federal Republic of Germany
  "GMY", 260L, "DEU", # Germany after reunification
  "GDR", 265L, "DDR", # East Germany / German Democratic Republic
  "CZE", 315L, "CSK", # Czechoslovakia
  "YUG", 345L, "YUG", # Yugoslavia
  "KOS", 347L, "XKX", # Kosovo; adjust if your main data uses KOS or XKS
  "YAR", 678L, "YAR", # Yemen Arab Republic / North Yemen
  "YPR", 680L, "YMD", # People's Democratic Republic of Yemen / South Yemen
  "RVN", 817L, "RVN"  # Republic of Vietnam / South Vietnam
)

# ------------------------------------------------------------------------------
# Cleaning the data ------------------------------------------------------------
# ------------------------------------------------------------------------------
reign_leaders <- reign_raw |>
  select(
    stateabb, ccode, leadid, leader,
    sdate, smonth, syear, emonth, eyear,
    birthyear, gender, militarycareer,
    l_month, l_year
  ) |>
  mutate(
    ccode = as.integer(ccode),
    sdate = as.integer(sdate),
    smonth = as.integer(smonth),
    syear = as.integer(syear),
    emonth = as.integer(emonth),
    eyear = as.integer(eyear),
    birthyear = as.integer(birthyear),
    gender = as.integer(gender),
    militarycareer = as.integer(militarycareer),
    
    # First attempt: convert COW code to ISO3.
    ISO_CODE_3 = countrycode(
      ccode,
      origin = "cown",
      destination = "iso3c",
      warn = FALSE
    )
  ) |>
  left_join(
    manual_reign_iso,
    by = c("stateabb", "ccode")
  ) |>
  mutate(
    # Use manual ISO code when countrycode() fails.
    ISO_CODE_3 = coalesce(ISO_CODE_3, ISO_CODE_3_manual),
    
    # Start date.
    start_date = make_date(
      year = syear,
      month = if_else(is.na(smonth), 1L, smonth),
      day = if_else(is.na(sdate), 1L, sdate)
    ),
    
    # End date.
    # The uploaded leader-list file has end month/year, but not an end day.
    # We assign the end date to the last day of the recorded end month.
    # Open-ended leaders are carried forward through 2022-12-31.
    end_date = case_when(
      !is.na(eyear) & !is.na(emonth) ~ ceiling_date(make_date(eyear, emonth, 1), "month") - days(1),
      !is.na(eyear) &  is.na(emonth) ~ make_date(eyear, 12, 31),
      TRUE ~ make_date(2022, 12, 31)
    )
  ) |>
  select(-ISO_CODE_3_manual) |>
  filter(!is.na(ISO_CODE_3)) |>
  filter(!is.na(start_date), !is.na(end_date)) |>
  arrange(ccode, start_date, leadid) |>
  group_by(ccode) |>
  mutate(
    # We do not treat the first observed leader in each COW country series
    # as a leadership change because the previous leader may be outside the file.
    leader_order = row_number(),
    leader_start_event = 1L,
    leader_change_event = if_else(leader_order == 1L, 0L, 1L)
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# Sanity check: Diagnosing remaining country-code problems ---------------------
# ------------------------------------------------------------------------------
unmatched_iso <- reign_leaders |>
  filter(is.na(ISO_CODE_3)) |>
  distinct(stateabb, ccode) |>
  arrange(ccode)

# We are good to go!
print(unmatched_iso, n = Inf)

# ------------------------------------------------------------------------------
# Expanding leader-tenure spells to leader-month data --------------------------
# ------------------------------------------------------------------------------
panel_start <- make_date(1971, 1, 1)
panel_end   <- make_date(2022, 12, 31)

reign_monthly <- reign_leaders |>
  mutate(
    spell_start = pmax(start_date, panel_start),
    spell_end   = pmin(end_date, panel_end)
  ) |>
  filter(spell_start <= spell_end) |>
  rowwise() |>
  mutate(
    month_date = list(seq.Date(
      from = floor_date(spell_start, "month"),
      to   = floor_date(spell_end, "month"),
      by   = "month"
    ))
  ) |>
  ungroup() |>
  unnest(month_date) |>
  mutate(
    YEAR = year(month_date),
    MONTH = month(month_date),
    
    leader_age = if_else(
      !is.na(birthyear),
      YEAR - birthyear,
      NA_integer_
    ),
    
    tenure_months_calc = interval(start_date, month_date) %/% months(1) + 1L,
    tenure_years_calc = tenure_months_calc / 12
  ) |>
  filter(YEAR >= 1971, YEAR <= 2022)


# ------------------------------------------------------------------------------
# Collapsing to main leader by country-year ------------------------------------
# ------------------------------------------------------------------------------
# Rule:
#   Main leader in a country-year = leader who was in power for the largest
#   number of months in that year.
main_leader_year <- reign_monthly |>
  count(
    ISO_CODE_3, YEAR,
    ccode, stateabb, leadid, leader,
    gender, militarycareer, birthyear,
    name = "months_in_power_year"
  ) |>
  group_by(ISO_CODE_3, YEAR) |>
  arrange(desc(months_in_power_year), leadid, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(
    leader_age_main = if_else(
      !is.na(birthyear),
      YEAR - birthyear,
      NA_integer_
    )
  ) |>
  select(
    ISO_CODE_3, YEAR,
    REIGN_CCODE = ccode,
    REIGN_STATEABB = stateabb,
    leadid_main = leadid,
    leader_main = leader,
    months_in_power_year,
    leader_age_main,
    leader_male_main = gender,
    leader_militarycareer_main = militarycareer
  )

leaders_in_year <- reign_monthly |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(
    n_leaders_in_year = n_distinct(leadid),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Leadership-change events by country-year -------------------------------------
# ------------------------------------------------------------------------------
# A leadership change is coded in the year in which a new leader starts,
# except for the first leader observed in each COW country series.
leader_change_years <- reign_leaders |>
  filter(syear >= 1971, syear <= 2022) |>
  transmute(
    ISO_CODE_3,
    YEAR = syear,
    leadid,
    leader,
    start_date,
    leader_start_event,
    leader_change_event
  ) |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(
    n_leader_starts_year = sum(leader_start_event, na.rm = TRUE),
    n_leader_changes_year = sum(leader_change_event, na.rm = TRUE),
    leader_start_year = as.integer(n_leader_starts_year > 0),
    leader_change_year = as.integer(n_leader_changes_year > 0),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Building balanced country-year REIGN panel, 1971-2022 ------------------------
# ------------------------------------------------------------------------------
country_year_grid <- reign_leaders |>
  distinct(ISO_CODE_3) |>
  tidyr::crossing(YEAR = 1971:2022)

reign_country_year <- country_year_grid |>
  left_join(main_leader_year, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(leaders_in_year, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(leader_change_years, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(
    n_leaders_in_year = replace_na(n_leaders_in_year, 0L),
    n_leader_starts_year = replace_na(n_leader_starts_year, 0L),
    n_leader_changes_year = replace_na(n_leader_changes_year, 0L),
    leader_start_year = replace_na(leader_start_year, 0L),
    leader_change_year = replace_na(leader_change_year, 0L)
  ) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(
    # Strict lag:
    # leadership change in the previous calendar year.
    leader_change_lag1 = lag(leader_change_year, 1, default = 0L),
    
    # Past windows excluding the current year.
    # These are useful if you do not want the model itself to lag the variable.
    leader_change_past1 = leader_change_lag1,
    
    leader_change_past3 = slider::slide_int(
      leader_change_lag1,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 2,
      .complete = FALSE
    ),
    
    leader_change_past5 = slider::slide_int(
      leader_change_lag1,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    ),
    
    # Windows including the current year.
    # These are useful if your hazard pipeline later lags all covariates by one year.
    leader_change_recent3_incl_current = slider::slide_int(
      leader_change_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 2,
      .complete = FALSE
    ),
    
    leader_change_recent5_incl_current = slider::slide_int(
      leader_change_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    ),
    
    # Years since last leadership change.
    last_change_year_tmp = if_else(
      leader_change_year == 1L,
      YEAR,
      NA_integer_
    )
  ) |>
  tidyr::fill(last_change_year_tmp, .direction = "down") |>
  mutate(
    years_since_leader_change = YEAR - last_change_year_tmp
  ) |>
  ungroup() |>
  select(
    ISO_CODE_3, YEAR,
    REIGN_CCODE, REIGN_STATEABB,
    leader_main, leadid_main,
    months_in_power_year,
    n_leaders_in_year,
    leader_age_main,
    leader_male_main,
    leader_militarycareer_main,
    leader_start_year,
    leader_change_year,
    n_leader_changes_year,
    leader_change_past1,
    leader_change_past3,
    leader_change_past5,
    leader_change_recent3_incl_current,
    leader_change_recent5_incl_current,
    years_since_leader_change
  )

# ------------------------------------------------------------------------------
# Diagnostics ------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Checking uniqueness by ISO_CODE_3-YEAR
duplicates_check <- reign_country_year |>
  count(ISO_CODE_3, YEAR) |>
  filter(n > 1)

print(duplicates_check, n = Inf)

# Overall coverage
coverage_summary <- reign_country_year |>
  summarise(
    min_year = min(YEAR, na.rm = TRUE),
    max_year = max(YEAR, na.rm = TRUE),
    n_countries = n_distinct(ISO_CODE_3),
    n_rows = n(),
    n_rows_with_main_leader = sum(!is.na(leader_main)),
    share_rows_with_main_leader = mean(!is.na(leader_main))
  )

print(coverage_summary)

# ------------------------------------------------------------------------------
# Final preparation ------------------------------------------------------------
# ------------------------------------------------------------------------------
reign_for_merge <- reign_country_year |>
  select(
    ISO_CODE_3,
    YEAR,
    leader_change_year,
    leader_change_recent3_incl_current,
    leader_change_recent5_incl_current,
    years_since_leader_change,
    leader_militarycareer_main
  ) |>
  rename(
    reign_leader_change_year = leader_change_year,
    reign_leader_change_recent3 = leader_change_recent3_incl_current,
    reign_leader_change_recent5 = leader_change_recent5_incl_current,
    reign_years_since_leader_change = years_since_leader_change,
    reign_leader_militarycareer = leader_militarycareer_main
  )

colnames(reign_for_merge) <- toupper(colnames(reign_for_merge))

reign_for_merge <- reign_for_merge |>
  mutate(YEAR = as.character(YEAR))

# Saving revised data ----------------------------------------------------------
save(reign_for_merge,
     file = "reign_leadership_change.RDS")