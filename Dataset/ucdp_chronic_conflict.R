# Revised UCDP acute and chronic conflict variables ----------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: August, 2026 -----------------------------------------------------------
# Purpose
#   1. Construct acute conflict intensity at the country-year level as the
#      maximum UCDP/PRIO intensity across eligible conflicts in that year.
#   2. Construct chronic conflict temporally, using consecutive active years.
#   3. Produce a merge-ready 1970-2022 country-year panel and audit outputs.

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# Packages ---------------------------------------------------------------------
suppressPackageStartupMessages({
  library(countrycode)
  library(dplyr)
  library(readr)
  library(readxl)
  library(tidyr)
})

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

INPUT_PATH <- if (length(args) >= 1L) {
  args[[1]]
} else {
  file.path("data", "ucdp-prio-acd-231-xlsx.zip")
}

YEAR_START <- 1970L  # retained to construct t-1 predictors for 1971
YEAR_END <- 2022L    # last year in UCDP/PRIO ACD v23.1
ELIGIBLE_CONFLICT_TYPES <- c(1L, 3L, 4L)
CHRONIC_MIN_CONSECUTIVE_YEARS <- 4L  # "more than three consecutive years"

# ------------------------------------------------------------------------------
# Loading the data -------------------------------------------------------------
# ------------------------------------------------------------------------------
read_ucdp_input <- function(path) {
  extension <- tolower(tools::file_ext(path))
  
  if (extension == "zip") {
    archive <- utils::unzip(path, list = TRUE)
    candidates <- archive$Name[
      grepl("UcdpPrioConflict.*\\.xlsx$", archive$Name, ignore.case = TRUE)
    ]
    
    if (length(candidates) != 1L) {
      stop(
        "Expected exactly one UCDP/PRIO XLSX file in the ZIP; found ",
        length(candidates),
        call. = FALSE
      )
    }
    
    extraction_dir <- tempfile("ucdp_v23_1_")
    dir.create(extraction_dir)
    on.exit(unlink(extraction_dir, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(path, files = candidates, exdir = extraction_dir)
    return(readxl::read_excel(file.path(extraction_dir, candidates)))
  }
  
  if (extension %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path))
  }
  
  if (extension == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  
  stop("Input must be a ZIP, XLSX, XLS, or CSV file.", call. = FALSE)
}

ucdp_raw <- read_ucdp_input(INPUT_PATH)

required_columns <- c(
  "conflict_id", "location", "year", "intensity_level",
  "type_of_conflict", "version"
)

missing_columns <- setdiff(required_columns, names(ucdp_raw))
if (length(missing_columns) > 0L) {
  stop(
    "The UCDP input is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# Cleaning ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Filtering: Retain the pre-1970 source history temporarily so conflicts already 
# underway at the analysis boundary receive their correct duration. This also 
# prevents interstate conflicts (type 2) from contaminating country-year measures.
ucdp_typed <- ucdp_raw |>
  mutate(
    year = as.integer(year),
    conflict_id = as.integer(conflict_id),
    intensity_level = as.integer(intensity_level),
    type_of_conflict = as.integer(type_of_conflict),
    location = trimws(as.character(location))
  )

ucdp_all_eligible <- ucdp_typed |>
  filter(
    year <= YEAR_END,
    type_of_conflict %in% ELIGIBLE_CONFLICT_TYPES
  )

ucdp_eligible <- ucdp_all_eligible |>
  filter(year >= YEAR_START)

if (nrow(ucdp_eligible) == 0L) {
  stop("No eligible conflict observations remain after filtering.", call. = FALSE)
}

if (any(!ucdp_eligible$intensity_level %in% c(1L, 2L))) {
  stop("Eligible UCDP intensity_level values must be 1 or 2.", call. = FALSE)
}

# In v23.1, multi-country locations occur in interstate conflicts, which were
# filtered out above. Stop instead of silently splitting a future exception.
multi_location <- unique(
  ucdp_eligible$location[grepl(", ", ucdp_eligible$location, fixed = TRUE)]
)

# Explicit historical-name decisions -------------------------------------------
location_iso3_overrides <- c(
  "Bosnia-Herzegovina" = "BIH",
  "Cambodia (Kampuchea)" = "KHM",
  "Congo" = "COG",
  "DR Congo (Zaire)" = "COD",
  "Gambia" = "GMB",
  "Ivory Coast" = "CIV",
  "Madagascar (Malagasy)" = "MDG",
  "Myanmar (Burma)" = "MMR",
  "North Macedonia" = "MKD",
  "Russia (Soviet Union)" = "RUS",
  "Serbia (Yugoslavia)" = "SRB",
  "South Yemen" = "YEM",
  "United States of America" = "USA",
  "Yemen (North Yemen)" = "YEM",
  "Zimbabwe (Rhodesia)" = "ZWE"
)

mapped_by_countrycode <- countrycode::countrycode(
  ucdp_eligible$location,
  origin = "country.name",
  destination = "iso3c",
  warn = TRUE
)
mapped_by_override <- unname(location_iso3_overrides[ucdp_eligible$location])

ucdp_eligible$ISO_CODE_3 <- dplyr::coalesce(
  mapped_by_override,
  mapped_by_countrycode
)

unresolved_locations <- sort(unique(
  ucdp_eligible$location[is.na(ucdp_eligible$ISO_CODE_3)]
))

# Checking for duplicates ------------------------------------------------------
duplicate_conflict_years <- ucdp_eligible |>
  count(ISO_CODE_3, conflict_id, year, name = "n") |>
  filter(n > 1L)

# ------------------------------------------------------------------------------
# Acute conflict ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# A country-year takes the most severe intensity observed across all eligible
# active conflicts: 0 = none, 1 = minor conflict, 2 = war.
annual_conflict <- ucdp_eligible |>
  group_by(ISO_CODE_3, year) |>
  summarise(
    CONFLICT_INTENSITY_YEAR = max(intensity_level),
    ACUTE_CONFLICT_WAR = as.integer(any(intensity_level == 2L)),
    CONFLICT_ACTIVE = 1L,
    N_WAR_FRONTS = n_distinct(conflict_id),
    MEAN_CONFLICT_INTENSITY_ACROSS_FRONTS = mean(intensity_level),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Chronic conflict, strict same-conflict definition ----------------------------
# ------------------------------------------------------------------------------
# A country-year is chronic when at least one specific UCDP conflict_id has
# been active in the current year and each of the preceding three years. Spell
# length is calculated before the 1970 output restriction to avoid left-edge
# misclassification.
conflict_spells_all_years <- ucdp_all_eligible |>
  distinct(location, conflict_id, year, .keep_all = TRUE) |>
  arrange(location, conflict_id, year) |>
  group_by(location, conflict_id) |>
  mutate(
    NEW_CONFLICT_SPELL = is.na(dplyr::lag(year)) |
      year != dplyr::lag(year) + 1L,
    CONFLICT_SPELL_ID = cumsum(NEW_CONFLICT_SPELL)
  ) |>
  group_by(location, conflict_id, CONFLICT_SPELL_ID) |>
  mutate(
    SAME_CONFLICT_ACTIVE_RUN_YEARS = dplyr::row_number(),
    CHRONIC_SAME_CONFLICT_4PLUS = as.integer(
      SAME_CONFLICT_ACTIVE_RUN_YEARS >= CHRONIC_MIN_CONSECUTIVE_YEARS
    )
  ) |>
  ungroup()

conflict_spells <- conflict_spells_all_years |>
  dplyr::filter(year >= YEAR_START) |>
  dplyr::inner_join(
    ucdp_eligible |>
      dplyr::distinct(location, conflict_id, year, ISO_CODE_3),
    by = c("location", "conflict_id", "year"),
    relationship = "one-to-one"
  )

same_conflict_chronic <- conflict_spells |>
  dplyr::group_by(ISO_CODE_3, year) |>
  dplyr::summarise(
    SAME_CONFLICT_ACTIVE_RUN_YEARS = max(SAME_CONFLICT_ACTIVE_RUN_YEARS),
    CHRONIC_SAME_CONFLICT_4PLUS = max(CHRONIC_SAME_CONFLICT_4PLUS),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Chronic conflict, country-exposure robustness definition ---------------------
# ------------------------------------------------------------------------------
# This alternative remains one when the country experiences any eligible
# conflict for four or more uninterrupted years, even if conflict_id changes.
# It also uses the full pre-1970 source history before trimming the output.
country_spells_all_years <- ucdp_all_eligible |>
  distinct(location, year) |>
  arrange(location, year) |>
  group_by(location) |>
  mutate(
    NEW_COUNTRY_CONFLICT_SPELL = is.na(dplyr::lag(year)) |
      year != dplyr::lag(year) + 1L,
    COUNTRY_CONFLICT_SPELL_ID = cumsum(NEW_COUNTRY_CONFLICT_SPELL)
  ) |>
  group_by(location, COUNTRY_CONFLICT_SPELL_ID) |>
  mutate(
    COUNTRY_CONFLICT_ACTIVE_RUN_YEARS = dplyr::row_number(),
    CHRONIC_COUNTRY_CONFLICT_4PLUS = as.integer(
      COUNTRY_CONFLICT_ACTIVE_RUN_YEARS >= CHRONIC_MIN_CONSECUTIVE_YEARS
    )
  ) |>
  ungroup()

country_spells <- country_spells_all_years |>
  dplyr::filter(year >= YEAR_START) |>
  dplyr::inner_join(
    ucdp_eligible |>
      dplyr::distinct(location, year, ISO_CODE_3),
    by = c("location", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::group_by(ISO_CODE_3, year) |>
  dplyr::summarise(
    COUNTRY_CONFLICT_ACTIVE_RUN_YEARS = max(
      COUNTRY_CONFLICT_ACTIVE_RUN_YEARS
    ),
    CHRONIC_COUNTRY_CONFLICT_4PLUS = max(
      CHRONIC_COUNTRY_CONFLICT_4PLUS
    ),
    .groups = "drop"
  ) |>
  dplyr::select(
    ISO_CODE_3,
    year,
    COUNTRY_CONFLICT_ACTIVE_RUN_YEARS,
    CHRONIC_COUNTRY_CONFLICT_4PLUS
  )

active_country_year <- annual_conflict |>
  dplyr::left_join(
    same_conflict_chronic,
    by = c("ISO_CODE_3", "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    country_spells,
    by = c("ISO_CODE_3", "year"),
    relationship = "one-to-one"
  )

# Produce a balanced panel for every country appearing in eligible UCDP rows.
# Countries never appearing in UCDP will be assigned zeros by the merge helper.
location_lookup <- ucdp_eligible |>
  group_by(ISO_CODE_3) |>
  summarise(
    UCDP_LOCATION_NAMES = paste(sort(unique(location)), collapse = " | "),
    .groups = "drop"
  )

conflict_panel <- tidyr::expand_grid(
  ISO_CODE_3 = sort(unique(ucdp_eligible$ISO_CODE_3)),
  YEAR = seq.int(YEAR_START, YEAR_END)
) |>
  dplyr::left_join(location_lookup, by = "ISO_CODE_3") |>
  dplyr::left_join(
    active_country_year |>
      dplyr::rename(YEAR = year),
    by = c("ISO_CODE_3", "YEAR"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        CONFLICT_INTENSITY_YEAR,
        ACUTE_CONFLICT_WAR,
        CONFLICT_ACTIVE,
        N_WAR_FRONTS,
        MEAN_CONFLICT_INTENSITY_ACROSS_FRONTS,
        SAME_CONFLICT_ACTIVE_RUN_YEARS,
        CHRONIC_SAME_CONFLICT_4PLUS,
        COUNTRY_CONFLICT_ACTIVE_RUN_YEARS,
        CHRONIC_COUNTRY_CONFLICT_4PLUS
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    CHRONIC_CONFLICT_4PLUS = CHRONIC_SAME_CONFLICT_4PLUS
  ) |>
  dplyr::arrange(ISO_CODE_3, YEAR)

# ------------------------------------------------------------------------------
# Validation -------------------------------------------------------------------
# ------------------------------------------------------------------------------
if (anyDuplicated(conflict_panel[c("ISO_CODE_3", "YEAR")]) > 0L) {
  stop("The output is not unique by ISO_CODE_3 and YEAR.", call. = FALSE)
}

if (!all(conflict_panel$CONFLICT_INTENSITY_YEAR %in% 0:2)) {
  stop("Acute conflict intensity must be 0, 1, or 2.", call. = FALSE)
}

binary_variables <- c(
  "ACUTE_CONFLICT_WAR",
  "CONFLICT_ACTIVE",
  "CHRONIC_SAME_CONFLICT_4PLUS",
  "CHRONIC_COUNTRY_CONFLICT_4PLUS",
  "CHRONIC_CONFLICT_4PLUS"
)

if (!all(vapply(
  conflict_panel[binary_variables],
  function(x) all(x %in% 0:1),
  logical(1)
))) {
  stop("At least one binary conflict variable contains values outside 0/1.")
}

if (!all(
  conflict_panel$CONFLICT_ACTIVE ==
  as.integer(conflict_panel$CONFLICT_INTENSITY_YEAR > 0L)
)) {
  stop("Conflict activity and acute intensity are inconsistent.")
}

if (any(
  conflict_panel$CHRONIC_SAME_CONFLICT_4PLUS >
  conflict_panel$CHRONIC_COUNTRY_CONFLICT_4PLUS
)) {
  stop("Same-conflict chronic exposure must imply country-level chronic exposure.")
}

if (any(
  conflict_panel$CHRONIC_CONFLICT_4PLUS == 1L &
  conflict_panel$SAME_CONFLICT_ACTIVE_RUN_YEARS <
  CHRONIC_MIN_CONSECUTIVE_YEARS
)) {
  stop("The primary chronic indicator violates its duration rule.")
}

# Audit the effect of replacing arbitrary first-row intensity with the maximum.
acute_order_diagnostic <- ucdp_eligible |>
  dplyr::group_by(ISO_CODE_3, year) |>
  dplyr::summarise(
    FIRST_ROW_INTENSITY = dplyr::first(intensity_level),
    MAX_INTENSITY = max(intensity_level),
    N_WAR_FRONTS = dplyr::n_distinct(conflict_id),
    FIRST_DIFFERS_FROM_MAX = as.integer(FIRST_ROW_INTENSITY != MAX_INTENSITY),
    .groups = "drop"
  )

source_versions <- paste(
  sort(unique(stats::na.omit(as.character(ucdp_raw$version)))),
  collapse = " | "
)

audit_summary <- data.frame(
  item = c(
    "input_path",
    "input_md5",
    "source_version",
    "year_window",
    "eligible_conflict_types",
    "raw_rows",
    "rows_in_year_window",
    "eligible_conflict_rows",
    "active_country_years",
    "multi_front_country_years",
    "first_row_intensity_differs_from_max",
    "primary_chronic_country_years",
    "country_exposure_chronic_country_years",
    "output_country_years"
  ),
  value = c(
    normalizePath(INPUT_PATH, winslash = "/", mustWork = TRUE),
    unname(tools::md5sum(INPUT_PATH)),
    source_versions,
    paste0(YEAR_START, "-", YEAR_END),
    paste(ELIGIBLE_CONFLICT_TYPES, collapse = ","),
    nrow(ucdp_raw),
    sum(ucdp_typed$year >= YEAR_START & ucdp_typed$year <= YEAR_END),
    nrow(ucdp_eligible),
    nrow(annual_conflict),
    sum(annual_conflict$N_WAR_FRONTS > 1L),
    sum(acute_order_diagnostic$FIRST_DIFFERS_FROM_MAX),
    sum(conflict_panel$CHRONIC_CONFLICT_4PLUS),
    sum(conflict_panel$CHRONIC_COUNTRY_CONFLICT_4PLUS),
    nrow(conflict_panel)
  ),
  stringsAsFactors = FALSE
)

# Final preparation for merge
conflict_panel <- conflict_panel |>
  mutate(YEAR = as.character(YEAR))

# Saving the data --------------------------------------------------------------
save(conflict_panel, file = "ucdp_chronic_clean.RDS")