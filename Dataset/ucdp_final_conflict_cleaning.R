# UCDP/PRIO: one country-year WAR indicator -------------------------------------
# Prepared for Cedric Antunes (FGV-CEPESP), September 2026
# Standalone replacement: does not require running ucdp_final_cleaning(1).R.
# Requires the FULL UCDP/PRIO Armed Conflict Dataset v23.1 (1946-2022),
# not the already-filtered output of the previous cleaning pipeline.
#
# DEFINITION (annual, not lagged):
# WAR = 1 if at least one of the following holds in that country-year:
#   - Civil war: type 3 or 4 AND intensity_level == 2 (>=1,000 deaths).
#   - Interstate conflict: type 2, at either recorded intensity (>=25 deaths).
# WAR = 0 otherwise, within 1970-2022.
# Type 1 (extrasystemic/colonial conflict) is excluded.
# Thresholds apply to each UCDP conflict-year, not summed country casualties.
# WAR_3PLUS = 1 from the third consecutive WAR == 1 year onward.
# WAR_5PLUS = 1 from the fifth consecutive WAR == 1 year onward.
# Both include the current year and reset to zero whenever WAR == 0.
# Continuity is at the ISO3 country level: conflict IDs and qualifying conflict
# types may change without breaking a run. These indicators are not exclusive.
# WAR_RECENT3 = 1 if WAR == 1 in any of t, t-1, or t-2; zero otherwise.
# WAR_RECENT5 = 1 if WAR == 1 in any of t through t-4; zero otherwise.
# These windows INCLUDE year t and do not require consecutive war years.
# They replace the prior WAR_PREV3 / WAR_PREV5 columns to accommodate the
# hazard pipeline's automatic one-year lag. Do not pre-lag these inputs here.
#
# AFTER THE HAZARD PIPELINE LAGS EACH VARIABLE ONCE, for outcome year t:
#   WAR_L1: WAR in t-1.
#   WAR_3PLUS_L1: WAR in every year from t-3 through t-1.
#   WAR_5PLUS_L1: WAR in every year from t-5 through t-1.
#   WAR_RECENT3_L1: WAR in ANY year from t-3 through t-1.
#   WAR_RECENT5_L1: WAR in ANY year from t-5 through t-1.
# Suffixes above illustrate timing; the hazard pipeline creates the lagged
# columns. Lag within country on an ordered, consecutive annual panel BEFORE
# restricting to the hazard risk set, so a row lag equals one calendar year.
# Output starts in 1970, supplying t-1 inputs for hazard outcomes from 1971.
# Five pre-1970 years are retained during construction, providing sufficient
# history for every indicator in the first output year.
#
# COUNTRY ASSIGNMENT:
# Split UCDP's location field: it identifies the country experiencing an
# internal conflict and ALL PRIMARY state parties to an interstate conflict.
# Foreign secondary interveners are not assigned WAR merely for intervening.
# Historical ISO3 mappings follow the supplied pipeline, including
# Serbia (Yugoslavia) -> SRB and Russia (Soviet Union) -> RUS. These are
# inherited coding conventions, not new claims about state continuity.
# South Vietnam is additionally mapped to VNM: its records are combined under
# Vietnam's ISO3 code rather than retained as a separate historical state.
#
# SOURCE / DEFINITIONS:
# https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-231.pdf
# Pettersson (2023), UCDP/PRIO Armed Conflict Dataset Codebook v23.1,
# intensity_level, type_of_conflict, and section 4.1 (Location).
#
# RUN in RStudio: edit INPUT_PATH below, then source this entire file.
# Or from a terminal:
# Rscript ucdp_WAR_cleaning.R data/ucdp-prio-acd-231-xlsx.zip output
#
# OUTPUT: war_panel, with ISO_CODE_3, YEAR (character), and five integer
# indicators: WAR, WAR_3PLUS, WAR_5PLUS, WAR_RECENT3, WAR_RECENT5.
# Files: ucdp_WAR_clean.rds (read with readRDS) and ucdp_WAR_clean.csv.
# The country roster includes ISO3 countries that never appear in UCDP.
# This balanced lookup is NOT a state-existence or estimation-sample roster:
# left-join it to your existing country-year sample using merge_war() below.

# Packages ---------------------------------------------------------------------
# Install once if needed:
# install.packages(c("countrycode", "dplyr", "readr", "readxl", "tidyr"))
suppressPackageStartupMessages({
  library(countrycode)
  library(dplyr)
  library(readr)
  library(readxl)
  library(tidyr)
})

# Parameters -------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
INPUT_PATH <- if (length(args) >= 1L) args[[1]] else
  file.path("data", "ucdp-prio-acd-231-xlsx.zip")
OUTPUT_DIR <- if (length(args) >= 2L) args[[2]] else "."
YEAR_START <- 1970L
YEAR_END <- 2022L
HISTORY_START <- YEAR_START - 5L  # includes t-5 for the first output year
CIVIL_WAR_TYPES <- c(3L, 4L)
INTERSTATE_INTENSITIES <- c(1L, 2L)
# If you intend INTERSTATE WAR (>=1,000 deaths) rather than any interstate
# conflict, change the preceding line to: INTERSTATE_INTENSITIES <- 2L

# Read the original data --------------------------------------------------------
read_ucdp_input <- function(path) {
  if (!file.exists(path)) stop("UCDP input not found: ", path, call. = FALSE)
  extension <- tolower(tools::file_ext(path))
  if (extension == "zip") {
    archive <- utils::unzip(path, list = TRUE)
    candidates <- archive$Name[
      grepl("UcdpPrioConflict.*\\.xlsx$", archive$Name, ignore.case = TRUE)
    ]
    if (length(candidates) != 1L) {
      stop("Expected exactly one UcdpPrioConflict XLSX in the ZIP; found ",
           length(candidates), call. = FALSE)
    }
    extraction_dir <- tempfile("ucdp_war_")
    dir.create(extraction_dir)
    on.exit(unlink(extraction_dir, recursive = TRUE), add = TRUE)
    utils::unzip(path, files = candidates, exdir = extraction_dir)
    return(readxl::read_excel(file.path(extraction_dir, candidates)))
  }
  if (extension %in% c("xlsx", "xls")) return(readxl::read_excel(path))
  if (extension == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  stop("Input must be ZIP, XLSX, XLS, or CSV.", call. = FALSE)
}

ucdp_raw <- read_ucdp_input(INPUT_PATH)
required_columns <- c("conflict_id", "location", "year", "intensity_level",
                      "type_of_conflict", "version")
missing_columns <- setdiff(required_columns, names(ucdp_raw))
if (length(missing_columns) > 0L) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "),
       call. = FALSE)
}
if (anyNA(ucdp_raw$version) ||
    !all(as.character(ucdp_raw$version) == "23.1")) {
  stop("This script expects UCDP/PRIO ACD v23.1; check source coverage before ",
       "using another release.", call. = FALSE)
}

# Reject malformed codes instead of turning missing observations into peace.
to_integer <- function(x, field) {
  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  if (any(!is.finite(numeric_x)) || any(numeric_x != trunc(numeric_x))) {
    stop("Missing or non-integer values in ", field, call. = FALSE)
  }
  as.integer(numeric_x)
}
ucdp_typed <- ucdp_raw |>
  mutate(
    year = to_integer(year, "year"),
    type_of_conflict = to_integer(type_of_conflict, "type_of_conflict"),
    intensity_level = to_integer(intensity_level, "intensity_level"),
    location = trimws(as.character(location))
  )
if (!all(ucdp_typed$type_of_conflict %in% 1:4) ||
    !all(ucdp_typed$intensity_level %in% 1:2)) {
  stop("Unexpected UCDP conflict type or intensity code.", call. = FALSE)
}
if (min(ucdp_typed$year) > HISTORY_START || max(ucdp_typed$year) < YEAR_END ||
    !any(ucdp_typed$type_of_conflict == 2L)) {
  stop("Use the full UCDP input, including interstate conflicts and the ",
       "entire requested year window plus five preceding years.", call. = FALSE)
}

# Keep civil and interstate conflict rows; split multi-country locations ---------
ucdp_country_conflicts <- ucdp_typed |>
  filter(year >= HISTORY_START, year <= YEAR_END,
         type_of_conflict %in% c(2L, CIVIL_WAR_TYPES)) |>
  select(conflict_id, location, year, type_of_conflict, intensity_level) |>
  tidyr::separate_rows(location, sep = ",\\s*") |>
  mutate(location = trimws(location))
if (anyNA(ucdp_country_conflicts$location) ||
    any(ucdp_country_conflicts$location == "")) {
  stop("Missing country location in an eligible UCDP row.", call. = FALSE)
}

# Preserve the historical-name decisions in the supplied script -----------------
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
  "South Vietnam" = "VNM",
  "South Yemen" = "YEM",
  "United States of America" = "USA",
  "Yemen (North Yemen)" = "YEM",
  "Zimbabwe (Rhodesia)" = "ZWE"
)
ucdp_country_conflicts$ISO_CODE_3 <- countrycode::countrycode(
  ucdp_country_conflicts$location,
  origin = "country.name", destination = "iso3c",
  custom_match = location_iso3_overrides, warn = TRUE
)
unresolved <- sort(unique(ucdp_country_conflicts$location[
  is.na(ucdp_country_conflicts$ISO_CODE_3)
]))
if (length(unresolved) > 0L) {
  stop("Unmapped locations: ", paste(unresolved, collapse = "; "),
       ". Add explicit ISO3 mappings before continuing.", call. = FALSE)
}

# Annual logical OR across all conflicts in each country ------------------------
annual_war <- ucdp_country_conflicts |>
  mutate(
    qualifies = (type_of_conflict %in% CIVIL_WAR_TYPES & intensity_level == 2L) |
      (type_of_conflict == 2L & intensity_level %in% INTERSTATE_INTENSITIES)
  ) |>
  group_by(ISO_CODE_3, year) |>
  summarise(WAR = as.integer(any(qualifies)), .groups = "drop") |>
  rename(YEAR = year)

# Include zero-conflict countries, not just countries ever observed at war.
country_roster <- sort(unique(c(
  as.character(countrycode::codelist$iso3c),
  ucdp_country_conflicts$ISO_CODE_3
)))
country_roster <- country_roster[!is.na(country_roster) & country_roster != ""]

# Consecutive WAR years, including the current year -----------------------------
# Apply only after completing the annual grid and sorting within each country.
# A zero breaks the run. An internal missing source year is represented by zero
# under the same complete-source assumption used to construct WAR.
consecutive_war_years <- function(war) {
  stopifnot(!anyNA(war), all(war %in% 0:1))
  run <- integer(length(war))
  previous <- 0L
  for (i in seq_along(war)) {
    previous <- if (war[[i]] == 1L) previous + 1L else 0L
    run[[i]] <- previous
  }
  run
}

# Any WAR in a window ending in the CURRENT year -------------------------------
# Used within each country on the complete, ordered annual grid below.
# Offsets 0:(window-1) anticipate the hazard pipeline's additional one-year lag.
# Lag padding only affects the initial history rows, which are discarded.
any_war_recent <- function(war, window) {
  stopifnot(!anyNA(war), all(war %in% 0:1),
            length(window) == 1L, !is.na(window), window >= 1L,
            window == as.integer(window))
  recent <- rep(FALSE, length(war))
  for (j in seq.int(0L, window - 1L)) {
    recent <- recent | dplyr::lag(war, n = j, default = 0L) == 1L
  }
  as.integer(recent)
}

war_panel <- tidyr::expand_grid(
  ISO_CODE_3 = country_roster,
  YEAR = seq.int(HISTORY_START, YEAR_END)
) |>
  left_join(annual_war, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(WAR = tidyr::replace_na(WAR, 0L)) |>
  group_by(ISO_CODE_3) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(
    WAR_RUN_YEARS = consecutive_war_years(WAR),
    WAR_3PLUS = as.integer(WAR_RUN_YEARS >= 3L),
    WAR_5PLUS = as.integer(WAR_RUN_YEARS >= 5L),
    WAR_RECENT3 = any_war_recent(WAR, 3L),
    WAR_RECENT5 = any_war_recent(WAR, 5L)
  ) |>
  ungroup() |>
  # Trim AFTER computing all indicators so 1970 has history back through 1965.
  filter(YEAR >= YEAR_START) |>
  select(ISO_CODE_3, YEAR, WAR, WAR_3PLUS, WAR_5PLUS, WAR_RECENT3, WAR_RECENT5) |>
  mutate(YEAR = as.character(YEAR)) |>
  arrange(ISO_CODE_3, YEAR)

war_variables <- c("WAR", "WAR_3PLUS", "WAR_5PLUS", "WAR_RECENT3", "WAR_RECENT5")
stopifnot(
  identical(names(war_panel), c("ISO_CODE_3", "YEAR", war_variables)),
  anyDuplicated(war_panel[c("ISO_CODE_3", "YEAR")]) == 0L,
  all(vapply(war_panel[war_variables],
             function(x) is.integer(x) && all(x %in% 0:1), logical(1))),
  all(war_panel$WAR_5PLUS <= war_panel$WAR_3PLUS),
  all(war_panel$WAR_3PLUS <= war_panel$WAR),
  all(war_panel$WAR_RECENT3 <= war_panel$WAR_RECENT5),
  all(war_panel$WAR <= war_panel$WAR_RECENT3)
)

# Optional merge into the existing sample; retains its rows and YEAR type --------
# Example (after sourcing this file):
# standardized_data_final <- merge_war(standardized_data_final)
# Uncovered years or unknown country codes cause an error, not a false zero.
merge_war <- function(data, lookup = war_panel) {
  indicators <- c("WAR", "WAR_3PLUS", "WAR_5PLUS", "WAR_RECENT3", "WAR_RECENT5")
  if (!all(c("ISO_CODE_3", "YEAR", indicators) %in% names(lookup))) {
    stop("Lookup must include the keys and all five WAR indicators.",
         call. = FALSE)
  }
  if (!all(c("ISO_CODE_3", "YEAR") %in% names(data))) {
    stop("Target data must contain ISO_CODE_3 and YEAR.", call. = FALSE)
  }
  if (anyDuplicated(lookup[c("ISO_CODE_3", "YEAR")]) > 0L) {
    stop("WAR lookup has duplicate country-years.", call. = FALSE)
  }
  key <- paste(as.character(data$ISO_CODE_3), as.character(data$YEAR), sep = "|")
  lookup_key <- paste(lookup$ISO_CODE_3, lookup$YEAR, sep = "|")
  index <- match(key, lookup_key)
  if (anyNA(data$ISO_CODE_3) || anyNA(data$YEAR) || anyNA(index)) {
    stop("Missing or unmatched country-year keys. Check ISO3 codes and ",
         "1970-2022 coverage before merging.", call. = FALSE)
  }
  for (variable in indicators) data[[variable]] <- lookup[[variable]][index]
  data
}

# Save -------------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
saveRDS(war_panel, file.path(OUTPUT_DIR, "ucdp_WAR_clean.rds"))
save(war_panel, file = "ucdp_war_clean.RDS")
readr::write_csv(war_panel, file.path(OUTPUT_DIR, "ucdp_WAR_clean.csv"))
message("Saved ", nrow(war_panel), " country-years; ", sum(war_panel$WAR),
        " coded WAR = 1; ", sum(war_panel$WAR_3PLUS), " coded WAR_3PLUS = 1; ",
        sum(war_panel$WAR_5PLUS), " coded WAR_5PLUS = 1; ",
        sum(war_panel$WAR_RECENT3), " coded WAR_RECENT3 = 1; ",
        sum(war_panel$WAR_RECENT5), " coded WAR_RECENT5 = 1.")