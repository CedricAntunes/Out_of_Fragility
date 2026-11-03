# ==============================================================================
# 09_ucdp_war.R: country-year WAR indicators (UCDP/PRIO Armed Conflict v23.1) --
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
#
# DEFINITION (annual, not lagged):
#   WAR = 1 if, in that country-year, there is a civil conflict (type 3 or 4)
#   at intensity 2 (>= 1,000 battle deaths), or the country is a primary party
#   to an interstate conflict (type 2) at any intensity (>= 25 deaths).
#   Type 1 (extrasystemic) is excluded. Thresholds apply per conflict-year.
#   WAR_3PLUS / WAR_5PLUS: third / fifth consecutive WAR year onward.
#   WAR_RECENT3 / WAR_RECENT5: any WAR in t-2..t / t-4..t (the hazard pipeline's
#   one-year lag turns these into t-3..t-1 / t-5..t-1).
# Country assignment splits UCDP's location field (the country hosting an
# internal conflict and all primary state parties to an interstate conflict).
#
# Mapping notes:
# Historical mappings: Serbia (Yugoslavia) -> SRB, Russia (Soviet Union) -> RUS,
# South Vietnam -> VNM, South Yemen -> YEM.
#
# Output: data/intermediate/ucdp_war.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

INPUT_PATH <- raw_path("ucdp", "ucdp-prio-acd-231-xlsx.zip")
WAR_OUTPUT_START <- 1970L        # output starts one year before the analysis window
HISTORY_START <- WAR_OUTPUT_START - 5L
CIVIL_WAR_TYPES <- c(3L, 4L)
INTERSTATE_INTENSITIES <- c(1L, 2L)
# Reading the original data -----------------------------------------------------
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
# Include zero-conflict countries, not just countries ever observed at war.
# XKX (Kosovo) is added explicitly because countrycode's list lacks it.
country_roster <- sort(unique(c(
  as.character(countrycode::codelist$iso3c),
  ucdp_country_conflicts$ISO_CODE_3,
  "XKX"
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
  filter(YEAR >= WAR_OUTPUT_START) |>
  select(ISO_CODE_3, YEAR, WAR, WAR_3PLUS, WAR_5PLUS, WAR_RECENT3, WAR_RECENT5) |>
  identity() |>
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

save_intermediate(war_panel, "ucdp_war")
