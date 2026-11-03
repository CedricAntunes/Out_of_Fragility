# ==============================================================================
# 10_ucdp_chronic.R: acute and chronic conflict, episode endings (UCDP/PRIO v23.1)
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
#
# Coding notes:
# Eligible conflicts: types 1, 3, 4 (extrasystemic, internal, internationalized
# internal); interstate conflicts are handled in 20_ucdp_war.R.
#   CONFLICT_INTENSITY_YEAR  max intensity across eligible conflicts (0/1/2)
#   ACUTE_CONFLICT_WAR       1 if any eligible conflict at intensity 2
#   CONFLICT_ACTIVE          1 if any eligible conflict is active
#   CONFLICT_EPISODE_END     1 if a UCDP episode ends (ep_end) in the year
#   WAR_SHORT / WAR_CHRONIC  war-level conflict in its 1st-3rd / 4th+
#                            consecutive country year (mutually exclusive)
#   CONFLICT_EPISODE_END_RECENT3/5/7, POST_CONFLICT_3/5/7
#
# Output: data/intermediate/ucdp_chronic.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

INPUT_PATH <- raw_path("ucdp", "ucdp-prio-acd-231-xlsx.zip")
CHRONIC_OUTPUT_START <- 1970L  # retained to construct t-1 predictors for 1971
ELIGIBLE_CONFLICT_TYPES <- c(1L, 3L, 4L)
CHRONIC_MIN_CONSECUTIVE_YEARS <- 4L  # "more than three consecutive years"

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
  "type_of_conflict", "ep_end", "version"
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
    ep_end = as.integer(ep_end),
    location = trimws(as.character(location))
  )

ucdp_all_eligible <- ucdp_typed |>
  filter(
    year <= YEAR_END,
    type_of_conflict %in% ELIGIBLE_CONFLICT_TYPES
  )

if (any(!ucdp_all_eligible$intensity_level %in% c(1L, 2L))) {
  stop(
    "Eligible UCDP intensity_level values (pre-1970 extension) must be 1 or 2.",
    call. = FALSE
  )
}

# War-only slice for chronic conflict. Chronic conflict is defined as
# consecutive years at UCDP war-level intensity (intensity_level == 2),
# matching the manuscript's "acute war for four or more uninterrupted
# years" definition. Built from ucdp_all_eligible (not ucdp_eligible) so
# the pre-1970 history is available for left-edge spell continuity, the
# same rationale as the unfiltered spell frames below.
ucdp_all_eligible_war <- ucdp_all_eligible |>
  filter(intensity_level == 2L)

ucdp_eligible <- ucdp_all_eligible |>
  filter(year >= CHRONIC_OUTPUT_START)

if (nrow(ucdp_eligible) == 0L) {
  stop("No eligible conflict observations remain after filtering.", call. = FALSE)
}

if (any(!ucdp_eligible$intensity_level %in% c(1L, 2L))) {
  stop("Eligible UCDP intensity_level values must be 1 or 2.", call. = FALSE)
}

if (any(!ucdp_eligible$ep_end %in% c(0L, 1L))) {
  stop("Eligible UCDP ep_end values must be 0 or 1.", call. = FALSE)
}

# In v23.1, multi-country locations occur in interstate conflicts, which were
# filtered out above. Stop instead of silently splitting a future exception.
multi_location <- unique(
  ucdp_eligible$location[grepl(", ", ucdp_eligible$location, fixed = TRUE)]
)

if (length(multi_location) > 0L) {
  stop(
    "Eligible UCDP rows contain multi-country locations: ",
    paste(sort(multi_location), collapse = "; "),
    call. = FALSE
  )
}

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
  custom_match = location_iso3_overrides,
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

if (length(unresolved_locations) > 0L) {
  stop(
    "Could not map these UCDP locations to ISO3 codes: ",
    paste(unresolved_locations, collapse = "; "),
    call. = FALSE
  )
}

# Checking for duplicates ------------------------------------------------------
duplicate_conflict_years <- ucdp_eligible |>
  count(ISO_CODE_3, conflict_id, year, name = "n") |>
  filter(n > 1L)

if (nrow(duplicate_conflict_years) > 0L) {
  stop("Duplicate ISO-conflict-year rows remain after cleaning.", call. = FALSE)
}

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
    CONFLICT_EPISODE_END = as.integer(any(ep_end == 1L, na.rm = TRUE)),
    N_WAR_FRONTS = n_distinct(conflict_id),
    MEAN_CONFLICT_INTENSITY_ACROSS_FRONTS = mean(intensity_level),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Chronic conflict, strict same-conflict definition ----------------------------
# ------------------------------------------------------------------------------
# A country-year is chronic when at least one specific UCDP conflict_id has
# been active AT WAR-LEVEL INTENSITY (intensity_level == 2) in the current
# year and each of the preceding three years. A year at minor-conflict
# intensity (1) for that conflict_id breaks the spell, since it is absent
# from ucdp_all_eligible_war and therefore reads as a gap in the year
# sequence below. Spell length is calculated before the 1970 output
# restriction to avoid left-edge misclassification.
conflict_spells_all_years <- ucdp_all_eligible_war |>
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
  dplyr::filter(year >= CHRONIC_OUTPUT_START) |>
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
# Chronic conflict, country-exposure main definition ---------------------------
# ------------------------------------------------------------------------------
# This measure remains one when the country experiences WAR-LEVEL conflict
# (intensity_level == 2) for four or more uninterrupted years, even if
# conflict_id changes; a year with only minor-intensity conflict (1), like a
# year with none, breaks the spell. It also uses the full pre-1970 source
# history before trimming the output.
country_spells_all_years <- ucdp_all_eligible_war |>
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
  dplyr::filter(year >= CHRONIC_OUTPUT_START) |>
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

# ------------------------------------------------------------------------------
# Recent episode endings and post-conflict windows -----------------------------
# ------------------------------------------------------------------------------
# ep_end identifies the final year of a UCDP conflict episode. It does not
# distinguish peace agreements, ceasefires, and military victories. The
# POST_CONFLICT indicators therefore capture a recent episode ending followed
# by a country-year with no remaining eligible conflict.
recent_any_window <- function(x, window) {
  recent <- x == 1L
  if (window > 1L) {
    for (j in seq_len(window - 1L)) {
      recent <- recent | dplyr::lag(x, n = j, default = 0L) == 1L
    }
  }
  as.integer(recent)
}

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
  YEAR = seq.int(CHRONIC_OUTPUT_START, YEAR_END)
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
        CONFLICT_EPISODE_END,
        N_WAR_FRONTS,
        MEAN_CONFLICT_INTENSITY_ACROSS_FRONTS,
        SAME_CONFLICT_ACTIVE_RUN_YEARS,
        CHRONIC_SAME_CONFLICT_4PLUS,
        COUNTRY_CONFLICT_ACTIVE_RUN_YEARS,
        CHRONIC_COUNTRY_CONFLICT_4PLUS
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    # Main chronic measure: uninterrupted country exposure to WAR-LEVEL
    # conflict (intensity_level == 2), regardless of whether UCDP assigns a
    # new conflict_id during the spell.
    CHRONIC_CONFLICT_4PLUS = CHRONIC_COUNTRY_CONFLICT_4PLUS,
    # War-duration recode, against a shared "no war" reference (ACUTE_CONFLICT_WAR
    # == 0). CHRONIC_CONFLICT_4PLUS == 1 implies ACUTE_CONFLICT_WAR == 1 by
    # construction (chronicity requires war-level intensity in the current
    # year), so entering ACUTE_CONFLICT_WAR and CHRONIC_CONFLICT_4PLUS together
    # additively makes the chronic coefficient an increment relative to short
    # war, not relative to peace. WAR_SHORT and WAR_CHRONIC are mutually
    # exclusive and both compare directly to the same no-war reference.
    WAR_SHORT = as.integer(
      ACUTE_CONFLICT_WAR == 1L & CHRONIC_CONFLICT_4PLUS == 0L
    ),
    WAR_CHRONIC = as.integer(CHRONIC_CONFLICT_4PLUS == 1L)
  ) |>
  dplyr::group_by(ISO_CODE_3) |>
  dplyr::arrange(YEAR, .by_group = TRUE) |>
  dplyr::mutate(
    CONFLICT_EPISODE_END_RECENT3 = recent_any_window(CONFLICT_EPISODE_END, 3L),
    CONFLICT_EPISODE_END_RECENT5 = recent_any_window(CONFLICT_EPISODE_END, 5L),
    CONFLICT_EPISODE_END_RECENT7 = recent_any_window(CONFLICT_EPISODE_END, 7L),
    POST_CONFLICT_3 = as.integer(
      CONFLICT_EPISODE_END_RECENT3 == 1L & CONFLICT_ACTIVE == 0L
    ),
    POST_CONFLICT_5 = as.integer(
      CONFLICT_EPISODE_END_RECENT5 == 1L & CONFLICT_ACTIVE == 0L
    ),
    POST_CONFLICT_7 = as.integer(
      CONFLICT_EPISODE_END_RECENT7 == 1L & CONFLICT_ACTIVE == 0L
    )
  ) |>
  dplyr::ungroup() |>
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
  "CONFLICT_EPISODE_END",
  "CHRONIC_SAME_CONFLICT_4PLUS",
  "CHRONIC_COUNTRY_CONFLICT_4PLUS",
  "CHRONIC_CONFLICT_4PLUS",
  "WAR_SHORT",
  "WAR_CHRONIC",
  "CONFLICT_EPISODE_END_RECENT3",
  "CONFLICT_EPISODE_END_RECENT5",
  "CONFLICT_EPISODE_END_RECENT7",
  "POST_CONFLICT_3",
  "POST_CONFLICT_5",
  "POST_CONFLICT_7"
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
  conflict_panel$COUNTRY_CONFLICT_ACTIVE_RUN_YEARS <
  CHRONIC_MIN_CONSECUTIVE_YEARS
)) {
  stop("The primary chronic indicator violates its duration rule.")
}

if (any(conflict_panel$WAR_SHORT == 1L & conflict_panel$WAR_CHRONIC == 1L)) {
  stop("WAR_SHORT and WAR_CHRONIC are not mutually exclusive.")
}

if (!all(
  (conflict_panel$WAR_SHORT + conflict_panel$WAR_CHRONIC) ==
  conflict_panel$ACUTE_CONFLICT_WAR
)) {
  stop("WAR_SHORT and WAR_CHRONIC do not exactly partition ACUTE_CONFLICT_WAR.")
}

if (any(
  conflict_panel$POST_CONFLICT_3 == 1L &
  (conflict_panel$CONFLICT_ACTIVE == 1L |
   conflict_panel$CONFLICT_EPISODE_END_RECENT3 == 0L)
) || any(
  conflict_panel$POST_CONFLICT_5 == 1L &
  (conflict_panel$CONFLICT_ACTIVE == 1L |
   conflict_panel$CONFLICT_EPISODE_END_RECENT5 == 0L)
) || any(
  conflict_panel$POST_CONFLICT_7 == 1L &
  (conflict_panel$CONFLICT_ACTIVE == 1L |
   conflict_panel$CONFLICT_EPISODE_END_RECENT7 == 0L)
)) {
  stop("At least one post-conflict indicator violates its definition.")
}

# Diagnostic only: chronic country-years under the PRIOR (any-eligible-
# intensity) definition, computed independently of conflict_panel, for
# comparison against the corrected war-level CHRONIC_CONFLICT_4PLUS above.
# This block feeds only audit_summary; it does not touch conflict_panel.
chronic_any_intensity_diagnostic <- ucdp_all_eligible |>
  distinct(location, year) |>
  arrange(location, year) |>
  group_by(location) |>
  mutate(
    NEW_SPELL_ANY = is.na(dplyr::lag(year)) | year != dplyr::lag(year) + 1L,
    SPELL_ID_ANY = cumsum(NEW_SPELL_ANY)
  ) |>
  group_by(location, SPELL_ID_ANY) |>
  mutate(
    RUN_YEARS_ANY = dplyr::row_number(),
    CHRONIC_ANY = as.integer(RUN_YEARS_ANY >= CHRONIC_MIN_CONSECUTIVE_YEARS)
  ) |>
  ungroup() |>
  filter(year >= CHRONIC_OUTPUT_START) |>
  inner_join(
    ucdp_eligible |> distinct(location, year, ISO_CODE_3),
    by = c("location", "year"),
    relationship = "one-to-one"
  ) |>
  group_by(ISO_CODE_3, year) |>
  summarise(CHRONIC_ANY = max(CHRONIC_ANY), .groups = "drop")

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
    "primary_chronic_country_years_prior_any_intensity_definition",
    "war_short_country_years",
    "war_chronic_country_years",
    "strict_same_conflict_chronic_country_years",
    "post_conflict_3_country_years",
    "post_conflict_5_country_years",
    "post_conflict_7_country_years",
    "output_country_years"
  ),
  value = c(
    normalizePath(INPUT_PATH, winslash = "/", mustWork = TRUE),
    unname(tools::md5sum(INPUT_PATH)),
    source_versions,
    paste0(CHRONIC_OUTPUT_START, "-", YEAR_END),
    paste(ELIGIBLE_CONFLICT_TYPES, collapse = ","),
    nrow(ucdp_raw),
    sum(ucdp_typed$year >= CHRONIC_OUTPUT_START & ucdp_typed$year <= YEAR_END),
    nrow(ucdp_eligible),
    nrow(annual_conflict),
    sum(annual_conflict$N_WAR_FRONTS > 1L),
    sum(acute_order_diagnostic$FIRST_DIFFERS_FROM_MAX),
    sum(conflict_panel$CHRONIC_CONFLICT_4PLUS),
    sum(chronic_any_intensity_diagnostic$CHRONIC_ANY),
    sum(conflict_panel$WAR_SHORT),
    sum(conflict_panel$WAR_CHRONIC),
    sum(conflict_panel$CHRONIC_SAME_CONFLICT_4PLUS),
    sum(conflict_panel$POST_CONFLICT_3),
    sum(conflict_panel$POST_CONFLICT_5),
    sum(conflict_panel$POST_CONFLICT_7),
    nrow(conflict_panel)
  ),
  stringsAsFactors = FALSE
)


conflict_panel <- conflict_panel |> select(-any_of("UCDP_LOCATION_NAMES"))
save_intermediate(conflict_panel, "ucdp_chronic")
