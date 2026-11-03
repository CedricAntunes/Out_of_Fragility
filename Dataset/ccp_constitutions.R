# CCP CHRONOLOGY V6: NEW-CONSTITUTION COUNTRY-YEAR CLEANING PIPELINE -----------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: August, 2026 -----------------------------------------------------------

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# ------------------------------------------------------------------------------
# USER SETTINGS ----------------------------------------------------------------
# ------------------------------------------------------------------------------
# Raw CCP version 6
zip_file <- "D:/Users/cedric/Downloads/ccpcce_v6.zip"

# Main analytical dataset
main_panel_file <- "D:/Fragility-Exit/standardized_data_final.RDS"

# Requested analytical period
analysis_start_year <- 1971L
analysis_end_year   <- 2022L

# Name used when the main-panel file was created with save().
expected_main_panel_object <- "standardized_data_final"

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(countrycode)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
read_single_data_frame <- function(path, expected_object = NULL) {
  rds_object <- tryCatch(
    readRDS(path),
    error = function(e) NULL
  )
  
  if (inherits(rds_object, "data.frame")) {
    return(rds_object)
  }
  
  load_environment <- new.env(parent = emptyenv())
  
  loaded_names <- tryCatch(
    load(path, envir = load_environment),
    error = function(e) {
      stop(
        "The main-panel file could not be read with either readRDS() or load(): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  if (!is.null(expected_object) && expected_object %in% loaded_names) {
    candidate <- load_environment[[expected_object]]
    
    if (!inherits(candidate, "data.frame")) {
      stop(
        "The expected object '", expected_object,
        "' exists but is not a data frame.",
        call. = FALSE
      )
    }
    
    return(candidate)
  }
  
  data_frame_names <- loaded_names[vapply(
    loaded_names,
    function(object_name) {
      inherits(load_environment[[object_name]], "data.frame")
    },
    logical(1)
  )]
  
  if (length(data_frame_names) != 1L) {
    stop(
      "The main-panel file does not contain the expected object '",
      expected_object,
      "' and does not contain exactly one data frame. Objects found: ",
      paste(loaded_names, collapse = ", "),
      call. = FALSE
    )
  }
  
  load_environment[[data_frame_names]]
}

# ------------------------------------------------------------------------------
# Main-panel country-year universe ---------------------------------------------
# ------------------------------------------------------------------------------
main_panel_raw <- read_single_data_frame(
  main_panel_file,
  expected_object = expected_main_panel_object
)

required_main_panel_columns <- c("ISO_CODE_3", "YEAR")

missing_main_panel_columns <- setdiff(
  required_main_panel_columns,
  names(main_panel_raw)
)

main_panel_keys_raw <- main_panel_raw |>
  ungroup() |>
  transmute(
    ISO_CODE_3 = str_to_upper(str_squish(as.character(ISO_CODE_3))),
    YEAR_INT = suppressWarnings(as.integer(as.character(YEAR)))
  )

duplicate_main_panel_keys <- main_panel_keys_raw |>
  count(ISO_CODE_3, YEAR_INT, name = "N_ROWS") |>
  filter(N_ROWS > 1L)

main_panel_analysis_keys <- main_panel_keys_raw |>
  filter(
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  ) |>
  distinct() |>
  arrange(ISO_CODE_3, YEAR_INT)

# ------------------------------------------------------------------------------
# Extract and read CCP version 6 -----------------------------------------------
# ------------------------------------------------------------------------------
zip_inventory <- utils::unzip(zip_file, list = TRUE)

csv_member <- zip_inventory$Name[
  str_detect(
    zip_inventory$Name,
    regex("(^|/)ccpcce_v6\\.csv$", ignore_case = TRUE)
  ) &
    !str_detect(zip_inventory$Name, "(^|/)__MACOSX/")
]

extract_dir <- tempfile("ccpcce_v6_")
dir.create(extract_dir)

utils::unzip(
  zip_file,
  files = csv_member,
  exdir = extract_dir
)

csv_file <- file.path(extract_dir, 
                      csv_member)

ccp_raw <- read_csv(
  csv_file,
  col_types = cols(
    cowcode = col_integer(),
    country = col_character(),
    year = col_integer(),
    systid = col_double(),
    evntid = col_double(),
    evnttype = col_character()
  ),
  show_col_types = FALSE
)

unlink(extract_dir, recursive = TRUE, force = TRUE)

required_ccp_columns <- c(
  "cowcode",
  "country",
  "year",
  "systid",
  "evntid",
  "evnttype"
)

missing_ccp_columns <- setdiff(required_ccp_columns, names(ccp_raw))

# ------------------------------------------------------------------------------
# Clean and validate source rows -----------------------------------------------
# ------------------------------------------------------------------------------
allowed_event_types <- c(
  "amendment",
  "interim",
  "new",
  "non-event",
  "reinstated",
  "samendment",
  "suspension"
)

ccp_pre_iso <- ccp_raw |>
  transmute(
    CCP_ROW_ID = row_number(),
    COW_CODE = as.integer(cowcode),
    CCP_COUNTRY = str_squish(as.character(country)),
    YEAR_INT = as.integer(year),
    SYSTID = as.integer(systid),
    EVNTID = as.integer(evntid),
    EVENT_TYPE = str_to_lower(str_squish(as.character(evnttype)))
  ) |>
  mutate(
    NEW_CONSTITUTION_SOURCE = as.integer(EVENT_TYPE == "new")
  )

if (
  any(is.na(ccp_pre_iso$COW_CODE)) ||
  any(is.na(ccp_pre_iso$CCP_COUNTRY)) ||
  any(ccp_pre_iso$CCP_COUNTRY == "") ||
  any(is.na(ccp_pre_iso$YEAR_INT)) ||
  any(is.na(ccp_pre_iso$EVENT_TYPE)) ||
  any(ccp_pre_iso$EVENT_TYPE == "")
) {
  stop(
    paste0(
      "At least one CCP row has a missing COW code, country, year, or event ",
      "type."
    ),
    call. = FALSE
  )
}

unexpected_event_types <- setdiff(
  sort(unique(ccp_pre_iso$EVENT_TYPE)),
  allowed_event_types
)

# Version 6 has an explicit country-year backbone only through the last year in
# which "non-event" rows appear. Later releases append recorded events without
# manufacturing non-event rows for every country. In v6 this transition occurs
# after 2019.
ccp_complete_backbone_end_year <- ccp_pre_iso |>
  filter(EVENT_TYPE == "non-event") |>
  summarise(LAST_NON_EVENT_YEAR = max(YEAR_INT)) |>
  pull(LAST_NON_EVENT_YEAR)

if (
  length(ccp_complete_backbone_end_year) != 1L ||
  is.na(ccp_complete_backbone_end_year) ||
  ccp_complete_backbone_end_year < analysis_start_year
) {
  stop(
    paste0(
      "The end of the CCP explicit country-year backbone could not be ",
      "identified from non-event rows."
    ),
    call. = FALSE
  )
}

ccp_year_coverage_profile <- ccp_pre_iso |>
  filter(
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  ) |>
  group_by(YEAR_INT) |>
  summarise(
    N_SOURCE_ROWS = n(),
    N_SOURCE_COUNTRIES = n_distinct(COW_CODE),
    N_NON_EVENT_ROWS = sum(
      EVENT_TYPE == "non-event",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    COVERAGE_MODE = if_else(
      YEAR_INT <= ccp_complete_backbone_end_year,
      "explicit_country_year_backbone",
      "event_only"
    )
  )

# The release contains three duplicated source country-years because two
# constitutional events are recorded in the same year. These are valid source
# rows. They are retained here and later collapsed with any(), so a country-year
# equals 1 if at least one recorded event is a new constitution.
duplicate_ccp_country_years <- ccp_pre_iso |>
  add_count(COW_CODE, YEAR_INT, name = "N_SOURCE_ROWS") |>
  filter(N_SOURCE_ROWS > 1L) |>
  arrange(COW_CODE, YEAR_INT, CCP_ROW_ID)

# ------------------------------------------------------------------------------
# Harmonizinh COW codes to the main panel's ISO-3 convention -------------------
# ------------------------------------------------------------------------------
manual_cow_crosswalk <- tribble(
  ~COW_CODE, ~ISO_CODE_3_MANUAL, ~ISO_MAPPING_NOTE,
  260L,      "DEU",              "West Germany/current Germany mapped to main-panel Germany",
  265L,      "DEU",              "East Germany mapped to main-panel Germany",
  315L,      "CZE",              "Czechoslovakia mapped to main-panel Czechia convention",
  316L,      "CZE",              "Czech Republic mapped to contemporary Czechia code",
  340L,      "SRB",              "Yugoslavia/Serbia sequence mapped to main-panel Serbia convention",
  341L,      "MNE",              "Montenegro",
  343L,      "MKD",              "Former Yugoslav Republic of Macedonia mapped to North Macedonia",
  344L,      "HRV",              "Croatia",
  346L,      "BIH",              "Bosnia-Herzegovina mapped to Bosnia and Herzegovina",
  347L,      "XKX",              "Kosovo code used by the main panel",
  349L,      "SVN",              "Slovenia",
  365L,      "RUS",              "Soviet Union/Russia sequence mapped to main-panel Russia convention",
  678L,      "YEM",              "North Yemen/unified Yemen mapped to main-panel Yemen convention",
  680L,      "YEM",              "South Yemen mapped to main-panel Yemen convention",
  816L,      "VNM",              "North/unified Vietnam mapped to main-panel Vietnam convention",
  817L,      "VNM",              "South Vietnam mapped to main-panel Vietnam convention",
  860L,      "TLS",              "East Timor mapped to Timor-Leste"
)

if (anyDuplicated(manual_cow_crosswalk$COW_CODE) > 0L) {
  stop("The manual COW crosswalk contains duplicate COW codes.", call. = FALSE)
}

main_panel_codes <- main_panel_analysis_keys |>
  distinct(ISO_CODE_3) |>
  mutate(ISO_IN_MAIN_PANEL = TRUE)

invalid_manual_iso_codes <- manual_cow_crosswalk |>
  anti_join(main_panel_codes, by = c("ISO_CODE_3_MANUAL" = "ISO_CODE_3"))

ccp_with_iso <- ccp_pre_iso |>
  mutate(
    ISO_CODE_3_AUTO = suppressWarnings(
      countrycode(
        sourcevar = COW_CODE,
        origin = "cown",
        destination = "iso3c",
        warn = FALSE
      )
    )
  ) |>
  left_join(manual_cow_crosswalk, by = "COW_CODE") |>
  mutate(
    ISO_CODE_3 = coalesce(ISO_CODE_3_MANUAL, ISO_CODE_3_AUTO),
    ISO_CODE_3 = str_to_upper(as.character(ISO_CODE_3)),
    ISO_MAPPING_METHOD = case_when(
      !is.na(ISO_CODE_3_MANUAL) ~ "manual_main_panel_convention",
      !is.na(ISO_CODE_3_AUTO) ~ "countrycode_cown_to_iso3c",
      TRUE ~ "unresolved"
    )
  )

# One row per distinct CCP location/COW-code mapping for inspection.
ccp_iso_crosswalk <- ccp_with_iso |>
  distinct(
    COW_CODE,
    CCP_COUNTRY,
    ISO_CODE_3_AUTO,
    ISO_CODE_3_MANUAL,
    ISO_CODE_3,
    ISO_MAPPING_METHOD,
    ISO_MAPPING_NOTE
  ) |>
  arrange(COW_CODE, CCP_COUNTRY)

# Classifying every source row in the requested period by whether its mapped ISO
# code and exact country-year are represented in the main analytical panel.
ccp_iso_reconciliation <- ccp_with_iso |>
  filter(
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  ) |>
  left_join(main_panel_codes, by = "ISO_CODE_3") |>
  left_join(
    main_panel_analysis_keys |>
      mutate(ISO_YEAR_IN_MAIN_PANEL = TRUE),
    by = c("ISO_CODE_3", "YEAR_INT")
  ) |>
  mutate(
    ISO_IN_MAIN_PANEL = coalesce(ISO_IN_MAIN_PANEL, FALSE),
    ISO_YEAR_IN_MAIN_PANEL = coalesce(ISO_YEAR_IN_MAIN_PANEL, FALSE),
    ISO_PANEL_STATUS = case_when(
      is.na(ISO_CODE_3) ~ "iso_unresolved",
      !ISO_IN_MAIN_PANEL ~ "code_absent_from_main_panel",
      !ISO_YEAR_IN_MAIN_PANEL ~ "year_outside_main_panel_coverage",
      TRUE ~ "reconciled_to_main_panel"
    )
  )

ccp_iso_reconciliation_summary <- ccp_iso_reconciliation |>
  count(
    COW_CODE,
    CCP_COUNTRY,
    ISO_CODE_3,
    ISO_MAPPING_METHOD,
    ISO_PANEL_STATUS,
    name = "N_SOURCE_ROWS"
  ) |>
  arrange(ISO_PANEL_STATUS, COW_CODE, CCP_COUNTRY)

# A missed mapping must never silently turn a recorded new constitution into a
# zero in the analytical output.
unresolved_new_constitution_events <- ccp_with_iso |>
  filter(
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year,
    NEW_CONSTITUTION_SOURCE == 1L,
    is.na(ISO_CODE_3)
  ) |>
  select(CCP_ROW_ID, COW_CODE, CCP_COUNTRY, YEAR_INT, EVENT_TYPE)

# ------------------------------------------------------------------------------
# Constructing the country-year indicator --------------------------------------
# ------------------------------------------------------------------------------
ccp_country_year_source <- ccp_with_iso |>
  filter(
    !is.na(ISO_CODE_3),
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  ) |>
  group_by(ISO_CODE_3, YEAR_INT) |>
  summarise(
    SOURCE_ROW_PRESENT = 1L,
    N_SOURCE_ROWS = n(),
    N_NEW_CONSTITUTION_EVENTS = sum(
      NEW_CONSTITUTION_SOURCE == 1L,
      na.rm = TRUE
    ),
    NEW_CONSTITUTION = as.integer(any(
      NEW_CONSTITUTION_SOURCE == 1L,
      na.rm = TRUE
    )),
    .groups = "drop"
  )

# Join the recorded CCP rows to the main-panel universe. Coverage is validated
# differently across the two source eras:
#   * through 2019, every in-scope country-year must have an explicit row;
#   * from 2020 onward, the file is event-only, so no recorded event means zero.
main_panel_source_coverage <- main_panel_analysis_keys |>
  left_join(
    ccp_country_year_source,
    by = c("ISO_CODE_3", "YEAR_INT")
  ) |>
  mutate(
    CCP_COVERAGE_MODE = if_else(
      YEAR_INT <= ccp_complete_backbone_end_year,
      "explicit_country_year_backbone",
      "event_only"
    )
  )

unmatched_backbone_country_years <- main_panel_source_coverage |>
  filter(
    CCP_COVERAGE_MODE == "explicit_country_year_backbone",
    is.na(SOURCE_ROW_PRESENT)
  ) |>
  ungroup() |>
  select(ISO_CODE_3, YEAR_INT)

# These are expected absences, not unmatched observations. They are retained as
# an audit showing the country-years assigned zero in the event-only era because
# the CCP records no constitutional event for them.
event_only_zero_country_years <- main_panel_source_coverage |>
  filter(
    CCP_COVERAGE_MODE == "event_only",
    is.na(SOURCE_ROW_PRESENT)
  ) |>
  ungroup() |>
  select(ISO_CODE_3, YEAR_INT)

ccp_new_constitution_country_year <- main_panel_source_coverage |>
  transmute(
    ISO_CODE_3 = as.character(ISO_CODE_3),
    YEAR = as.character(YEAR_INT),
    NEW_CONSTITUTION = coalesce(
      as.integer(NEW_CONSTITUTION),
      0L
    )
  ) |>
  arrange(ISO_CODE_3, YEAR)

# ------------------------------------------------------------------------------
# Final validation -------------------------------------------------------------
# ------------------------------------------------------------------------------
if (nrow(ccp_new_constitution_country_year) !=
    nrow(main_panel_analysis_keys)) {
  stop(
    "The final output does not have exactly one row per main-panel key.",
    call. = FALSE
  )
}

if (anyDuplicated(
  ccp_new_constitution_country_year[c("ISO_CODE_3", "YEAR")]
) > 0L) {
  stop("The final output contains duplicate country-year keys.", call. = FALSE)
}

if (
  !is.character(ccp_new_constitution_country_year$ISO_CODE_3) ||
  !is.character(ccp_new_constitution_country_year$YEAR)
) {
  stop("ISO_CODE_3 and YEAR must both be character variables.", call. = FALSE)
}

if (any(is.na(ccp_new_constitution_country_year$NEW_CONSTITUTION))) {
  stop("NEW_CONSTITUTION contains missing values.", call. = FALSE)
}

if (any(!ccp_new_constitution_country_year$NEW_CONSTITUTION %in% c(0L, 1L))) {
  stop("NEW_CONSTITUTION contains values other than 0 and 1.", call. = FALSE)
}

if (any(
  as.integer(ccp_new_constitution_country_year$YEAR) < analysis_start_year |
  as.integer(ccp_new_constitution_country_year$YEAR) > analysis_end_year
)) {
  stop("The final output contains a year outside the requested period.",
       call. = FALSE)
}

final_key_check <- ccp_new_constitution_country_year |>
  transmute(
    ISO_CODE_3,
    YEAR_INT = as.integer(YEAR)
  ) |>
  anti_join(
    main_panel_analysis_keys,
    by = c("ISO_CODE_3", "YEAR_INT")
  )

reverse_key_check <- main_panel_analysis_keys |>
  anti_join(
    ccp_new_constitution_country_year |>
      transmute(
        ISO_CODE_3,
        YEAR_INT = as.integer(YEAR)
      ),
    by = c("ISO_CODE_3", "YEAR_INT")
  )

if (nrow(final_key_check) > 0L || nrow(reverse_key_check) > 0L) {
  stop(
    "The final output keys do not exactly reproduce the main-panel keys.",
    call. = FALSE
  )
}

positive_cases <- ccp_new_constitution_country_year |>
  filter(NEW_CONSTITUTION == 1L) |>
  arrange(YEAR, ISO_CODE_3)

final_summary <- tibble(
  METRIC = c(
    "output_country_years",
    "countries",
    "minimum_year",
    "maximum_year",
    "explicit_backbone_end_year",
    "event_only_zero_country_years",
    "new_constitution_country_years",
    "non_new_constitution_country_years"
  ),
  VALUE = c(
    nrow(ccp_new_constitution_country_year),
    n_distinct(ccp_new_constitution_country_year$ISO_CODE_3),
    min(as.integer(ccp_new_constitution_country_year$YEAR)),
    max(as.integer(ccp_new_constitution_country_year$YEAR)),
    ccp_complete_backbone_end_year,
    nrow(event_only_zero_country_years),
    sum(ccp_new_constitution_country_year$NEW_CONSTITUTION == 1L),
    sum(ccp_new_constitution_country_year$NEW_CONSTITUTION == 0L)
  )
)

# ------------------------------------------------------------------------------
# Saving outputs ---------------------------------------------------------------
# ------------------------------------------------------------------------------
save(
  ccp_new_constitution_country_year,
  file = "ccp_new_constitution_country_year.RDS")