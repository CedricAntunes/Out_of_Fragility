# ==============================================================================
# CLEAN UCDP CONFLICT TERMINATION DATA TO A MERGE-READY COUNTRY-YEAR PANEL
# Source: UCDP Conflict Termination Dataset v4-2024, conflict-level CSV
# Output keys: ISO_CODE_3 (character) and YEAR (character)
# Output period: 1971-2022
# ==============================================================================
#
# This pipeline uses the actual termination dataset rather than reconstructing
# endings from the annual UCDP/PRIO API data. It distinguishes:
#   1 = peace agreement
#   2 = ceasefire
#   3 = government victory
#   4 = non-state victory
#   5 = low activity
#   6 = actor ceases to exist
#
# Recommended hazard variable:
#   UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3
#
# This equals one when an episode ends through an agreement, ceasefire, or
# victory (outcomes 1-4), no intrastate conflict is active in the country in the
# following year, and the termination occurred in the current or prior two
# panel years. Once the hazard pipeline applies its standard one-year lag, the
# model at t captures terminations in t-1, t-2, or t-3.
#
# The less restrictive alternative is:
#   UCDP_SUBSTANTIVE_TERMINATION_RECENT3
# which does not require all intrastate conflict in the country to cease.
#
# IMPORTANT VERSION NOTE
# The termination codebook states that v4-2024 corresponds to UCDP/PRIO v25.1.
# The separately supplied UCDP/PRIO v26.1 file is therefore not joined here.
# The termination CSV already contains the conflict-year, location, episode,
# end-year, outcome, and Gleditsch-Ward fields required for this task.
# ==============================================================================

# 0. Packages ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(countrycode)
  library(janitor)
})

# 1. Settings ------------------------------------------------------------------

INPUT_CSV <- "D:/Users/cedric/Downloads/UCDPConflictTerminationDataset_v4_2024_Conflict.csv"

ANALYSIS_START_YEAR <- 1971L
ANALYSIS_END_YEAR   <- 2022L

# Build the internal panel over the full source period so that the recent-three
# windows are correctly populated at the beginning of the analysis period.
SOURCE_START_YEAR <- 1946L
SOURCE_END_YEAR   <- 2024L

EXPECTED_SOURCE_ROWS <- 2752L
EXPECTED_VERSION <- "4.2024002"

# 2. Read source as character --------------------------------------------------

# Reading every source column as character preserves the complete version number
# and prevents IDs from being silently converted or rounded.
termination_raw <- readr::read_csv(
  INPUT_CSV,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  janitor::clean_names()

required_columns <- c(
  "conflict_id",
  "location",
  "side_a",
  "side_b",
  "side_b_id",
  "year",
  "type_of_conflict2",
  "gwno_loc",
  "c_epid",
  "c_epno",
  "c_ep_startyear",
  "c_epterm",
  "c_outcome",
  "c_ep_endyear",
  "c_ependdate",
  "c_ependprec",
  "version"
)

missing_columns <- setdiff(required_columns, names(termination_raw))

if (length(missing_columns) > 0L) {
  stop(
    "The termination CSV is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

source_versions <- sort(unique(termination_raw$version[!is.na(termination_raw$version)]))

if (!identical(source_versions, EXPECTED_VERSION)) {
  stop(
    "Unexpected source version. Expected ", EXPECTED_VERSION,
    " but found: ", paste(source_versions, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(termination_raw) != EXPECTED_SOURCE_ROWS) {
  stop(
    "Unexpected number of source rows. Expected ", EXPECTED_SOURCE_ROWS,
    " but found ", nrow(termination_raw), ".",
    call. = FALSE
  )
}

# Preserve a byte-independent copy of the source object used by the pipeline.
saveRDS(
  termination_raw,
  file.path("ucdp_conflict_termination_v4_2024_raw_as_character.rds")
)

# 3. Parse the analytical fields ----------------------------------------------

termination <- termination_raw %>%
  transmute(
    UCDP_CONFLICT_ID = as.character(conflict_id),
    UCDP_LOCATION = as.character(location),
    UCDP_SIDE_A = as.character(side_a),
    UCDP_SIDE_B = as.character(side_b),
    UCDP_SIDE_B_ID = as.character(side_b_id),
    UCDP_ROW_YEAR = readr::parse_integer(year),
    UCDP_CONFLICT_TYPE = readr::parse_integer(type_of_conflict2),
    # Parse the location code only after filtering to intrastate conflicts.
    # Interstate/extrasystemic rows can contain comma-separated country codes.
    UCDP_GWNO_RAW = as.character(gwno_loc),
    UCDP_SOURCE_EPID = as.character(c_epid),
    UCDP_EPISODE_NUMBER = readr::parse_integer(c_epno),
    UCDP_EPISODE_START_YEAR = readr::parse_integer(c_ep_startyear),
    UCDP_EPISODE_TERMINATES = readr::parse_integer(c_epterm),
    UCDP_OUTCOME_CODE = readr::parse_integer(c_outcome),
    UCDP_EPISODE_END_YEAR = readr::parse_integer(c_ep_endyear),
    UCDP_EPISODE_END_DATE = as.Date(c_ependdate),
    UCDP_EPISODE_END_PRECISION = readr::parse_integer(c_ependprec),
    UCDP_VERSION = as.character(version)
  )

# Codebook's combined conflict type:
#   1 = extrasystemic
#   2 = interstate
#   3 = intrastate, including internationalized intrastate years
intrastate_rows <- termination %>%
  filter(UCDP_CONFLICT_TYPE == 3L) %>%
  mutate(
    UCDP_GWNO = readr::parse_integer(UCDP_GWNO_RAW)
  )

if (anyNA(intrastate_rows$UCDP_GWNO)) {
  bad_intrastate_gw_codes <- intrastate_rows %>%
    filter(is.na(UCDP_GWNO)) %>%
    distinct(UCDP_LOCATION, UCDP_GWNO_RAW)
  print(bad_intrastate_gw_codes, n = Inf)
  stop("At least one intrastate location has an invalid GW country code.", call. = FALSE)
}

# 4. Source anomaly audit ------------------------------------------------------

# The supplied source contains duplicate c_epid values for substantively
# different episodes. It also contains one termination whose row year differs
# from c_ep_endyear. These are retained and handled explicitly.
duplicate_source_epids <- intrastate_rows %>%
  filter(UCDP_EPISODE_TERMINATES == 1L) %>%
  add_count(UCDP_SOURCE_EPID, name = "N_SOURCE_EPID") %>%
  filter(N_SOURCE_EPID > 1L) %>%
  arrange(UCDP_SOURCE_EPID, UCDP_EPISODE_START_YEAR)

row_endyear_mismatches <- intrastate_rows %>%
  filter(
    UCDP_EPISODE_TERMINATES == 1L,
    !is.na(UCDP_EPISODE_END_YEAR),
    UCDP_ROW_YEAR != UCDP_EPISODE_END_YEAR
  ) %>%
  arrange(UCDP_LOCATION, UCDP_EPISODE_END_YEAR)

readr::write_csv(
  duplicate_source_epids,
  file.path("audit_duplicate_source_episode_ids.csv")
)

readr::write_csv(
  row_endyear_mismatches,
  file.path("audit_row_year_endyear_mismatches.csv")
)

# 5. Identify completed intrastate episodes -----------------------------------

termination_events <- intrastate_rows %>%
  filter(UCDP_EPISODE_TERMINATES == 1L) %>%
  mutate(
    # Use the explicitly coded episode end year, not the row year.
    END_YEAR_INT = UCDP_EPISODE_END_YEAR,
    
    # c_epid is not unique in the supplied file. This reconstructed key is.
    UCDP_EPISODE_KEY = paste(
      UCDP_CONFLICT_ID,
      UCDP_EPISODE_START_YEAR,
      UCDP_EPISODE_END_YEAR,
      sep = "_"
    ),
    
    UCDP_OUTCOME_LABEL = case_when(
      UCDP_OUTCOME_CODE == 1L ~ "Peace agreement",
      UCDP_OUTCOME_CODE == 2L ~ "Ceasefire",
      UCDP_OUTCOME_CODE == 3L ~ "Government victory",
      UCDP_OUTCOME_CODE == 4L ~ "Non-state victory",
      UCDP_OUTCOME_CODE == 5L ~ "Low activity",
      UCDP_OUTCOME_CODE == 6L ~ "Actor ceases to exist",
      TRUE ~ NA_character_
    ),
    
    # Outcomes 1-4 capture an agreement, ceasefire, or decisive victory.
    UCDP_SUBSTANTIVE_OUTCOME = as.integer(UCDP_OUTCOME_CODE %in% 1:4),
    UCDP_NEGOTIATED_OUTCOME = as.integer(UCDP_OUTCOME_CODE %in% 1:2),
    UCDP_VICTORY_OUTCOME = as.integer(UCDP_OUTCOME_CODE %in% 3:4)
  )

if (anyNA(termination_events$END_YEAR_INT)) {
  stop("At least one completed intrastate episode lacks c_ep_endyear.", call. = FALSE)
}

if (anyNA(termination_events$UCDP_OUTCOME_LABEL)) {
  bad_outcomes <- termination_events %>%
    filter(is.na(UCDP_OUTCOME_LABEL)) %>%
    distinct(UCDP_OUTCOME_CODE)
  print(bad_outcomes)
  stop("At least one completed episode has an unrecognized outcome.", call. = FALSE)
}

if (anyDuplicated(termination_events$UCDP_EPISODE_KEY)) {
  stop(
    "The reconstructed episode key is not unique. Inspect conflict/start/end combinations.",
    call. = FALSE
  )
}

# 6. Map Gleditsch-Ward locations to ISO3 -------------------------------------

# Explicit overrides make historically sensitive mappings transparent. These
# mappings align historical UCDP locations with the current ISO3 identifiers
# conventionally used in country-year panels.
manual_gw_overrides <- tibble::tribble(
  ~UCDP_GWNO, ~ISO_CODE_3_OVERRIDE, ~MAPPING_NOTE,
  365L,        "RUS",                "Russia / Soviet Union",
  345L,        "SRB",                "Serbia / Yugoslavia",
  678L,        "YEM",                "North Yemen",
  680L,        "YEM",                "South Yemen",
  552L,        "ZWE",                "Zimbabwe / Rhodesia",
  811L,        "KHM",                "Cambodia / Kampuchea",
  817L,        "VNM",                "South Vietnam mapped to present-day Vietnam",
  751L,        "IND",                "Hyderabad mapped to India"
)

location_crosswalk <- intrastate_rows %>%
  distinct(UCDP_GWNO, UCDP_LOCATION) %>%
  mutate(
    ISO_CODE_3_AUTO = countrycode::countrycode(
      sourcevar = UCDP_GWNO,
      origin = "gwn",
      destination = "iso3c",
      warn = FALSE
    ),
    ISO_CODE_3_NAME = countrycode::countrycode(
      sourcevar = UCDP_LOCATION,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  ) %>%
  left_join(manual_gw_overrides, by = "UCDP_GWNO") %>%
  mutate(
    ISO_CODE_3 = coalesce(
      ISO_CODE_3_OVERRIDE,
      ISO_CODE_3_AUTO,
      ISO_CODE_3_NAME
    ),
    ISO_CODE_3 = as.character(ISO_CODE_3),
    ISO_MAPPING_METHOD = case_when(
      !is.na(ISO_CODE_3_OVERRIDE) ~ "manual_historical_override",
      !is.na(ISO_CODE_3_AUTO) ~ "countrycode_gw_number",
      !is.na(ISO_CODE_3_NAME) ~ "countrycode_location_name",
      TRUE ~ "unmapped"
    )
  )

readr::write_csv(
  location_crosswalk,
  file.path("audit_ucdp_location_iso3_crosswalk.csv")
)

unmapped_locations <- location_crosswalk %>%
  filter(is.na(ISO_CODE_3) | ISO_CODE_3 == "")

if (nrow(unmapped_locations) > 0L) {
  print(unmapped_locations, n = Inf)
  stop(
    "At least one intrastate-conflict location was not mapped to ISO3. ",
    "Review audit_ucdp_location_iso3_crosswalk.csv and add a justified override.",
    call. = FALSE
  )
}

# Every GW location should map consistently to one ISO3 code.
inconsistent_gw_mappings <- location_crosswalk %>%
  count(UCDP_GWNO, ISO_CODE_3, name = "N_LOCATION_LABELS") %>%
  count(UCDP_GWNO, name = "N_ISO_CODES") %>%
  filter(N_ISO_CODES > 1L)

if (nrow(inconsistent_gw_mappings) > 0L) {
  print(inconsistent_gw_mappings, n = Inf)
  stop("A GW location maps to more than one ISO3 code.", call. = FALSE)
}

intrastate_rows_iso <- intrastate_rows %>%
  left_join(
    location_crosswalk %>% select(UCDP_GWNO, UCDP_LOCATION, ISO_CODE_3),
    by = c("UCDP_GWNO", "UCDP_LOCATION")
  )

termination_events_iso <- termination_events %>%
  left_join(
    location_crosswalk %>% select(UCDP_GWNO, UCDP_LOCATION, ISO_CODE_3),
    by = c("UCDP_GWNO", "UCDP_LOCATION")
  )

stopifnot(
  !anyNA(intrastate_rows_iso$ISO_CODE_3),
  !anyNA(termination_events_iso$ISO_CODE_3)
)

# 7. Construct active-conflict country-years ----------------------------------

# The ordinary year field identifies active conflict-years. Add every explicitly
# coded c_ep_endyear as active by definition. This fixes the known source record
# where the row year and coded end year differ.
active_from_rows <- intrastate_rows_iso %>%
  filter(!is.na(UCDP_ROW_YEAR)) %>%
  transmute(
    ISO_CODE_3,
    YEAR_INT = UCDP_ROW_YEAR,
    UCDP_CONFLICT_ID
  )

active_from_endyears <- termination_events_iso %>%
  transmute(
    ISO_CODE_3,
    YEAR_INT = END_YEAR_INT,
    UCDP_CONFLICT_ID
  )

active_conflict_country_year <- bind_rows(
  active_from_rows,
  active_from_endyears
) %>%
  distinct() %>%
  group_by(ISO_CODE_3, YEAR_INT) %>%
  summarise(
    UCDP_INTRASTATE_CONFLICT_ACTIVE = 1L,
    UCDP_N_ACTIVE_CONFLICTS = n_distinct(UCDP_CONFLICT_ID),
    .groups = "drop"
  )

# 8. Aggregate termination types to country-end-year --------------------------

termination_country_year <- termination_events_iso %>%
  group_by(ISO_CODE_3, END_YEAR_INT) %>%
  summarise(
    UCDP_ANY_TERMINATION = 1L,
    UCDP_N_TERMINATIONS = n_distinct(UCDP_EPISODE_KEY),
    UCDP_SUBSTANTIVE_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE %in% 1:4)),
    UCDP_N_SUBSTANTIVE_TERMINATIONS = n_distinct(
      UCDP_EPISODE_KEY[UCDP_OUTCOME_CODE %in% 1:4]
    ),
    UCDP_NEGOTIATED_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE %in% 1:2)),
    UCDP_PEACE_AGREEMENT_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 1L)),
    UCDP_CEASEFIRE_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 2L)),
    UCDP_VICTORY_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE %in% 3:4)),
    UCDP_GOVERNMENT_VICTORY_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 3L)),
    UCDP_NONSTATE_VICTORY_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 4L)),
    UCDP_LOW_ACTIVITY_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 5L)),
    UCDP_ACTOR_CEASES_TERMINATION = as.integer(any(UCDP_OUTCOME_CODE == 6L)),
    .groups = "drop"
  ) %>%
  rename(YEAR_INT = END_YEAR_INT)

# 9. Build a balanced country-year panel --------------------------------------

# The panel includes current ISO3 identifiers plus every historical identifier
# present after the transparent mapping above. Extra country-years are harmless
# in a later left join to standardized_data_final; their presence ensures that
# non-conflict country-years are represented as explicit zeros.
current_iso_universe <- countrycode::codelist %>%
  transmute(ISO_CODE_3 = as.character(iso3c)) %>%
  filter(!is.na(ISO_CODE_3), ISO_CODE_3 != "") %>%
  distinct()

all_iso_codes <- sort(unique(c(
  current_iso_universe$ISO_CODE_3,
  location_crosswalk$ISO_CODE_3
)))

country_year_full <- tidyr::expand_grid(
  ISO_CODE_3 = all_iso_codes,
  YEAR_INT = seq.int(SOURCE_START_YEAR, SOURCE_END_YEAR)
) %>%
  left_join(
    active_conflict_country_year,
    by = c("ISO_CODE_3", "YEAR_INT")
  ) %>%
  left_join(
    termination_country_year,
    by = c("ISO_CODE_3", "YEAR_INT")
  )

count_variables <- c(
  "UCDP_N_ACTIVE_CONFLICTS",
  "UCDP_N_TERMINATIONS",
  "UCDP_N_SUBSTANTIVE_TERMINATIONS"
)

event_variables <- c(
  "UCDP_INTRASTATE_CONFLICT_ACTIVE",
  "UCDP_ANY_TERMINATION",
  "UCDP_SUBSTANTIVE_TERMINATION",
  "UCDP_NEGOTIATED_TERMINATION",
  "UCDP_PEACE_AGREEMENT_TERMINATION",
  "UCDP_CEASEFIRE_TERMINATION",
  "UCDP_VICTORY_TERMINATION",
  "UCDP_GOVERNMENT_VICTORY_TERMINATION",
  "UCDP_NONSTATE_VICTORY_TERMINATION",
  "UCDP_LOW_ACTIVITY_TERMINATION",
  "UCDP_ACTOR_CEASES_TERMINATION"
)

country_year_full <- country_year_full %>%
  mutate(
    across(
      all_of(c(count_variables, event_variables)),
      ~ tidyr::replace_na(as.integer(.x), 0L)
    )
  ) %>%
  group_by(ISO_CODE_3) %>%
  arrange(YEAR_INT, .by_group = TRUE) %>%
  mutate(
    UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR = as.integer(
      dplyr::lead(UCDP_INTRASTATE_CONFLICT_ACTIVE, default = NA_integer_) == 0L &
        YEAR_INT < SOURCE_END_YEAR
    ),
    
    # Broad country cessation includes all six UCDP termination outcomes.
    UCDP_COUNTRY_CONFLICT_CESSATION = as.integer(
      UCDP_ANY_TERMINATION == 1L &
        UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR == 1L
    ),
    
    # Preferred strict measure: agreement, ceasefire, or victory, followed by
    # no active intrastate conflict in the country in the next calendar year.
    UCDP_SUBSTANTIVE_COUNTRY_CESSATION = as.integer(
      UCDP_SUBSTANTIVE_TERMINATION == 1L &
        UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR == 1L
    ),
    
    # Three-year windows. Because the hazard pipeline later adds L1, these
    # become t-1/t-2/t-3 predictors relative to the exit at t.
    UCDP_ANY_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_ANY_TERMINATION,
        dplyr::lag(UCDP_ANY_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_ANY_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_SUBSTANTIVE_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_SUBSTANTIVE_TERMINATION,
        dplyr::lag(UCDP_SUBSTANTIVE_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_SUBSTANTIVE_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_NEGOTIATED_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_NEGOTIATED_TERMINATION,
        dplyr::lag(UCDP_NEGOTIATED_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_NEGOTIATED_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_PEACE_AGREEMENT_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_PEACE_AGREEMENT_TERMINATION,
        dplyr::lag(UCDP_PEACE_AGREEMENT_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_PEACE_AGREEMENT_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_CEASEFIRE_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_CEASEFIRE_TERMINATION,
        dplyr::lag(UCDP_CEASEFIRE_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_CEASEFIRE_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_VICTORY_TERMINATION_RECENT3 = as.integer(
      pmax(
        UCDP_VICTORY_TERMINATION,
        dplyr::lag(UCDP_VICTORY_TERMINATION, 1L, default = 0L),
        dplyr::lag(UCDP_VICTORY_TERMINATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_COUNTRY_CONFLICT_CESSATION_RECENT3 = as.integer(
      pmax(
        UCDP_COUNTRY_CONFLICT_CESSATION,
        dplyr::lag(UCDP_COUNTRY_CONFLICT_CESSATION, 1L, default = 0L),
        dplyr::lag(UCDP_COUNTRY_CONFLICT_CESSATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    ),
    UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3 = as.integer(
      pmax(
        UCDP_SUBSTANTIVE_COUNTRY_CESSATION,
        dplyr::lag(UCDP_SUBSTANTIVE_COUNTRY_CESSATION, 1L, default = 0L),
        dplyr::lag(UCDP_SUBSTANTIVE_COUNTRY_CESSATION, 2L, default = 0L),
        na.rm = TRUE
      ) > 0L
    )
  ) %>%
  ungroup()

# 10. Produce the merge-ready 1971-2022 output --------------------------------

ucdp_conflict_termination_country_year <- country_year_full %>%
  filter(
    YEAR_INT >= ANALYSIS_START_YEAR,
    YEAR_INT <= ANALYSIS_END_YEAR
  ) %>%
  mutate(
    ISO_CODE_3 = as.character(ISO_CODE_3),
    YEAR = as.character(YEAR_INT)
  ) %>%
  select(
    ISO_CODE_3,
    YEAR,
    all_of(count_variables),
    all_of(event_variables),
    UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR,
    UCDP_COUNTRY_CONFLICT_CESSATION,
    UCDP_SUBSTANTIVE_COUNTRY_CESSATION,
    UCDP_ANY_TERMINATION_RECENT3,
    UCDP_SUBSTANTIVE_TERMINATION_RECENT3,
    UCDP_NEGOTIATED_TERMINATION_RECENT3,
    UCDP_PEACE_AGREEMENT_TERMINATION_RECENT3,
    UCDP_CEASEFIRE_TERMINATION_RECENT3,
    UCDP_VICTORY_TERMINATION_RECENT3,
    UCDP_COUNTRY_CONFLICT_CESSATION_RECENT3,
    UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3
  ) %>%
  arrange(ISO_CODE_3, YEAR)

# If the key in your local data is genuinely ISO_COD_3 rather than ISO_CODE_3,
# uncomment the following line:
# ucdp_conflict_termination_country_year <-
#   ucdp_conflict_termination_country_year %>%
#   rename(ISO_COD_3 = ISO_CODE_3)

# 11. Event-level audit --------------------------------------------------------

termination_event_audit <- termination_events_iso %>%
  filter(
    END_YEAR_INT >= ANALYSIS_START_YEAR,
    END_YEAR_INT <= ANALYSIS_END_YEAR
  ) %>%
  mutate(
    YEAR = as.character(END_YEAR_INT)
  ) %>%
  select(
    ISO_CODE_3,
    YEAR,
    UCDP_LOCATION,
    UCDP_GWNO,
    UCDP_EPISODE_KEY,
    UCDP_SOURCE_EPID,
    UCDP_CONFLICT_ID,
    UCDP_SIDE_A,
    UCDP_SIDE_B,
    UCDP_EPISODE_START_YEAR,
    UCDP_EPISODE_END_YEAR,
    UCDP_EPISODE_END_DATE,
    UCDP_EPISODE_END_PRECISION,
    UCDP_OUTCOME_CODE,
    UCDP_OUTCOME_LABEL,
    UCDP_SUBSTANTIVE_OUTCOME,
    UCDP_NEGOTIATED_OUTCOME,
    UCDP_VICTORY_OUTCOME
  ) %>%
  arrange(ISO_CODE_3, YEAR, UCDP_CONFLICT_ID)

# 12. Validation ---------------------------------------------------------------

stopifnot(
  is.character(ucdp_conflict_termination_country_year$ISO_CODE_3),
  is.character(ucdp_conflict_termination_country_year$YEAR),
  !anyDuplicated(
    ucdp_conflict_termination_country_year[c("ISO_CODE_3", "YEAR")]
  ),
  !anyDuplicated(termination_event_audit$UCDP_EPISODE_KEY)
)

binary_output_variables <- c(
  event_variables,
  "UCDP_NO_INTRASTATE_CONFLICT_NEXT_YEAR",
  "UCDP_COUNTRY_CONFLICT_CESSATION",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION",
  "UCDP_ANY_TERMINATION_RECENT3",
  "UCDP_SUBSTANTIVE_TERMINATION_RECENT3",
  "UCDP_NEGOTIATED_TERMINATION_RECENT3",
  "UCDP_PEACE_AGREEMENT_TERMINATION_RECENT3",
  "UCDP_CEASEFIRE_TERMINATION_RECENT3",
  "UCDP_VICTORY_TERMINATION_RECENT3",
  "UCDP_COUNTRY_CONFLICT_CESSATION_RECENT3",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3"
)

for (variable in binary_output_variables) {
  values <- unique(ucdp_conflict_termination_country_year[[variable]])
  if (anyNA(values) || !all(values %in% c(0L, 1L))) {
    stop(variable, " is not a complete 0/1 indicator.", call. = FALSE)
  }
}

if (any(
  ucdp_conflict_termination_country_year$UCDP_SUBSTANTIVE_COUNTRY_CESSATION == 1L &
  ucdp_conflict_termination_country_year$UCDP_SUBSTANTIVE_TERMINATION == 0L
)) {
  stop("A strict substantive cessation lacks a substantive termination.", call. = FALSE)
}

# Empirical checks tied to the supplied source file.
source_intrastate_ends <- termination_events_iso
analysis_intrastate_ends <- termination_event_audit

if (nrow(source_intrastate_ends) != 415L) {
  stop("Expected 415 completed intrastate episodes in the full source period.")
}

if (nrow(analysis_intrastate_ends) != 341L) {
  stop("Expected 341 completed intrastate episodes during 1971-2022.")
}

if (sum(analysis_intrastate_ends$UCDP_SUBSTANTIVE_OUTCOME) != 159L) {
  stop("Expected 159 substantive intrastate terminations during 1971-2022.")
}

expected_analysis_outcomes <- tibble::tibble(
  UCDP_OUTCOME_CODE = 1:6,
  EXPECTED_N = c(33L, 47L, 58L, 21L, 167L, 15L)
)

observed_analysis_outcomes <- analysis_intrastate_ends %>%
  count(UCDP_OUTCOME_CODE, name = "OBSERVED_N") %>%
  right_join(expected_analysis_outcomes, by = "UCDP_OUTCOME_CODE") %>%
  mutate(OBSERVED_N = tidyr::replace_na(OBSERVED_N, 0L))

if (any(observed_analysis_outcomes$OBSERVED_N != observed_analysis_outcomes$EXPECTED_N)) {
  print(observed_analysis_outcomes)
  stop("Outcome counts do not match the supplied v4-2024 source file.")
}

# 13. Save outputs -------------------------------------------------------------
save(
  ucdp_conflict_termination_country_year,
  file = "ucdp_conflict_termination_country_year.RDS")

panel_csv <- file.path(paste0(
    "ucdp_conflict_termination_country_year_",
    ANALYSIS_START_YEAR, "_", ANALYSIS_END_YEAR, ".csv"
  )
)

panel_rds <- file.path(paste0(
    "ucdp_conflict_termination_country_year_",
    ANALYSIS_START_YEAR, "_", ANALYSIS_END_YEAR, ".rds"
  )
)

event_csv <- file.path(paste0(
    "ucdp_conflict_termination_event_audit_",
    ANALYSIS_START_YEAR, "_", ANALYSIS_END_YEAR, ".csv"
  )
)

readr::write_csv(ucdp_conflict_termination_country_year, panel_csv)
saveRDS(ucdp_conflict_termination_country_year, panel_rds)
readr::write_csv(termination_event_audit, event_csv)

# 14. Console audit ------------------------------------------------------------

cat("\n===============================================================\n")
cat("UCDP CONFLICT TERMINATION CLEANING COMPLETE\n")
cat("===============================================================\n")
cat("Source version:                         ", EXPECTED_VERSION, "\n")
cat("Source rows:                            ", nrow(termination_raw), "\n")
cat("Completed intrastate episodes, all years:", nrow(source_intrastate_ends), "\n")
cat("Completed intrastate episodes, 1971-2022:", nrow(analysis_intrastate_ends), "\n")
cat("Substantive endings, 1971-2022:          ", sum(analysis_intrastate_ends$UCDP_SUBSTANTIVE_OUTCOME), "\n")
cat("Low-activity/actor endings, 1971-2022:   ", sum(analysis_intrastate_ends$UCDP_OUTCOME_CODE %in% 5:6), "\n")
cat("Duplicate source c_epid rows audited:    ", nrow(duplicate_source_epids), "\n")
cat("Row/end-year mismatches audited:         ", nrow(row_endyear_mismatches), "\n")
cat("Merge-ready rows:                        ", format(nrow(ucdp_conflict_termination_country_year), big.mark = ","), "\n")
cat("Unique ISO3 codes:                       ", n_distinct(ucdp_conflict_termination_country_year$ISO_CODE_3), "\n")
cat("Panel CSV:                               ", panel_csv, "\n")
cat("Panel RDS:                               ", panel_rds, "\n")
cat("Event audit:                             ", event_csv, "\n")
cat("===============================================================\n\n")

print(observed_analysis_outcomes)

print(
  ucdp_conflict_termination_country_year %>%
    summarise(
      N_ANY_TERMINATION = sum(UCDP_ANY_TERMINATION),
      N_SUBSTANTIVE_TERMINATION = sum(UCDP_SUBSTANTIVE_TERMINATION),
      N_SUBSTANTIVE_RECENT3 = sum(UCDP_SUBSTANTIVE_TERMINATION_RECENT3),
      N_STRICT_SUBSTANTIVE_CESSATION = sum(UCDP_SUBSTANTIVE_COUNTRY_CESSATION),
      N_STRICT_SUBSTANTIVE_RECENT3 = sum(UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3)
    )
)

# ==============================================================================
# MERGE REMINDER (not executed)
# ==============================================================================
#
# standardized_data_final <- standardized_data_final %>%
#   left_join(
#     ucdp_conflict_termination_country_year,
#     by = c("ISO_CODE_3", "YEAR")
#   )
#
# Before using a termination indicator in the hazard model:
#   1. Add it to FINAL_VARS_BASE.
#   2. Add it to the `binary_vars` list so it is not standardized.
#   3. Let the existing hazard pipeline generate its one-year lag.
#
# Preferred strict variable:
#   UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3
#
# Less restrictive alternative:
#   UCDP_SUBSTANTIVE_TERMINATION_RECENT3
# ==============================================================================