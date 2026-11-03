# NAVCO 1.3: MAIN-PANEL-COMPATIBLE MASS MOBILIZATION CLEANING PIPELINE ---------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: August, 2026 -----------------------------------------------------------

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# ------------------------------------------------------------------------------
# Settings ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Input (raw) directory
zip_file <- "D:/Users/cedric/Downloads/dataverse_files.zip"

# Main analytical dataset
main_panel_file <- "D:/Fragility-Exit/standardized_data_final.RDS"

# Requested output period.
analysis_start_year <- 1971L
analysis_end_year   <- 2022L

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readxl)
  library(readr)
  library(stringr)
  library(countrycode)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
to_binary_integer <- function(x) {
  out <- suppressWarnings(as.integer(x))
  if (any(!is.na(out) & !out %in% c(0L, 1L))) {
    stop("A purported binary variable contains values other than 0, 1, or NA.",
         call. = FALSE)
  }
  out
}

any_int <- function(x) as.integer(any(x %in% TRUE, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Main-panel country-year keys -------------------------------------------------
# ------------------------------------------------------------------------------
main_panel_environment <- new.env(parent = emptyenv())

loaded_object_names <- load(
  main_panel_file,
  envir = main_panel_environment
)

main_panel_raw <- main_panel_environment[["standardized_data_final"]]

required_main_panel_columns <- c("ISO_CODE_3", 
                                 "YEAR")

missing_main_panel_columns <- setdiff(
  required_main_panel_columns,
  names(main_panel_raw)
)

main_panel_keys_raw <- main_panel_raw |>
  transmute(
    ISO_CODE_3 = as.character(ISO_CODE_3),
    YEAR_INT = suppressWarnings(as.integer(YEAR))
  )

duplicate_main_panel_keys <- main_panel_keys_raw |>
  count(
    ISO_CODE_3,
    YEAR_INT,
    name = "N_ROWS"
  ) |>
  filter(N_ROWS > 1L)

main_panel_keys <- main_panel_keys_raw |>
  distinct()

main_panel_analysis_keys <- main_panel_keys |>
  filter(
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  )

# Extracting raw data ----------------------------------------------------------
if (!file.exists(zip_file)) {
  stop("ZIP file not found: ", zip_file, call. = FALSE)
}

zip_inventory <- utils::unzip(zip_file, list = TRUE)
xlsx_member <- zip_inventory$Name[
  str_detect(zip_inventory$Name, regex("NAVCO.*1\\.3.*\\.xlsx$", ignore_case = TRUE))
]

extract_dir <- tempfile("navco_1_3_")
dir.create(extract_dir)

utils::unzip(zip_file, files = xlsx_member, exdir = extract_dir)
xlsx_file <- file.path(extract_dir, xlsx_member)

navco_raw <- read_excel(xlsx_file, sheet = 1) |>
  rename_with(~ str_to_upper(str_replace_all(.x, "[^A-Za-z0-9]+", "_")))

unlink(extract_dir, 
       recursive = TRUE, 
       force = TRUE)

# Required columns -------------------------------------------------------------
required_columns <- c(
  "NAVCOID", 
  "CAMPAIGN", 
  "LOCATION", 
  "BYEAR", 
  "EYEAR", 
  "TARGET",
  "SUCCESS", 
  "LIMITED", 
  "FAILURE", 
  "ONGOING", 
  "VIOL", 
  "NONVIOL",
  "FSELFDET", 
  "REGCHANGE", 
  "SECESSION", 
  "OTHER"
)

binary_columns <- c(
  "SUCCESS", 
  "LIMITED", 
  "FAILURE", 
  "ONGOING", 
  "VIOL", 
  "NONVIOL",
  "FSELFDET", 
  "REGCHANGE", 
  "SECESSION", 
  "OTHER"
)

missing_navco_columns <- setdiff(required_columns, names(navco_raw))

# Cleaning and auditing campaign-level data ------------------------------------
navco_campaign_pre_iso <- navco_raw |>
  transmute(
    NAVCO_ROW_ID = row_number(),
    NAVCOID = as.character(NAVCOID),
    CAMPAIGN = str_squish(as.character(CAMPAIGN)),
    NAVCO_LOCATION = str_squish(as.character(LOCATION)),
    BYEAR = as.integer(BYEAR),
    EYEAR = as.integer(EYEAR),
    TARGET = na_if(str_squish(as.character(TARGET)), ""),
    across(all_of(binary_columns), to_binary_integer)
  ) |>
  mutate(
    # Directly coded, preferred measure
    TARGET_INCUMBENT_REMOVAL = REGCHANGE,
    # Exploratory (coded on our end)
    TARGET_GOVERNING_ACTOR_TEXT = as.integer(
      str_detect(
        coalesce(TARGET, ""),
        regex(
          paste0(
            "\\b(government|gov(?:ernment)?|gov['’]?t|regime|junta|",
            "president|prime minister|premier|ruling party|administration|",
            "cabinet|incumbent|dictator|monarch|king|military rule)\\b"
          ),
          ignore_case = TRUE
        )
      )
    ),
    MASS_CAMPAIGN = 1L,
    NONMASS_CAMPAIGN = NA_integer_,
    SOURCE_OTHER_MISSING = as.integer(is.na(OTHER)),
    SOURCE_TARGET_MISSING = as.integer(is.na(TARGET))
  )

if (any(is.na(navco_campaign_pre_iso$BYEAR)) ||
    any(is.na(navco_campaign_pre_iso$EYEAR))) {
  stop("At least one campaign has a missing BYEAR or EYEAR.", call. = FALSE)
}

if (any(navco_campaign_pre_iso$BYEAR > navco_campaign_pre_iso$EYEAR,
        na.rm = TRUE)) {
  stop("At least one campaign has BYEAR greater than EYEAR.", call. = FALSE)
}

if (any(is.na(navco_campaign_pre_iso$VIOL)) ||
    any(is.na(navco_campaign_pre_iso$NONVIOL)) ||
    any(is.na(navco_campaign_pre_iso$REGCHANGE))) {
  stop(
    "VIOL, NONVIOL, and REGCHANGE must be observed for every campaign.",
    call. = FALSE
  )
}

if (any(navco_campaign_pre_iso$VIOL + navco_campaign_pre_iso$NONVIOL != 1L,
        na.rm = TRUE)) {
  stop("VIOL and NONVIOL are not mutually exclusive and exhaustive.",
       call. = FALSE)
}

# NAVCOID is not unique in the released workbook, so NAVCO_ROW_ID is the safe
# row identifier. Preserving the repeated source IDs and write them to an audit.
duplicate_navco_ids <- navco_campaign_pre_iso |>
  add_count(NAVCOID, name = "N_ROWS_WITH_NAVCOID") |>
  filter(N_ROWS_WITH_NAVCOID > 1L) |>
  arrange(NAVCOID, 
          BYEAR, 
          NAVCO_LOCATION)

# ------------------------------------------------------------------------------
# Harmonizing ISO-3 ------------------------------------------------------------
# ------------------------------------------------------------------------------
manual_location_crosswalk <- tribble(
  ~NAVCO_LOCATION,                    ~ISO_CODE_3_MANUAL, ~ISO_MAPPING_NOTE,                                      ~ISO_REVIEW,
  "Bosnia-Herzegovina",              "BIH",              "Nonstandard spelling",                               FALSE,
  "Burma",                           "MMR",              "Contemporary Myanmar code",                          FALSE,
  "Congo-Brazzaville (ROC)",         "COG",              "Republic of the Congo",                              FALSE,
  "Czechoslovakia",                  "CZE",              "Reconciled to main-panel successor convention",         FALSE,
  "Democratic Republic of Congo",    "COD",              "Democratic Republic of the Congo",                   FALSE,
  "East Germany",                    "DEU",              "Reconciled to main-panel successor convention",         FALSE,
  "East Timor",                      "TLS",              "Contemporary Timor-Leste code",                      FALSE,
  "Ivory Coast",                     "CIV",              "Cote d'Ivoire",                                      FALSE,
  "Kosovo",                          "XKX",              "Reconciled to main-panel Kosovo convention",          FALSE,
  "Macedonia",                       "MKD",              "North Macedonia",                                    FALSE,
  "Namibia/South West Africa",       "NAM",              "Contemporary Namibia code",                          FALSE,
  "Natal",                           "ZAF",              "Reconciled to main-panel South Africa convention",    FALSE,
  "Northern Ireland",                "GBR",              "Reconciled to main-panel United Kingdom convention",  FALSE,
  "Ottoman Empire",                  "TUR",              "Reconciled to main-panel successor convention",       FALSE,
  "Palestine",                       "PSE",              "Valid code; absent from the current main panel",       FALSE,
  "Palestinian Territories",         "PSE",              "Valid code; absent from the current main panel",       FALSE,
  "Republic of the Congo",           "COG",              "Republic of the Congo",                              FALSE,
  "Republic of Yemen",               "YEM",              "Unified Yemen code",                                FALSE,
  "South Korea",                     "KOR",              "Republic of Korea",                                  FALSE,
  "South Vietnam",                   "VNM",              "Reconciled to main-panel successor convention",         FALSE,
  "Swaziland",                       "SWZ",              "Source-period name; ISO code retained",              FALSE,
  "Tanzania/German East Africa",     "TZA",              "Reconciled to main-panel Tanzania convention",        FALSE,
  "Tibet",                           "CHN",              "Reconciled to main-panel China convention",           FALSE,
  "USSR",                            "RUS",              "Reconciled to main-panel successor convention",       FALSE,
  "West Papua",                      "IDN",              "Reconciled to main-panel Indonesia convention",       FALSE,
  "Western Sahara",                  "MAR",              "Reconciled to main-panel Morocco convention",         FALSE,
  "Yemen Arab Republic",             "YEM",              "Reconciled to main-panel unified-Yemen convention",   FALSE,
  "Yemen People's Republic",         "YEM",              "Reconciled to main-panel unified-Yemen convention",   FALSE,
  "Yugoslavia",                      "SRB",              "Reconciled to main-panel successor convention",       FALSE,
  "Zaire/DRC",                       "COD",              "Democratic Republic of the Congo",                   FALSE
)

auto_iso <- suppressWarnings(
  countrycode(
    sourcevar = navco_campaign_pre_iso$NAVCO_LOCATION,
    origin = "country.name",
    destination = "iso3c",
    warn = FALSE
  )
)

navco_campaign_clean <- navco_campaign_pre_iso |>
  mutate(ISO_CODE_3_AUTO = auto_iso) |>
  left_join(manual_location_crosswalk,
            by = "NAVCO_LOCATION") |>
  mutate(
    ISO_CODE_3 = coalesce(ISO_CODE_3_MANUAL, 
                          ISO_CODE_3_AUTO),
    ISO_MAPPING_METHOD = case_when(
      !is.na(ISO_CODE_3_MANUAL) ~ "manual",
      !is.na(ISO_CODE_3_AUTO) ~ "countrycode",
      TRUE ~ "unresolved"
    ),
    ISO_REVIEW = coalesce(ISO_REVIEW, FALSE)
  ) |>
  select(
    NAVCO_ROW_ID, 
    NAVCOID, 
    CAMPAIGN, 
    NAVCO_LOCATION, 
    ISO_CODE_3,
    ISO_MAPPING_METHOD, 
    ISO_MAPPING_NOTE, 
    ISO_REVIEW,
    BYEAR, 
    EYEAR, 
    TARGET, 
    TARGET_INCUMBENT_REMOVAL,
    TARGET_GOVERNING_ACTOR_TEXT, 
    MASS_CAMPAIGN, 
    NONMASS_CAMPAIGN,
    VIOL, 
    NONVIOL, 
    FSELFDET, 
    REGCHANGE, 
    SECESSION, 
    OTHER,
    SUCCESS, 
    LIMITED, 
    FAILURE, 
    ONGOING,
    SOURCE_OTHER_MISSING, 
    SOURCE_TARGET_MISSING
  )

unresolved_locations <- navco_campaign_clean |>
  filter(is.na(ISO_CODE_3)) |>
  distinct(NAVCO_LOCATION) |>
  arrange(NAVCO_LOCATION)

# ------------------------------------------------------------------------------
# Country-year campaigns -------------------------------------------------------
# ------------------------------------------------------------------------------
navco_campaign_year <- navco_campaign_clean |>
  mutate(YEAR_INT = map2(BYEAR, EYEAR, ~ seq.int(.x, .y))) |>
  unnest(YEAR_INT) |>
  mutate(
    IS_ONSET = as.integer(YEAR_INT == BYEAR),
    IS_PEAK_OR_END = as.integer(YEAR_INT == EYEAR)
  )

# ------------------------------------------------------------------------------
# Reconcile manual mappings with the main analytical panel ---------------------
# ------------------------------------------------------------------------------
main_panel_codes <- main_panel_keys |>
  distinct(ISO_CODE_3) |>
  mutate(ISO_IN_MAIN_PANEL = TRUE)

main_panel_keys_flagged <- main_panel_keys |>
  mutate(ISO_YEAR_IN_MAIN_PANEL = TRUE)

manual_iso_reconciliation <- navco_campaign_year |>
  filter(
    ISO_MAPPING_METHOD == "manual",
    YEAR_INT >= analysis_start_year,
    YEAR_INT <= analysis_end_year
  ) |>
  left_join(main_panel_codes, by = "ISO_CODE_3") |>
  left_join(
    main_panel_keys_flagged,
    by = c("ISO_CODE_3", "YEAR_INT")
  ) |>
  mutate(
    ISO_IN_MAIN_PANEL = replace_na(ISO_IN_MAIN_PANEL, FALSE),
    ISO_YEAR_IN_MAIN_PANEL = replace_na(ISO_YEAR_IN_MAIN_PANEL, FALSE),
    ISO_PANEL_STATUS = case_when(
      !ISO_IN_MAIN_PANEL ~ "code_absent_from_main_panel",
      !ISO_YEAR_IN_MAIN_PANEL ~ "year_outside_main_panel_coverage",
      TRUE ~ "reconciled_to_main_panel"
    )
  )

manual_iso_reconciliation_summary <- manual_iso_reconciliation |>
  group_by(
    NAVCO_LOCATION,
    ISO_CODE_3,
    ISO_MAPPING_NOTE,
    ISO_PANEL_STATUS
  ) |>
  summarise(
    N_CAMPAIGNS = n_distinct(NAVCO_ROW_ID),
    N_CAMPAIGN_YEARS = n(),
    FIRST_YEAR = min(YEAR_INT),
    LAST_YEAR = max(YEAR_INT),
    .groups = "drop"
  ) |>
  arrange(ISO_PANEL_STATUS, NAVCO_LOCATION)

# Under the current main-panel convention, the only permissible unmatched
# manual mappings are Palestine, whose PSE code is absent from the panel, and
# East Timor campaign-years preceding the panel's Timor-Leste coverage.
tls_panel_years <- main_panel_keys |>
  filter(ISO_CODE_3 == "TLS") |>
  pull(YEAR_INT)

tls_first_panel_year <- if (length(tls_panel_years) == 0L) {
  NA_integer_
} else {
  min(tls_panel_years)
}

unexpected_manual_nonmatches <- manual_iso_reconciliation |>
  filter(ISO_PANEL_STATUS != "reconciled_to_main_panel") |>
  filter(
    !(
      ISO_CODE_3 == "PSE" &
        ISO_PANEL_STATUS == "code_absent_from_main_panel"
    ),
    !(
      ISO_CODE_3 == "TLS" &
        !is.na(tls_first_panel_year) &
        YEAR_INT < tls_first_panel_year &
        ISO_PANEL_STATUS == "year_outside_main_panel_coverage"
    )
  )

# Sparse event table: it includes only country-years with at least one NAVCO
# campaign. Zeroes are added later using an explicit country-year universe.
navco_country_year_events <- navco_campaign_year |>
  group_by(ISO_CODE_3, YEAR_INT) |>
  summarise(
    # Active-window counts and indicators. Treat as a proxy because EYEAR is
    # the peak year and is not always a true campaign end year.
    NAVCO_N_MASS_ACTIVE = n(),
    NAVCO_ANY_MASS_ACTIVE = 1L,
    NAVCO_N_VIOLENT_ACTIVE = sum(VIOL == 1L, na.rm = TRUE),
    NAVCO_ANY_VIOLENT_ACTIVE = any_int(VIOL == 1L),
    NAVCO_N_NONVIOLENT_ACTIVE = sum(NONVIOL == 1L, na.rm = TRUE),
    NAVCO_ANY_NONVIOLENT_ACTIVE = any_int(NONVIOL == 1L),
    
    NAVCO_N_INCUMBENT_ACTIVE = sum(TARGET_INCUMBENT_REMOVAL == 1L,
                                   na.rm = TRUE),
    NAVCO_ANY_INCUMBENT_ACTIVE = any_int(TARGET_INCUMBENT_REMOVAL == 1L),
    NAVCO_N_INCUMBENT_VIOLENT_ACTIVE = sum(
      TARGET_INCUMBENT_REMOVAL == 1L & VIOL == 1L, na.rm = TRUE
    ),
    NAVCO_ANY_INCUMBENT_VIOLENT_ACTIVE = any_int(
      TARGET_INCUMBENT_REMOVAL == 1L & VIOL == 1L
    ),
    NAVCO_N_INCUMBENT_NONVIOLENT_ACTIVE = sum(
      TARGET_INCUMBENT_REMOVAL == 1L & NONVIOL == 1L, na.rm = TRUE
    ),
    NAVCO_ANY_INCUMBENT_NONVIOLENT_ACTIVE = any_int(
      TARGET_INCUMBENT_REMOVAL == 1L & NONVIOL == 1L
    ),
    
    # Onset measures. These are preferred for temporal ordering in the hazard
    # model because they use the directly coded campaign start year.
    NAVCO_N_MASS_ONSET = sum(IS_ONSET == 1L, na.rm = TRUE),
    NAVCO_ANY_MASS_ONSET = any_int(IS_ONSET == 1L),
    NAVCO_N_VIOLENT_ONSET = sum(IS_ONSET == 1L & VIOL == 1L, na.rm = TRUE),
    NAVCO_ANY_VIOLENT_ONSET = any_int(IS_ONSET == 1L & VIOL == 1L),
    NAVCO_N_NONVIOLENT_ONSET = sum(IS_ONSET == 1L & NONVIOL == 1L,
                                   na.rm = TRUE),
    NAVCO_ANY_NONVIOLENT_ONSET = any_int(IS_ONSET == 1L & NONVIOL == 1L),
    NAVCO_N_INCUMBENT_ONSET = sum(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L, na.rm = TRUE
    ),
    NAVCO_ANY_INCUMBENT_ONSET = any_int(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L
    ),
    NAVCO_N_INCUMBENT_VIOLENT_ONSET = sum(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L & VIOL == 1L,
      na.rm = TRUE
    ),
    NAVCO_ANY_INCUMBENT_VIOLENT_ONSET = any_int(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L & VIOL == 1L
    ),
    NAVCO_N_INCUMBENT_NONVIOLENT_ONSET = sum(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L & NONVIOL == 1L,
      na.rm = TRUE
    ),
    NAVCO_ANY_INCUMBENT_NONVIOLENT_ONSET = any_int(
      IS_ONSET == 1L & TARGET_INCUMBENT_REMOVAL == 1L & NONVIOL == 1L
    ),
    
    # Exploratory free-text alternative for a broader governing-actor target.
    NAVCO_ANY_GOVACTOR_TEXT_ACTIVE = any_int(TARGET_GOVERNING_ACTOR_TEXT == 1L),
    NAVCO_ANY_GOVACTOR_TEXT_ONSET = any_int(
      IS_ONSET == 1L & TARGET_GOVERNING_ACTOR_TEXT == 1L
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Main-panel-compatible country-year data --------------------------------------
# ------------------------------------------------------------------------------
navco_source_start <- min(navco_campaign_clean$BYEAR, na.rm = TRUE)
navco_source_end   <- max(navco_campaign_clean$EYEAR, na.rm = TRUE)

# The source contains campaigns rather than a roster of all countries. Use the
# exact keys in the main analytical dataset rather than only countries observed
# in NAVCO. Add one synthetic pre-period year for countries observed in the
# first analysis year so strict one-year lags are available in that year.
navco_country_year_universe <- main_panel_analysis_keys |>
  bind_rows(
    main_panel_analysis_keys |>
      filter(YEAR_INT == analysis_start_year) |>
      mutate(YEAR_INT = analysis_start_year - 1L)
  ) |>
  distinct() |>
  arrange(ISO_CODE_3, YEAR_INT)

navco_measure_columns <- setdiff(
  names(navco_country_year_events), c("ISO_CODE_3", 
                                      "YEAR_INT")
)

navco_country_year <- navco_country_year_universe |>
  left_join(navco_country_year_events, by = c("ISO_CODE_3", 
                                              "YEAR_INT")) |>
  mutate(
    NAVCO_SOURCE_COVERAGE = as.integer(
      YEAR_INT >= navco_source_start & YEAR_INT <= navco_source_end
    ),
    across(
      all_of(navco_measure_columns),
      ~ if_else(
        NAVCO_SOURCE_COVERAGE == 1L,
        replace_na(as.integer(.x), 0L),
        NA_integer_
      )
    )
  ) |>
  group_by(ISO_CODE_3) |>
  arrange(YEAR_INT, .by_group = TRUE) |>
  mutate(
    NAVCO_INCUMBENT_NONVIOLENT_ONSET_L1 =
      lag(NAVCO_ANY_INCUMBENT_NONVIOLENT_ONSET, 1L),
    NAVCO_INCUMBENT_VIOLENT_ONSET_L1 =
      lag(NAVCO_ANY_INCUMBENT_VIOLENT_ONSET, 1L),
    NAVCO_ANY_MASS_ONSET_L1 = lag(NAVCO_ANY_MASS_ONSET, 1L),
    NAVCO_ANY_MASS_ACTIVE_L1 = lag(NAVCO_ANY_MASS_ACTIVE, 1L),
    NAVCO_INCUMBENT_NONVIOLENT_ACTIVE_L1 =
      lag(NAVCO_ANY_INCUMBENT_NONVIOLENT_ACTIVE, 1L),
    NAVCO_INCUMBENT_VIOLENT_ACTIVE_L1 =
      lag(NAVCO_ANY_INCUMBENT_VIOLENT_ACTIVE, 1L)
  ) |>
  ungroup() |>
  filter(YEAR_INT >= analysis_start_year) |>
  mutate(
    ISO_CODE_3 = as.character(ISO_CODE_3),
    YEAR = as.character(YEAR_INT)
  ) |>
  select(ISO_CODE_3, YEAR, everything(), -YEAR_INT)

# ------------------------------------------------------------------------------
# Final sanity checks ----------------------------------------------------------
# ------------------------------------------------------------------------------
# The final output must reproduce the requested main-panel keys exactly.
navco_output_keys <- navco_country_year |>
  transmute(
    ISO_CODE_3,
    YEAR_INT = as.integer(YEAR)
  )

missing_output_keys <- anti_join(
  main_panel_analysis_keys,
  navco_output_keys,
  by = c("ISO_CODE_3", "YEAR_INT")
)

extra_output_keys <- anti_join(
  navco_output_keys,
  main_panel_analysis_keys,
  by = c("ISO_CODE_3", "YEAR_INT")
)

# During source coverage, absence of a recorded NAVCO campaign is coded zero.
in_coverage_values <- navco_country_year |>
  filter(
    as.integer(YEAR) >= navco_source_start,
    as.integer(YEAR) <= navco_source_end
  ) |>
  select(all_of(navco_measure_columns)) |>
  unlist(use.names = FALSE)

# There must be no false zeroes after source coverage ends. A lagged measure can
# legitimately be observed in 2020 because its value comes from covered year
# 2019, so this check applies only to contemporaneous source measures.
post_coverage_values <- navco_country_year |>
  filter(as.integer(YEAR) > navco_source_end) |>
  select(all_of(navco_measure_columns)) |>
  unlist(use.names = FALSE)

# ------------------------------------------------------------------------------
# Save cleaned data and reconciliation audits ----------------------------------
# ------------------------------------------------------------------------------
#write_rds(
#  navco_campaign_clean,
#  file.path("navco_1_3_campaign_clean.rds")
#)

save(
  navco_country_year,
  file = "navco_1_3_country_year.RDS")

#write_csv(
#  manual_iso_reconciliation,
#  file.path("navco_manual_iso_reconciliation_detailed.csv")
#)

#write_csv(
#  manual_iso_reconciliation_summary,
#  file.path("navco_manual_iso_reconciliation_summary.csv")
#)

#write_csv(
#  manual_location_crosswalk,
#  file.path("navco_manual_location_crosswalk.csv")
#)

#write_csv(
#  duplicate_navco_ids,
#  file.path("navco_duplicate_source_ids.csv")
#)

message(
  "NAVCO pipeline completed. Country-year rows: ",
  format(nrow(navco_country_year), big.mark = ","),
  ". Manual campaign-years reconciled to the main panel: ",
  format(
    sum(
      manual_iso_reconciliation$ISO_PANEL_STATUS ==
        "reconciled_to_main_panel"
    ),
    big.mark = ","
  ),
  "."
)