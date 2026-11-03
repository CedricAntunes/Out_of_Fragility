# ==============================================================================
# 13_navco.R: mass campaigns (NAVCO 1.3, Chenoweth and Shay 2020) --------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: NAVCO 1.3 campaign list (data-raw/navco/navco_1_3_dataverse_files.zip),
# maximalist campaigns, 1900-2019. Country-years after 2019 are NA.
#
# Definitions kept from the legacy navco.R:
#   * incumbent-targeting campaign = NAVCO REGCHANGE goal (regime change)
#   * onset = first campaign year (BYEAR); a campaign is active BYEAR..EYEAR
#   * location crosswalk below (territory conventions of the legacy script)
# Change: "East Germany" campaigns are mapped to DDR (outside the universe),
# not to DEU; the legacy script credited the 1989 East German campaign to
# (West) Germany.
#
# Variables:
#   NAVCO_N_INCUMBENT_ONSET              number of incumbent-targeting campaign
#                                        onsets (Table 4 predictor)
#   NAVCO_ANY_INCUMBENT_ONSET            1 if any such onset
#   NAVCO_ANY_INCUMBENT_NONVIOLENT_ONSET / _VIOLENT_ONSET   split by method
#   NAVCO_ANY_INCUMBENT_ACTIVE           1 if any incumbent-targeting campaign
#                                        is active
#   NAVCO_ANY_INCUMBENT_ONSET_RECENT3    any onset in t-2 .. t
#
# Output: data/intermediate/navco.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

zip <- raw_path("navco", "navco_1_3_dataverse_files.zip")
tmp <- tempfile("navco_"); dir.create(tmp)
utils::unzip(zip, files = "NAVCO 1.3 List.xlsx", exdir = tmp)
navco_raw <- read_excel(file.path(tmp, "NAVCO 1.3 List.xlsx"), sheet = 1)
names(navco_raw) <- toupper(gsub("[^A-Za-z0-9]+", "_", names(navco_raw)))

NAVCO_ISO <- c(
  "Bosnia-Herzegovina" = "BIH", "Burma" = "MMR", "Congo-Brazzaville (ROC)" = "COG",
  "Czechoslovakia" = "CZE", "Democratic Republic of Congo" = "COD",
  "East Germany" = "DDR", "East Timor" = "TLS", "Ivory Coast" = "CIV",
  "Kosovo" = "XKX", "Macedonia" = "MKD", "Namibia/South West Africa" = "NAM",
  "Natal" = "ZAF", "Northern Ireland" = "GBR", "Ottoman Empire" = "TUR",
  "Palestine" = "PSE", "Palestinian Territories" = "PSE",
  "Republic of the Congo" = "COG", "Republic of Yemen" = "YEM",
  "South Korea" = "KOR", "South Vietnam" = "VNM", "Swaziland" = "SWZ",
  "Tanzania/German East Africa" = "TZA", "Tibet" = "CHN", "USSR" = "RUS",
  "West Papua" = "IDN", "Western Sahara" = "MAR",
  "Yemen Arab Republic" = "YEM", "Yemen People's Republic" = "YEM",
  "Yugoslavia" = "SRB", "Zaire/DRC" = "COD"
)

campaigns <- navco_raw |>
  transmute(
    LOCATION = trimws(as.character(LOCATION)),
    BYEAR = as.integer(BYEAR), EYEAR = as.integer(EYEAR),
    VIOL = as.integer(VIOL), NONVIOL = as.integer(NONVIOL),
    INCUMBENT = as.integer(REGCHANGE)
  ) |>
  mutate(ISO_CODE_3 = coalesce(unname(NAVCO_ISO[LOCATION]),
                               countrycode(LOCATION, "country.name", "iso3c", warn = FALSE)))

stopifnot(!anyNA(campaigns$BYEAR), !anyNA(campaigns$EYEAR), all(campaigns$BYEAR <= campaigns$EYEAR),
          all(campaigns$VIOL + campaigns$NONVIOL == 1L), !anyNA(campaigns$INCUMBENT))
if (anyNA(campaigns$ISO_CODE_3)) stop("Unmapped NAVCO locations: ",
                                      paste(unique(campaigns$LOCATION[is.na(campaigns$ISO_CODE_3)]), collapse = "; "))

NAVCO_START <- min(campaigns$BYEAR)
NAVCO_END   <- max(campaigns$EYEAR)

campaign_years <- campaigns |>
  rowwise() |>
  reframe(ISO_CODE_3, BYEAR, VIOL, NONVIOL, INCUMBENT, YEAR = seq.int(BYEAR, EYEAR)) |>
  mutate(ONSET = as.integer(YEAR == BYEAR))

events <- campaign_years |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(
    NAVCO_N_INCUMBENT_ONSET = sum(ONSET == 1L & INCUMBENT == 1L),
    NAVCO_ANY_INCUMBENT_NONVIOLENT_ONSET = as.integer(any(ONSET == 1L & INCUMBENT == 1L & NONVIOL == 1L)),
    NAVCO_ANY_INCUMBENT_VIOLENT_ONSET = as.integer(any(ONSET == 1L & INCUMBENT == 1L & VIOL == 1L)),
    NAVCO_ANY_INCUMBENT_ACTIVE = as.integer(any(INCUMBENT == 1L)),
    .groups = "drop"
  )

universe <- read_intermediate("universe")
measures <- setdiff(names(events), c("ISO_CODE_3", "YEAR"))

navco <- universe |>
  distinct(ISO_CODE_3) |>
  tidyr::crossing(YEAR = (YEAR_START - 2L):YEAR_END) |>
  left_join(events, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(
    in_source = YEAR >= NAVCO_START & YEAR <= NAVCO_END,
    across(all_of(measures), ~ if_else(in_source, coalesce(as.integer(.x), 0L), NA_integer_)),
    NAVCO_ANY_INCUMBENT_ONSET = as.integer(NAVCO_N_INCUMBENT_ONSET > 0L)
  ) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(NAVCO_ANY_INCUMBENT_ONSET_RECENT3 = recent_any(NAVCO_ANY_INCUMBENT_ONSET, YEAR, 3L)) |>
  ungroup() |>
  filter(YEAR >= YEAR_START) |>
  select(-in_source)

message(sprintf("NAVCO source years: %d-%d", NAVCO_START, NAVCO_END))
assert_unique_keys(navco, label = "navco")
assert_binary(navco, setdiff(names(navco), c("ISO_CODE_3", "YEAR", "NAVCO_N_INCUMBENT_ONSET")), label = "navco")
save_intermediate(navco, "navco")
