# ==============================================================================
# 12_ucdp_troops.R: external states with troops in a country's conflicts -------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: UCDP/PRIO Armed Conflict Dataset v23.1 (side_a_2nd, side_b_2nd).
#
# Coding notes:
# N_TOTAL_TROOPS = number of DISTINCT third-party governments that UCDP records
# as secondary warring parties with troops, on either side, in any conflict
# active on the country's territory that year (all conflict types, >= 25
# deaths). The unit is states, not soldiers. UN missions are not secondary
# parties in UCDP, but member states fighting in coalitions are (e.g. Mali,
# CAR), so the count can be large.
#
# Output: data/intermediate/ucdp_troops.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

zip <- raw_path("ucdp", "ucdp-prio-acd-231-xlsx.zip")
tmp <- tempfile("acd_"); dir.create(tmp)
xlsx <- grep("UcdpPrioConflict.*\\.xlsx$", utils::unzip(zip, list = TRUE)$Name, value = TRUE)
stopifnot(length(xlsx) == 1L)
utils::unzip(zip, files = xlsx, exdir = tmp)
acd <- read_excel(file.path(tmp, xlsx))

UCDP_ISO <- c(
  "Bosnia-Herzegovina" = "BIH", "Cambodia (Kampuchea)" = "KHM", "Congo" = "COG",
  "DR Congo (Zaire)" = "COD", "Gambia" = "GMB", "Ivory Coast" = "CIV",
  "Madagascar (Malagasy)" = "MDG", "Myanmar (Burma)" = "MMR",
  "North Macedonia" = "MKD", "Russia (Soviet Union)" = "RUS",
  "Serbia (Yugoslavia)" = "SRB", "South Vietnam" = "VNM", "South Yemen" = "YEM",
  "United States of America" = "USA", "Yemen (North Yemen)" = "YEM",
  "Zimbabwe (Rhodesia)" = "ZWE"
)

split_gov <- function(x) {
  x <- ifelse(is.na(x), "", x)
  lapply(strsplit(x, ",\\s*(?=Government of)", perl = TRUE), function(v) trimws(v[nzchar(trimws(v))]))
}

rows <- acd |>
  mutate(year = as.integer(year)) |>
  filter(year >= YEAR_START - 1L, year <= YEAR_END) |>
  select(conflict_id, location, year, side_a, side_a_2nd, side_b_2nd) |>
  tidyr::separate_rows(location, sep = ",\\s*") |>
  mutate(location = trimws(location),
         ISO_CODE_3 = countrycode(location, "country.name", "iso3c",
                                  custom_match = UCDP_ISO, warn = FALSE))
if (anyNA(rows$ISO_CODE_3)) stop("Unmapped UCDP locations: ",
                                 paste(unique(rows$location[is.na(rows$ISO_CODE_3)]), collapse = "; "))

supporters <- rows |>
  mutate(gov = mapply(function(a, b) unique(c(a, b)),
                      split_gov(side_a_2nd), split_gov(side_b_2nd), SIMPLIFY = FALSE)) |>
  select(ISO_CODE_3, YEAR = year, gov) |>
  tidyr::unnest(gov)

# A government is never an "external" supporter in its own territory.
supporters <- supporters |>
  mutate(gov_iso = countrycode(sub("^Government of ", "", gov), "country.name", "iso3c",
                               custom_match = c("Serbia (Yugoslavia)" = "SRB",
                                                "Russia (Soviet Union)" = "RUS",
                                                "Cambodia (Kampuchea)" = "KHM",
                                                "Vietnam (North Vietnam)" = "VNM",
                                                "Zimbabwe (Rhodesia)" = "ZWE",
                                                "Yemen (North Yemen)" = "YEM",
                                                "DR Congo (Zaire)" = "COD",
                                                "Madagascar (Malagasy)" = "MDG",
                                                "Myanmar (Burma)" = "MMR",
                                                "Bosnia-Herzegovina" = "BIH",
                                                "Ivory Coast" = "CIV"),
                               warn = FALSE)) |>
  filter(is.na(gov_iso) | gov_iso != ISO_CODE_3)

troops <- supporters |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(N_TOTAL_TROOPS = n_distinct(gov), .groups = "drop")

# Country-years with an active conflict but no secondary state: zero.
active <- rows |> distinct(ISO_CODE_3, YEAR = year)
ucdp_troops <- active |>
  left_join(troops, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(N_TOTAL_TROOPS = coalesce(N_TOTAL_TROOPS, 0L))

assert_unique_keys(ucdp_troops, label = "ucdp_troops")
save_intermediate(ucdp_troops, "ucdp_troops")
