# ==============================================================================
# 08_reign.R: leadership change (REIGN leader list) ----------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: REIGN leader list (leader_list_8_21.csv; OEF Research), observed
# through August 2021.
#
# Variables:
#   REIGN_LEADER_CHANGE          1 if a new leader took office in year t (the
#                                first leader of each country series is not a
#                                change, since its predecessor is unobserved)
#   REIGN_LEADER_CHANGE_RECENT3  any change in t-2 .. t
#   REIGN_LEADER_CHANGE_RECENT5  any change in t-4 .. t
#   REIGN_LEADER_MILITARY        1 if the leader in power longest in year t had
#                                a military career
# All four are NA from 2021, because the source ends in August 2021.
#
# Changes from the legacy script:
#   * COW 345 (Yugoslavia / Serbia, continuous) is mapped to SRB. The legacy
#     script mapped it to "YUG", so Serbia had no leader data at all.
#   * Czechoslovakia (315) -> CZE and Yemen Arab Republic (678) -> YEM, the
#     V-Dem codes for those territories. Changes are counted within the ISO3
#     series, so the first leader of a successor state counts as a change.
#   * 2021-2022 are missing instead of carrying open spells forward as zeros.
#
# Output: data/intermediate/reign.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

REIGN_LAST_FULL_YEAR <- 2020L
REIGN_ISO <- c(`345` = "SRB", `347` = "XKX", `315` = "CZE", `316` = "CZE",
               `678` = "YEM", `679` = "YEM", `680` = "YMD", `260` = "DEU",
               `255` = "DEU", `265` = "DDR", `817` = "RVN", `816` = "VNM")

reign_raw <- read_csv(raw_path("reign", "leader_list_8_21.csv"),
                      locale = locale(encoding = "Latin1"), show_col_types = FALSE)

leaders <- reign_raw |>
  transmute(
    ccode = as.integer(ccode), leadid, leader,
    syear = as.integer(syear), smonth = as.integer(smonth),
    eyear = as.integer(eyear), emonth = as.integer(emonth),
    # REIGN codes -99 as unknown and 0.5 for ambiguous careers (5 leaders);
    # both are treated as missing.
    militarycareer = case_when(militarycareer %in% c(0, 1) ~ as.integer(militarycareer),
                               TRUE ~ NA_integer_)
  ) |>
  mutate(ISO_CODE_3 = coalesce(unname(REIGN_ISO[as.character(ccode)]),
                               countrycode(ccode, "cown", "iso3c", warn = FALSE))) |>
  filter(!is.na(ISO_CODE_3), !is.na(syear)) |>
  mutate(
    smonth = coalesce(smonth, 1L),
    eyear  = coalesce(eyear, 2021L),
    emonth = coalesce(emonth, 12L),
    start_m = syear * 12L + smonth - 1L,     # month index
    end_m   = eyear * 12L + emonth - 1L
  ) |>
  arrange(ISO_CODE_3, start_m, leadid) |>
  group_by(ISO_CODE_3) |>
  mutate(change_event = as.integer(row_number() > 1L)) |>
  ungroup()

# Leader changes by year
changes <- leaders |>
  filter(change_event == 1L) |>
  distinct(ISO_CODE_3, YEAR = syear) |>
  mutate(REIGN_LEADER_CHANGE = 1L)

# Main leader per year: most months in office in that year
main_leader <- leaders |>
  rowwise() |>
  reframe(ISO_CODE_3, militarycareer, leadid,
          m = seq.int(max(start_m, (YEAR_START - 5L) * 12L), min(end_m, (YEAR_END + 1L) * 12L - 1L))) |>
  mutate(YEAR = m %/% 12L) |>
  count(ISO_CODE_3, YEAR, leadid, militarycareer, name = "months") |>
  group_by(ISO_CODE_3, YEAR) |>
  arrange(desc(months), leadid, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(ISO_CODE_3, YEAR, REIGN_LEADER_MILITARY = militarycareer)

reign <- leaders |>
  distinct(ISO_CODE_3) |>
  tidyr::crossing(YEAR = (YEAR_START - 4L):YEAR_END) |>
  left_join(changes, by = c("ISO_CODE_3", "YEAR")) |>
  left_join(main_leader, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(REIGN_LEADER_CHANGE = coalesce(REIGN_LEADER_CHANGE, 0L),
         REIGN_LEADER_CHANGE = if_else(YEAR > REIGN_LAST_FULL_YEAR, NA_integer_, REIGN_LEADER_CHANGE)) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(
    REIGN_LEADER_CHANGE_RECENT3 = recent_any(REIGN_LEADER_CHANGE, YEAR, 3L),
    REIGN_LEADER_CHANGE_RECENT5 = recent_any(REIGN_LEADER_CHANGE, YEAR, 5L)
  ) |>
  ungroup() |>
  mutate(REIGN_LEADER_MILITARY = if_else(YEAR > REIGN_LAST_FULL_YEAR, NA_integer_, REIGN_LEADER_MILITARY)) |>
  filter(YEAR >= YEAR_START)

assert_unique_keys(reign, label = "reign")
assert_binary(reign, c("REIGN_LEADER_CHANGE", "REIGN_LEADER_CHANGE_RECENT3",
                       "REIGN_LEADER_CHANGE_RECENT5", "REIGN_LEADER_MILITARY"), label = "reign")
save_intermediate(reign, "reign")
