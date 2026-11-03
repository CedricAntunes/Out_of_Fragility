# ==============================================================================
# 14_un_pko.R: UN peacekeeping operations (DPPA-DPO list of missions) ----------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: DPPADPOSS-PKO.csv (UN Department of Peace Operations mission list).
# The mission-to-host-country map is copied verbatim from the legacy
# un_pko_operations.R; missions without a host map entry are reported.
#
# Variables (host country-year; a mission is active from its start year to its
# end year, open missions through 2022):
#   UN_PKO_PRESENT_ANY      1 if any UN mission is active
#   UN_PKO_ONSET_YEAR       1 if a UN mission starts in year t
#   UN_PKO_ONSET_RECENT3    1 if a mission started in t-2 .. t   (Table 4)
#   UN_PKO_PRESENT_RECENT3  1 if a mission was active in t-2 .. t
#
# Output: data/intermediate/un_pko.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

pko_raw <- read_csv(raw_path("un_pko", "DPPADPOSS-PKO.csv"), show_col_types = FALSE)

missions <- pko_raw |>
  transmute(mission_acronym = trimws(mission_acronym),
            start_year = as.integer(substr(start_date, 1, 4)),
            end_year = as.integer(substr(end_date, 1, 4))) |>
  filter(!is.na(start_year)) |>
  mutate(end_year = pmin(coalesce(end_year, YEAR_END), YEAR_END))

un_pko_host_map <- tribble(
  ~mission_acronym, ~ISO_CODE_3, ~host_note,
  
  # Americas and Caribbean
  "DOMREP",    "DOM", "Dominican Republic",
  "MINUGUA",   "GTM", "Guatemala",
  "MINUJUSTH", "HTI", "Haiti",
  "MINUSTAH",  "HTI", "Haiti",
  "MIPONUH",   "HTI", "Haiti",
  "UNMIH",     "HTI", "Haiti",
  "UNSMIH",    "HTI", "Haiti",
  "UNTMIH",    "HTI", "Haiti",
  "ONUCA",     "CRI", "Central America",
  "ONUCA",     "SLV", "Central America",
  "ONUCA",     "GTM", "Central America",
  "ONUCA",     "HND", "Central America",
  "ONUCA",     "NIC", "Central America",
  "ONUSAL",    "SLV", "El Salvador",
  
  # Africa
  "MINUCI",    "CIV", "Cote d'Ivoire",
  "UNOCI",     "CIV", "Cote d'Ivoire",
  "MINURCA",   "CAF", "Central African Republic",
  "MINURCAT",  "CAF", "Central African Republic and Chad",
  "MINURCAT",  "TCD", "Central African Republic and Chad",
  "MINUSCA",   "CAF", "Central African Republic",
  "MINUSMA",   "MLI", "Mali",
  "MINURSO",   "ESH", "Western Sahara",
  "MONUA",     "AGO", "Angola",
  "UNAVEM I",  "AGO", "Angola",
  "UNAVEM II", "AGO", "Angola",
  "UNAVEM III","AGO", "Angola",
  "MONUC",     "COD", "Democratic Republic of the Congo",
  "MONUSCO",   "COD", "Democratic Republic of the Congo",
  "ONUB",      "BDI", "Burundi",
  "ONUC",      "COD", "Democratic Republic of the Congo",
  "ONUMOZ",    "MOZ", "Mozambique",
  "UNAMID",    "SDN", "Darfur, Sudan",
  "UNAMIR",    "RWA", "Rwanda",
  "UNAMSIL",   "SLE", "Sierra Leone",
  "UNOMSIL",   "SLE", "Sierra Leone",
  "UNASOG",    "TCD", "Aouzou Strip / Chad",
  "UNASOG",    "LBY", "Aouzou Strip / Libya",
  "UNISFA",    "SDN", "Abyei",
  "UNISFA",    "SSD", "Abyei",
  "UNMEE",     "ETH", "Ethiopia and Eritrea",
  "UNMEE",     "ERI", "Ethiopia and Eritrea",
  "UNMIL",     "LBR", "Liberia",
  "UNOMIL",    "LBR", "Liberia",
  "UNMIS",     "SDN", "Sudan",
  "UNMISS",    "SSD", "South Sudan",
  "UNOMUR",    "UGA", "Uganda-Rwanda border",
  "UNOMUR",    "RWA", "Uganda-Rwanda border",
  "UNOSOM I",  "SOM", "Somalia",
  "UNOSOM II", "SOM", "Somalia",
  "UNTAG",     "NAM", "Namibia",
  
  # Asia
  "UNAMIC",    "KHM", "Cambodia",
  "UNTAC",     "KHM", "Cambodia",
  "UNGOMAP",   "AFG", "Afghanistan and Pakistan",
  "UNGOMAP",   "PAK", "Afghanistan and Pakistan",
  "UNMOGIP",   "IND", "India and Pakistan",
  "UNMOGIP",   "PAK", "India and Pakistan",
  "UNIPOM",    "IND", "India and Pakistan",
  "UNIPOM",    "PAK", "India and Pakistan",
  "UNMISET",   "TLS", "Timor-Leste",
  "UNMIT",     "TLS", "Timor-Leste",
  "UNTAET",    "TLS", "Timor-Leste",
  "UNMOT",     "TJK", "Tajikistan",
  "UNSF",      "IDN", "West New Guinea / West Irian",
  
  # Middle East
  "UNDOF",     "SYR", "Golan Heights",
  "UNDOF",     "ISR", "Golan Heights",
  "UNEF I",    "EGY", "Egypt / Gaza / Israel",
  "UNEF I",    "ISR", "Egypt / Gaza / Israel",
  "UNEF II",   "EGY", "Egypt / Israel",
  "UNEF II",   "ISR", "Egypt / Israel",
  "UNIFIL",    "LBN", "Lebanon",
  "UNIIMOG",   "IRN", "Iran and Iraq",
  "UNIIMOG",   "IRQ", "Iran and Iraq",
  "UNIKOM",    "IRQ", "Iraq and Kuwait",
  "UNIKOM",    "KWT", "Iraq and Kuwait",
  "UNOGIL",    "LBN", "Lebanon",
  "UNSMIS",    "SYR", "Syria",
  "UNTSO",     "EGY", "Middle East truce supervision",
  "UNTSO",     "ISR", "Middle East truce supervision",
  "UNTSO",     "JOR", "Middle East truce supervision",
  "UNTSO",     "LBN", "Middle East truce supervision",
  "UNTSO",     "SYR", "Middle East truce supervision",
  "UNYOM",     "YEM", "Yemen",
  
  # Europe
  "UNCRO",     "HRV", "Croatia",
  "UNFICYP",   "CYP", "Cyprus",
  "UNMIBH",    "BIH", "Bosnia and Herzegovina",
  "UNMIK",     "XKX", "Kosovo",
  "UNMOP",     "HRV", "Prevlaka / Croatia",
  "UNPREDEP",  "MKD", "North Macedonia",
  "UNPROFOR",  "BIH", "Former Yugoslavia",
  "UNPROFOR",  "HRV", "Former Yugoslavia",
  "UNPROFOR",  "MKD", "Former Yugoslavia",
  "UNPSG",     "HRV", "Croatia",
  "UNOMIG",    "GEO", "Georgia",
  "UNTAES",    "HRV", "Eastern Slavonia / Croatia"
)

unmapped <- setdiff(unique(missions$mission_acronym), un_pko_host_map$mission_acronym)
if (length(unmapped)) message("Missions without a host-country map (excluded): ", paste(sort(unmapped), collapse = ", "))

mission_years <- missions |>
  inner_join(un_pko_host_map |> select(mission_acronym, ISO_CODE_3),
             by = "mission_acronym", relationship = "many-to-many") |>
  filter(end_year >= YEAR_START - 4L, start_year <= YEAR_END) |>
  rowwise() |>
  reframe(ISO_CODE_3, mission_acronym, start_year,
          YEAR = seq.int(max(start_year, YEAR_START - 4L), end_year)) |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(UN_PKO_PRESENT_ANY = 1L,
            UN_PKO_ONSET_YEAR = as.integer(any(YEAR == start_year)),
            .groups = "drop")

universe <- read_intermediate("universe")
un_pko <- universe |>
  distinct(ISO_CODE_3) |>
  tidyr::crossing(YEAR = (YEAR_START - 4L):YEAR_END) |>
  left_join(mission_years, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(across(c(UN_PKO_PRESENT_ANY, UN_PKO_ONSET_YEAR), ~ coalesce(.x, 0L))) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(UN_PKO_ONSET_RECENT3   = recent_any(UN_PKO_ONSET_YEAR, YEAR, 3L),
         UN_PKO_PRESENT_RECENT3 = recent_any(UN_PKO_PRESENT_ANY, YEAR, 3L)) |>
  ungroup() |>
  filter(YEAR >= YEAR_START)

assert_unique_keys(un_pko, label = "un_pko")
assert_binary(un_pko, setdiff(names(un_pko), c("ISO_CODE_3", "YEAR")), label = "un_pko")
save_intermediate(un_pko, "un_pko")
