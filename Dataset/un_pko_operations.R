# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: June, 2026 -------------------------------------------------------------

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packages 
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(slider)
})

# ------------------------------------------------------------------------------
# Loading the data -------------------------------------------------------------
# ------------------------------------------------------------------------------
un_pko_raw <- readr::read_csv(
  "D:/Users/cedric/Downloads/DPPADPOSS-PKO.csv",
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
) |>
  clean_names()

# Inspecting columns
names(un_pko_raw)

# ------------------------------------------------------------------------------
# Cleaning the data ------------------------------------------------------------
# ------------------------------------------------------------------------------
panel_start <- lubridate::make_date(1971, 1, 1)
panel_end   <- lubridate::make_date(2022, 12, 31)

un_pko_missions <- un_pko_raw |>
  mutate(
    mission_acronym = str_squish(mission_acronym),
    mission_name = str_squish(mission_name),
    mission_isactive = str_to_upper(str_squish(mission_isactive)),
    
    start_date = as.Date(start_date),
    end_date_raw = as.Date(end_date),
    
    # Active missions or missions ending after 2022 are censored at the end
    # of the analysis panel
    end_date = case_when(
      is.na(end_date_raw) ~ panel_end,
      end_date_raw > panel_end ~ panel_end,
      TRUE ~ end_date_raw
    ),
    
    start_year = year(start_date),
    end_year = year(end_date),
    
    # Coordinates in the UN file use decimal commas.
    mission_longitude_num = as.numeric(str_replace(mission_longitude, ",", ".")),
    mission_latitude_num  = as.numeric(str_replace(mission_latitude, ",", "."))
  ) |>
  filter(!is.na(start_date), !is.na(end_date)) |>
  filter(end_date >= panel_start, start_date <= panel_end)

# ------------------------------------------------------------------------------
# Manual mission-to-host-country crosswalk -------------------------------------
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Diagnostics: checking whether every mission has a host mapping ---------------
# ------------------------------------------------------------------------------
missions_without_host <- un_pko_missions |>
  distinct(mission_acronym, mission_name) |>
  anti_join(
    un_pko_host_map |> distinct(mission_acronym),
    by = "mission_acronym"
  ) |>
  arrange(mission_acronym)

# we are good to go!
print(missions_without_host, n = Inf)

# ------------------------------------------------------------------------------
# Join: host countries and classifying legacy monitoring missions --------------
# ------------------------------------------------------------------------------
# Long-running observer/buffer missions can behave differently from the kind of
# postwar reconstruction or state-stabilization missions most relevant to the
# qualitative story. We keep the all-missions measure, but also create a
# nonlegacy version that excludes several long-running status-quo observer missions.
legacy_monitoring_missions <- c(
  "UNTSO",
  "UNMOGIP",
  "UNFICYP",
  "UNDOF",
  "MINURSO"
)

un_pko_host_missions <- un_pko_missions |>
  left_join(un_pko_host_map, by = "mission_acronym") |>
  mutate(
    pko_legacy_monitoring = as.integer(mission_acronym %in% legacy_monitoring_missions),
    pko_nonlegacy = as.integer(pko_legacy_monitoring == 0)
  )

# ------------------------------------------------------------------------------
# Expanding mission spells to country-year data --------------------------------
# ------------------------------------------------------------------------------
un_pko_country_year_long <- un_pko_host_missions |>
  mutate(
    active_start_year = pmax(year(start_date), 1971),
    active_end_year   = pmin(year(end_date), 2022)
  ) |>
  filter(active_start_year <= active_end_year) |>
  rowwise() |>
  mutate(
    YEAR = list(seq.int(active_start_year, active_end_year))
  ) |>
  ungroup() |>
  unnest(YEAR) |>
  mutate(
    un_pko_present_any = 1L,
    un_pko_present_nonlegacy = as.integer(pko_nonlegacy == 1L),
    
    # Mission onset is coded in the actual start year, as long as it falls
    # within the analysis panel.
    un_pko_onset_year = as.integer(YEAR == year(start_date) & YEAR >= 1971 & YEAR <= 2022),
    
    un_pko_onset_nonlegacy_year = as.integer(
      un_pko_onset_year == 1L & pko_nonlegacy == 1L
    )
  )

# ------------------------------------------------------------------------------
# Collapsing to one row per ISO_CODE_3-YEAR ------------------------------------
# ------------------------------------------------------------------------------
un_pko_country_year_observed <- un_pko_country_year_long |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(
    un_pko_present_any = as.integer(any(un_pko_present_any == 1L, na.rm = TRUE)),
    un_pko_present_nonlegacy = as.integer(any(un_pko_present_nonlegacy == 1L, na.rm = TRUE)),
    un_pko_onset_year = as.integer(any(un_pko_onset_year == 1L, na.rm = TRUE)),
    un_pko_onset_nonlegacy_year = as.integer(any(un_pko_onset_nonlegacy_year == 1L, na.rm = TRUE)),
    
    n_un_pko_missions = n_distinct(mission_acronym),
    n_un_pko_missions_nonlegacy = n_distinct(mission_acronym[pko_nonlegacy == 1L]),
    
    n_un_pko_onsets_year = n_distinct(mission_acronym[un_pko_onset_year == 1L]),
    n_un_pko_onsets_nonlegacy_year = n_distinct(
      mission_acronym[un_pko_onset_nonlegacy_year == 1L]
    ),
    
    un_pko_missions_active = paste(sort(unique(mission_acronym)), collapse = "; "),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Building balanced host-country-year panel, 1971-2022 -------------------------
# ------------------------------------------------------------------------------
# This grid includes all countries that ever host or are directly affected by
# a mapped UN peacekeeping mission. Countries with no UN PKO exposure will be
# added as zeros after merging into the main analysis dataframe.
un_pko_country_year <- un_pko_host_map |>
  distinct(ISO_CODE_3) |>
  crossing(YEAR = 1971:2022) |>
  left_join(un_pko_country_year_observed, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(
    across(
      c(
        un_pko_present_any,
        un_pko_present_nonlegacy,
        un_pko_onset_year,
        un_pko_onset_nonlegacy_year,
        n_un_pko_missions,
        n_un_pko_missions_nonlegacy,
        n_un_pko_onsets_year,
        n_un_pko_onsets_nonlegacy_year
      ),
      ~ replace_na(.x, 0L)
    ),
    un_pko_missions_active = replace_na(un_pko_missions_active, "")
  ) |>
  arrange(ISO_CODE_3, YEAR) |>
  group_by(ISO_CODE_3) |>
  mutate(
    # Current-year windows
    un_pko_onset_recent3 = slider::slide_int(
      un_pko_onset_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 2,
      .complete = FALSE
    ),
    
    un_pko_onset_recent5 = slider::slide_int(
      un_pko_onset_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    ),
    
    un_pko_onset_nonlegacy_recent3 = slider::slide_int(
      un_pko_onset_nonlegacy_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 2,
      .complete = FALSE
    ),
    
    un_pko_onset_nonlegacy_recent5 = slider::slide_int(
      un_pko_onset_nonlegacy_year,
      .f = ~ max(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    )
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# Diagnostics ------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Checking uniqueness
duplicates_check <- un_pko_country_year |>
  count(ISO_CODE_3, YEAR) |>
  filter(n > 1)

# We are good to go!
print(duplicates_check, n = Inf)

names(un_pko_country_year) <- toupper(names(un_pko_country_year))

un_pko_country_year <- un_pko_country_year |>
  mutate(YEAR = as.character(YEAR))

# Saving the data --------------------------------------------------------------
save(un_pko_country_year,
     file = "un_pko_country_year.RDS")