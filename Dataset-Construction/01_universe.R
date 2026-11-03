# ==============================================================================
# 01_universe.R: the country-year universe of the panel ------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Rule: every V-Dem country-year in the analysis window with all four index
# components observed, from the year a country became sovereign, excluding
# entities that are not sovereign states.
#
# Output: data/intermediate/universe.rds (ISO_CODE_3, YEAR, COUNTRY_NAME)
# ==============================================================================

source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

# Entities excluded from the universe ------------------------------------------
# Non-sovereign or sub-national V-Dem units, plus Taiwan and the German
# Democratic Republic, which the legacy panel excluded. Set INCLUDE_TWN_DDR to
# TRUE to add the latter two (both are never fragile; percentiles shift a bit).
INCLUDE_TWN_DDR <- TRUE
EXCLUDED_ENTITIES <- c(
  PSE = "Palestine/West Bank", 
  PSG = "Palestine/Gaza", 
  SML = "Somaliland",
  HKG = "Hong Kong", 
  ZZB = "Zanzibar"
)
if (!INCLUDE_TWN_DDR) {
  EXCLUDED_ENTITIES <- c(EXCLUDED_ENTITIES, TWN = "Taiwan", DDR = "German Democratic Republic")
}

# First year of sovereignty for countries V-Dem covers before independence ----
# Countries not listed are included for the whole window.
SOVEREIGN_FROM <- c(
  GNB = 1974L,
  AGO = 1975L, CPV = 1975L, COM = 1975L, MOZ = 1975L, PNG = 1975L,
  STP = 1975L, SUR = 1975L,
  SYC = 1976L, DJI = 1977L, SLB = 1979L, VUT = 1980L,
  NAM = 1990L,
  ARM = 1991L, AZE = 1991L, BLR = 1991L, EST = 1991L, GEO = 1991L,
  KAZ = 1991L, KGZ = 1991L, LTU = 1991L, LVA = 1991L, MDA = 1991L,
  TJK = 1991L, TKM = 1991L, UKR = 1991L, UZB = 1991L,
  HRV = 1992L, SVN = 1992L,
  ERI = 1993L, TLS = 2002L, MNE = 2006L, XKX = 2008L
)

vdem <- load_vdem()
components <- unname(VDEM_COMPONENTS)

universe <- vdem |>
  as_tibble() |>
  transmute(ISO_CODE_3 = country_text_id, YEAR = as.integer(year),
            !!!rlang::syms(setNames(components, components))) |>
  filter(YEAR %in% ANALYSIS_YEARS,
         !ISO_CODE_3 %in% names(EXCLUDED_ENTITIES)) |>
  filter(if_all(all_of(components), ~ !is.na(.x))) |>
  mutate(SOVEREIGN_FROM = unname(SOVEREIGN_FROM[ISO_CODE_3])) |>
  filter(is.na(SOVEREIGN_FROM) | YEAR >= SOVEREIGN_FROM) |>
  select(ISO_CODE_3, YEAR)

# Country names: World Bank style names used throughout the paper and the
# analysis scripts (frozen crosswalk in data-raw/country_names.csv).
names_xwalk <- read_csv(raw_path("country_names.csv"), show_col_types = FALSE)
universe <- universe |>
  left_join(names_xwalk, by = "ISO_CODE_3") |>
  arrange(ISO_CODE_3, YEAR)

if (anyNA(universe$COUNTRY_NAME)) {
  stop("Countries without a name in data-raw/country_names.csv: ",
       paste(unique(universe$ISO_CODE_3[is.na(universe$COUNTRY_NAME)]), collapse = ", "))
}
assert_unique_keys(universe, label = "universe")

message(sprintf("Universe: %d countries, %d country-years, %d-%d",
                n_distinct(universe$ISO_CODE_3), nrow(universe),
                min(universe$YEAR), max(universe$YEAR)))

save_intermediate(universe, "universe")
