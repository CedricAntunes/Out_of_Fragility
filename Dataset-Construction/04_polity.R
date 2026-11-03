# ==============================================================================
# 04_polity.R: Polity5 regime variables ----------------------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: Polity5 (p5v2018.xls; Marshall 2020), country-year, coverage to 2018.
#
# Variables:
#   REGIME_DURABILITY_YEARS              durable: years since the last regime
#                                        change (3-point Polity shift)
#   PARTIAL_DEMOCRACY_WITH_FACTIONALISM  1 if exrec in 6-8 and parcomp == 3
#                                        (Goldstone et al. 2010)
#   POLITY2                              polity2, -10 to 10
#   POLITY_DEMOCRACY_SCORE               democ, 0-10
#   POLITY_AUTOCRACY_SCORE               autoc, 0-10
# Polity's special codes (-66 interruption, -77 interregnum, -88 transition)
# are set to missing in democ and autoc.
#
# Territory mapping: Polity codes predecessor states separately. They are
# mapped to the ISO3 code that V-Dem uses for the same territory, so that
# Polity follows the universe:
#   Yugoslavia (YUG, 345) 1971-1991 and FRY / Serbia and Montenegro (YGS, 347)
#   1992-2005 -> SRB; Czechoslovakia (315) -> CZE; Yemen North (678) -> YEM;
#   Kosovo (341) -> XKX.
# This replaces the separate Serbia patch (Data-Construction/Serbia_fix.R).
# Where two Polity rows map to the same ISO3-year (state births, unifications,
# secessions), the successor polity is kept (e.g. unified Germany in 1990,
# Sudan-North in 2011).
#
# Output: data/intermediate/polity.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

# Raw data
polity_raw <- read_excel(raw_path("polity", "p5v2018.xls"))

# ccode -> ISO3 for predecessor polities; all others mapped from the name.
PREDECESSOR_ISO <- c(`345` = "SRB", `347` = "SRB", `315` = "CZE", `678` = "YEM",
                     `341` = "XKX")
# Successor preference when two polities map to the same ISO3-year.
# Lower number = preferred.
SUCCESSOR_PRIORITY <- c(`255` = 1, `260` = 2,   # Germany 1990: unified over West
                        `529` = 1, `530` = 2,   # Ethiopia 1993: post-1993 over pre
                        `626` = 1, `625` = 2,   # Sudan 2011: Sudan-North over Sudan
                        `818` = 1, `816` = 2,   # Vietnam 1976: unified over North
                        `342` = 1, `347` = 2,   # Serbia 2006: Serbia over S&M
                        `679` = 1, `678` = 2)   # Yemen 1990: Yemen over Yemen North

polity <- polity_raw |>
  mutate(ccode = as.integer(ccode), year = as.integer(year)) |>
  filter(year >= YEAR_START - 5L, year <= YEAR_END) |>
  mutate(
    ISO_CODE_3 = coalesce(
      unname(PREDECESSOR_ISO[as.character(ccode)]),
      countrycode(country, "country.name", "iso3c", warn = FALSE)
    )
  ) |>
  filter(!is.na(ISO_CODE_3))

# Unmapped Polity units: Germany East, South Vietnam, Yemen South. These are
# outside the universe (DDR excluded; the others are not V-Dem universe units).

# Resolve duplicated ISO3-years -------------------------------------------------
# For 1991 Yugoslavia: SFRY (345) exists for most of 1991; FRY (347) is coded
# from 1991. We keep SFRY for 1991 and FRY from 1992, as V-Dem does.
polity <- polity |>
  filter(!(ccode == 347 & year == 1991L)) |>
  mutate(priority = coalesce(unname(SUCCESSOR_PRIORITY[as.character(ccode)]), 1)) |>
  group_by(ISO_CODE_3, year) |>
  arrange(priority, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

special <- function(x) ifelse(x %in% c(-66, -77, -88), NA_real_, as.numeric(x))

polity <- polity |>
  transmute(
    ISO_CODE_3,
    YEAR = year,
    REGIME_DURABILITY_YEARS = as.numeric(durable),
    PARTIAL_DEMOCRACY_WITH_FACTIONALISM = as.integer(exrec %in% 6:8 & parcomp == 3),
    POLITY2 = as.numeric(polity2),
    POLITY_DEMOCRACY_SCORE = special(democ),
    POLITY_AUTOCRACY_SCORE = special(autoc)
  )

assert_unique_keys(polity, label = "polity")
assert_binary(polity, "PARTIAL_DEMOCRACY_WITH_FACTIONALISM", label = "polity")

save_intermediate(polity, "polity")
