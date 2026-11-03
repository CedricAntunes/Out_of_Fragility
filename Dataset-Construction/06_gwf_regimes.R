# ==============================================================================
# 06_gwf.R: Geddes, Wright and Frantz political regimes (1946-2010) ------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: GWF Autocratic Regimes 1.2, official country-year file
# GWF_AllPoliticalRegimes.dta (data-raw/gwf). Coverage ends in 2010.
#
# Variables (NA outside GWF coverage):
#   GWF_DEMOCRACY    1 if gwf_nonautocracy == "democracy", else 0
#   GWF_PARTY        1 if gwf_regimetype contains "party" (party, party-military,
#                    party-personal, party-military-personal)
#   GWF_MILITARY     1 if it contains "military" (military, indirect military,
#                    military-personal, party-military, party-military-personal)
#   GWF_PERSONALIST  1 if it contains "personal" (personal and every hybrid
#                    with a personal component)
#
# Notes on coding:
# The flags are inclusive of hybrids, so they overlap. They are built from
# gwf_regimetype rather than the file's gwf_party / gwf_military / gwf_personal
# dummies, which are mutually exclusive (each hybrid is assigned to one
# category, oligarchy is counted as party, and gwf_personal = 1 only for pure
# personalist regimes). Monarchy and oligarchy get 0 on all three flags.
# Subtype flags are 0 in democracies and other non-autocratic years.
#
# Changes from the legacy GWF_cleanning.R, which expanded a hand-built spell
# list (gwf_raw.csv, kept in data-raw/gwf for reference):
#   * The official country-year file is used. The spell list had no regime
#     type for Iran after 1979 and missed the party-military and
#     party-personal hybrids in the party flag.
#   * Years GWF does not code stay missing; the legacy script coded them 0.
#   * Yugoslavia and Czechoslovakia map to SRB and CZE, the V-Dem codes for
#     those territories; for years with two units the successor is kept.
#
# Output: data/intermediate/gwf.rds
# ==============================================================================

# Helpers 
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

GWF_ISO <- c(`345` = "SRB", `347` = "SRB", `315` = "CZE", `316` = "CZE",
             `678` = "YEM", `679` = "YEM", `680` = "YMD", `260` = "DEU", `255` = "DEU",
             `265` = "DDR", `816` = "VNM", `818` = "VNM", `817` = "RVN",
             `529` = "ETH", `530` = "ETH")
GWF_PRIORITY <- c(`347` = 2, `260` = 2, `678` = 2, `530` = 2, `816` = 2)  # predecessor loses ties

gwf_raw <- haven::read_dta(raw_path("gwf", "GWF_AllPoliticalRegimes.dta"))

gwf <- gwf_raw |>
  transmute(cowcode = as.integer(cowcode), YEAR = as.integer(year),
            regimetype = as.character(gwf_regimetype),
            nonaut = as.character(gwf_nonautocracy)) |>
  filter(YEAR >= YEAR_START - 5L) |>
  mutate(ISO_CODE_3 = coalesce(unname(GWF_ISO[as.character(cowcode)]),
                               countrycode(cowcode, "cown", "iso3c", warn = FALSE)),
         priority = coalesce(unname(GWF_PRIORITY[as.character(cowcode)]), 1)) |>
  filter(!is.na(ISO_CODE_3)) |>
  group_by(ISO_CODE_3, YEAR) |>
  arrange(priority, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

coded <- !is.na(gwf$regimetype) & gwf$regimetype != "" | !is.na(gwf$nonaut) & gwf$nonaut != ""
gwf <- gwf |>
  mutate(
    coded = coded,
    autocracy = !is.na(regimetype) & regimetype != "",
    GWF_DEMOCRACY   = if_else(coded, as.integer(coalesce(nonaut, "") == "democracy"), NA_integer_),
    GWF_PARTY       = if_else(coded, as.integer(autocracy & grepl("party", regimetype)), NA_integer_),
    GWF_MILITARY    = if_else(coded, as.integer(autocracy & grepl("military", regimetype)), NA_integer_),
    GWF_PERSONALIST = if_else(coded, as.integer(autocracy & grepl("personal", regimetype)), NA_integer_)
  ) |>
  select(ISO_CODE_3, YEAR, GWF_DEMOCRACY, GWF_PARTY, GWF_MILITARY, GWF_PERSONALIST)

assert_unique_keys(gwf, label = "gwf")
assert_binary(gwf, c("GWF_DEMOCRACY", "GWF_PARTY", "GWF_MILITARY", "GWF_PERSONALIST"), label = "gwf")
save_intermediate(gwf, "gwf")
