rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)

# -----------------------------
# 0) Read raw GWF spell list
# -----------------------------
gwf_raw <- read_csv("gwf_raw.csv", show_col_types = FALSE) %>%
  select(-matches("^Unnamed"))

FINAL_YEAR <- 2010L

# -----------------------------
# 1) Normalize columns + set end_yr if missing
# -----------------------------
gwf_spells <- gwf_raw %>%
  transmute(
    country     = as.character(country),
    beg_yr      = as.integer(beg_yr),
    end_yr      = as.integer(end_yr),
    regime_type = str_to_lower(str_trim(as.character(regime_type)))
  ) %>%
  filter(!is.na(country), !is.na(beg_yr)) %>%
  mutate(
    end_yr = if_else(is.na(end_yr), FINAL_YEAR, end_yr),
    # clip to analysis window
    beg_yr = pmax(beg_yr, 1970L),
    end_yr = pmin(end_yr, FINAL_YEAR)
  ) %>%
  filter(beg_yr <= end_yr)

# -----------------------------
# 2) ISO3 codes (name fixes + manual historical)
# -----------------------------
name_fix <- c(
  "Cen African Rep" = "Central African Republic",
  "Congo/Zaire"     = "Congo, The Democratic Republic of the",
  "Congo-Brz"       = "Congo",
  "Dominican Rep"   = "Dominican Republic",
  "Korea North"     = "Korea, Democratic People's Republic of",
  "Korea South"     = "Korea, Republic of"
)

manual_iso3 <- c(
  "Czechoslovakia" = "CSK",
  "Germany East"   = "DDR",
  "South Vietnam"  = "VNM",
  "South Yemen"    = "YMD",
  "Yugoslavia"     = "YUG"
)

gwf_spells <- gwf_spells %>%
  mutate(
    country_cc = dplyr::coalesce(unname(name_fix[country]), country),
    ISO_CODE_3 = dplyr::coalesce(
      countrycode(country_cc, origin = "country.name", destination = "iso3c", warn = FALSE),
      unname(manual_iso3[country_cc])
    )
  )

# If anything still missing, inspect:
# gwf_spells %>% filter(is.na(ISO_CODE_3)) %>% distinct(country, country_cc)

# -----------------------------
# 3) Expand spells to country-year
# -----------------------------
gwf_long <- gwf_spells %>%
  mutate(
    duration = end_yr - beg_yr + 1L,
    spell_id = row_number()
  ) %>%
  tidyr::uncount(duration) %>%
  group_by(spell_id) %>%
  mutate(YEAR = beg_yr + row_number() - 1L) %>%
  ungroup() %>%
  select(ISO_CODE_3, YEAR, country = country_cc, regime_type)

# In a well-formed spell file, ISO3×YEAR should be unique.
# If duplicates exist (overlapping spells), keep the first and flag it.
dups <- gwf_long %>% count(ISO_CODE_3, YEAR) %>% filter(n > 1)
# print(dups)

gwf_long <- gwf_long %>%
  group_by(ISO_CODE_3, YEAR) %>%
  slice(1) %>%
  ungroup()

# -----------------------------
# 4) Balanced panel 1970–2010
# -----------------------------
gwf_panel <- expand_grid(
  ISO_CODE_3 = sort(unique(gwf_long$ISO_CODE_3)),
  YEAR       = 1970:FINAL_YEAR
) %>%
  left_join(gwf_long, by = c("ISO_CODE_3", "YEAR"))

# -----------------------------
# 5) Dummies with your logic:
#    - democracy => all autocracy indicators = 0
#    - autocracy components can overlap (hybrids), but only if autocracy
# -----------------------------
gwf_final <- gwf_panel %>%
  mutate(
    is_demo = !is.na(regime_type) & regime_type == "democracy",
    is_aut  = !is.na(regime_type) & regime_type != "democracy"
  ) %>%
  mutate(
    GWF_DEMOCRACY = if_else(is_demo, 1L, 0L, missing = NA_integer_),
    
    # "Any autocracy" (includes monarchy, provisional, etc. as autocracy unless you later exclude)
    GWF_AUTOCRACY = if_else(is_aut, 1L, 0L, missing = NA_integer_),
    
    # Component dummies (only meaningful within autocracy; forced 0 if democracy)
    GWF_AUTOCRACY_PARTY = case_when(
      is_demo ~ 0L,
      is_aut  ~ as.integer(str_detect(regime_type, "party")),
      TRUE    ~ NA_integer_
    ),
    GWF_MILITARY = case_when(
      is_demo ~ 0L,
      is_aut  ~ as.integer(str_detect(regime_type, "mil")),
      TRUE    ~ NA_integer_
    ),
    GWF_PERSONALIST = case_when(
      is_demo ~ 0L,
      is_aut  ~ as.integer(str_detect(regime_type, "personal")),
      TRUE    ~ NA_integer_
    )
  ) %>%
  select(
    ISO_CODE_3, YEAR, country, regime_type,
    GWF_DEMOCRACY, GWF_AUTOCRACY, GWF_AUTOCRACY_PARTY, GWF_MILITARY, GWF_PERSONALIST
  )

# -----------------------------
# 6) Sanity checks
# -----------------------------
# (a) democracy & autocracy never both 1
stopifnot(nrow(gwf_final %>% filter(GWF_DEMOCRACY == 1L, GWF_AUTOCRACY == 1L)) == 0)

# (b) in democratic years, components must be 0
stopifnot(nrow(gwf_final %>% filter(GWF_DEMOCRACY == 1L,
                                    (GWF_AUTOCRACY_PARTY != 0L |
                                       GWF_MILITARY != 0L |
                                       GWF_PERSONALIST != 0L))) == 0)

# (c) look at regime_type distribution
# gwf_final %>% count(regime_type, sort = TRUE)

gwf_final <- gwf_final |>
  mutate(YEAR = as.character(YEAR))

# Save if you want:
# write_csv(gwf_final, "gwf_1970_2010_clean.csv")
save(gwf_final, 
     file = "gwf_clean.RDS")
