# ==============================================================================
# 03_wgi_index.R: the WGI fragility index (robustness, Appendix B) -------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# The WGI index is the first principal component of control of corruption,
# rule of law, government effectiveness, and political stability / absence of
# violence (and the same without rule of law). 
#
# WGI was not published for 1997, 1999 and 2001; those years stay missing here.
# Status uses the same annual P30 / P35 rule as the V-Dem index, computed over
# the universe country-years with a WGI score.
#
# Output: data/intermediate/wgi_index.rds
# ==============================================================================

source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

universe <- read_intermediate("universe")

wgi_iso <- function(name) {
  countrycode(name, "country.name", "iso3c", warn = FALSE,
              custom_match = c("Kosovo" = "XKX", "Netherlands Antilles (former)" = "ANT",
                               "Turkey" = "TUR"))
}

read_wgi <- function(file, var) {
  read_excel(raw_path("wgi", file)) |>
    transmute(ISO_CODE_3 = wgi_iso(`Country Name`), YEAR = as.integer(Year),
              !!var := `Loading Factor 1 Normalized`) |>
    filter(!is.na(ISO_CODE_3))
}

wgi <- read_wgi("wgi - PCA data.xlsx", "WGI_INDEX") |>
  full_join(read_wgi("PCA-WGI without RoL.xlsx", "WGI_INDEX_NORL"),
            by = c("ISO_CODE_3", "YEAR"))
assert_unique_keys(wgi, label = "wgi")

wgi_index <- universe |>
  select(ISO_CODE_3, YEAR) |>
  left_join(wgi, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(
    WGI_STATUS      = annual_status(WGI_INDEX,      YEAR),
    WGI_STATUS_NORL = annual_status(WGI_INDEX_NORL, YEAR)
  )

message(sprintf("WGI coverage: %d of %d universe country-years (%d-%d)",
                sum(!is.na(wgi_index$WGI_INDEX)), nrow(wgi_index),
                min(wgi_index$YEAR[!is.na(wgi_index$WGI_INDEX)]),
                max(wgi_index$YEAR[!is.na(wgi_index$WGI_INDEX)])))

save_intermediate(wgi_index, "wgi_index")
