# ==============================================================================
# 15_ccp.R: new constitutions (Comparative Constitutions Project, CCP v6) ------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: ccpcce_v6.zip (Chronology of Constitutional Events).
#
# NEW_CONSTITUTION = 1 if CCP records a "new" constitution event in the
# country-year; 0 in other years covered by CCP; NA after the last CCP year.
#
# Changes from the legacy ccp_constitutions.R:
#   * No longer reads standardized_data_final.RDS (the legacy script read the
#     final output it fed, a circular dependency). It uses the universe.
#   * Territory mapping follows V-Dem: Yugoslavia (COW 345) -> SRB (the legacy
#     crosswalk listed only COW 340, so Yugoslav events were dropped);
#     East Germany (265), South Yemen (680) and South Vietnam (817) map to
#     their own codes (outside the universe) instead of being credited to
#     Germany, Yemen and Vietnam.
#
# Output: data/intermediate/ccp.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

zip <- raw_path("ccp", "ccpcce_v6.zip")
member <- grep("^ccpcce_v6/ccpcce/ccpcce_v6\\.csv$", utils::unzip(zip, list = TRUE)$Name, value = TRUE)
stopifnot(length(member) == 1L)
tmp <- tempfile("ccp_"); dir.create(tmp)
utils::unzip(zip, files = member, exdir = tmp)
ccp_raw <- read_csv(file.path(tmp, member), show_col_types = FALSE,
                    col_types = cols(cowcode = col_integer(), year = col_integer(),
                                     evnttype = col_character(), .default = col_guess()))

CCP_ISO <- c(`260` = "DEU", `255` = "DEU", `265` = "DDR", `315` = "CZE", `316` = "CZE",
             `340` = "SRB", `345` = "SRB", `341` = "MNE", `343` = "MKD", `344` = "HRV",
             `346` = "BIH", `347` = "XKX", `349` = "SVN", `365` = "RUS",
             `678` = "YEM", `679` = "YEM", `680` = "YMD",
             `816` = "VNM", `818` = "VNM", `817` = "RVN", `860` = "TLS")

ccp <- ccp_raw |>
  transmute(COW = cowcode, YEAR = year, NEW = as.integer(tolower(trimws(evnttype)) == "new")) |>
  mutate(ISO_CODE_3 = coalesce(unname(CCP_ISO[as.character(COW)]),
                               countrycode(COW, "cown", "iso3c", warn = FALSE))) |>
  filter(!is.na(ISO_CODE_3))

CCP_LAST_YEAR <- max(ccp$YEAR)

events <- ccp |>
  group_by(ISO_CODE_3, YEAR) |>
  summarise(NEW_CONSTITUTION = as.integer(any(NEW == 1L)), .groups = "drop")

universe <- read_intermediate("universe")
ccp_panel <- universe |>
  select(ISO_CODE_3, YEAR) |>
  left_join(events, by = c("ISO_CODE_3", "YEAR")) |>
  mutate(NEW_CONSTITUTION = if_else(YEAR <= CCP_LAST_YEAR,
                                    coalesce(NEW_CONSTITUTION, 0L), NA_integer_))

message(sprintf("CCP covers through %d", CCP_LAST_YEAR))
assert_unique_keys(ccp_panel, label = "ccp")
assert_binary(ccp_panel, "NEW_CONSTITUTION", label = "ccp")
save_intermediate(ccp_panel, "ccp")
