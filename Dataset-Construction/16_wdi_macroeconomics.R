# ==============================================================================
# 16_wdi.R: World Bank WDI economic indicators ---------------------------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Source: World Development Indicators, through the WDI package.
#
# Step 1, download. WDI is the one source pulled from a
# live API, so the download is saved with its date and the build always reads
# the saved file:
#   data-raw/wdi/wdi_YYYY-MM-DD.csv        one row per country-year
#   data-raw/wdi/wdi_YYYY-MM-DD_meta.csv   indicator codes, World Bank labels,
#                                          observation counts, package version
# The script downloads only if no dated file exists, or if REFRESH_WDI is
# TRUE. Commit both files after a download so every rebuild uses the same
# vintage. Downloading needs internet access and install.packages("WDI").
#
# Step 2, build. Reads the newest dated file and derives the variables.
#
# Variables:
#   GDP_PER_CAPITA, LOG_GDP_PER_CAPITA   GDP per capita, constant 2015 US$
#                                        (NY.GDP.PCAP.KD)
#   GDP_PER_CAPITA_GROWTH                GDP per capita growth, annual %
#                                        (NY.GDP.PCAP.KD.ZG)
#   GDP_DEFLATOR                         inflation, GDP deflator, annual %
#                                        (NY.GDP.DEFL.KD.ZG)
#   INFLATION_CRISIS                     1 if GDP_DEFLATOR > 40, the high-inflation
#                                        crisis threshold of Bruno and Easterly (1998)
#   ODA_PER_CAPITA, LOG_ODA_PER_CAPITA   net ODA received per capita, current US$
#                                        (DT.ODA.ODAT.PC.ZS); log is missing when
#                                        ODA <= 0
# GDP growth (NY.GDP.MKTP.KD.ZG) is downloaded, as in the original extract,
# but not used in the panel.
#
# Output: data/intermediate/wdi.rds
# ==============================================================================

# Helpers
source("D:/Users/cedric/Downloads/Out_of_Fragility_rebuild/out_of_fragility/R/00_config.R")

REFRESH_WDI <- FALSE   # TRUE: download a new vintage even if one is saved

WDI_INDICATORS <- c(
  GDP_PER_CAPITA        = "NY.GDP.PCAP.KD",
  GDP_PER_CAPITA_GROWTH = "NY.GDP.PCAP.KD.ZG",
  GDP_GROWTH            = "NY.GDP.MKTP.KD.ZG",
  GDP_DEFLATOR          = "NY.GDP.DEFL.KD.ZG",
  ODA_PER_CAPITA        = "DT.ODA.ODAT.PC.ZS"
)
WDI_START_YEAR <- YEAR_START - 1L   # one-year lags for the first panel year
WDI_END_YEAR   <- 2024L

INFLATION_CRISIS_THRESHOLD <- 40

WDI_DIR <- raw_path("wdi")
dir.create(WDI_DIR, recursive = TRUE, showWarnings = FALSE)

saved_vintages <- function() {
  sort(list.files(WDI_DIR, pattern = "^wdi_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE))
}

# Step 1: download --------------------------------------------------------------
download_wdi <- function() {
  if (!requireNamespace("WDI", quietly = TRUE)) {
    stop("No saved WDI file in data-raw/wdi/ and the WDI package is not installed. ",
         "Run install.packages(\"WDI\") and rerun this script.", call. = FALSE)
  }
  message("Downloading WDI indicators ", WDI_START_YEAR, "-", WDI_END_YEAR, " from the World Bank")
  raw <- WDI::WDI(country = "all", indicator = WDI_INDICATORS,
                  start = WDI_START_YEAR, end = WDI_END_YEAR,
                  extra = TRUE, language = "en")

  missing_ind <- setdiff(names(WDI_INDICATORS), names(raw))
  if (length(missing_ind) > 0L) {
    stop("The World Bank API returned no data for ",
         paste(WDI_INDICATORS[missing_ind], collapse = ", "), ". Try again later.", call. = FALSE)
  }

  # Series labels are column attributes; keep them before filtering rows.
  labels <- vapply(names(WDI_INDICATORS), function(v) {
    lab <- attr(raw[[v]], "label")
    if (is.null(lab)) NA_character_ else as.character(lab)[1]
  }, character(1))

  # Countries only: regional and income aggregates have region "Aggregates".
  raw <- raw[(is.na(raw$region) | raw$region != "Aggregates") &
               !is.na(raw$iso3c) & raw$iso3c != "", ]

  out <- data.frame(ISO_CODE_3 = raw$iso3c, COUNTRY_WB = raw$country,
                    YEAR = as.integer(raw$year), raw[names(WDI_INDICATORS)],
                    row.names = NULL)
  out <- out[order(out$ISO_CODE_3, out$YEAR), ]
  assert_unique_keys(out, label = "wdi download")

  stamp <- format(Sys.Date(), "%Y-%m-%d")
  data_file <- file.path(WDI_DIR, paste0("wdi_", stamp, ".csv"))
  utils::write.csv(out, data_file, row.names = FALSE, na = "")
  utils::write.csv(
    data.frame(variable = names(WDI_INDICATORS), indicator = unname(WDI_INDICATORS),
               label = unname(labels),
               non_missing = vapply(names(WDI_INDICATORS), function(v) sum(!is.na(out[[v]])), integer(1)),
               downloaded = stamp, wdi_package = as.character(utils::packageVersion("WDI")),
               row.names = NULL),
    file.path(WDI_DIR, paste0("wdi_", stamp, "_meta.csv")), row.names = FALSE)
  message("Saved ", nrow(out), " country-years to ", data_file, ". Commit it with its _meta.csv.")
}

if (REFRESH_WDI || length(saved_vintages()) == 0L) download_wdi()

# Step 2: build -------------------------------------------------------------------
src <- utils::tail(saved_vintages(), 1L)
message("WDI source: ", basename(src))

wdi <- read_csv(src, show_col_types = FALSE) |>
  transmute(ISO_CODE_3, YEAR = as.integer(YEAR), GDP_PER_CAPITA, GDP_PER_CAPITA_GROWTH,
            GDP_DEFLATOR, ODA_PER_CAPITA) |>
  filter(!is.na(ISO_CODE_3), YEAR >= WDI_START_YEAR, YEAR <= YEAR_END) |>
  mutate(
    LOG_GDP_PER_CAPITA = log(if_else(GDP_PER_CAPITA > 0, GDP_PER_CAPITA, NA_real_)),
    LOG_ODA_PER_CAPITA = log(if_else(ODA_PER_CAPITA > 0, ODA_PER_CAPITA, NA_real_)),
    INFLATION_CRISIS   = as.integer(GDP_DEFLATOR > INFLATION_CRISIS_THRESHOLD)
  )

assert_unique_keys(wdi, label = "wdi")
assert_binary(wdi, "INFLATION_CRISIS", label = "wdi")
save_intermediate(wdi, "wdi")
