# ==============================================================================
# 00_config.R: settings and helpers shared by every pipeline script ------------
# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October, 2026 ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Sourced at the top of each numbered script. Defines paths, the analysis
# window, source versions, and the small helpers that more than one module
# uses (rolling windows, annual percentile status, first-exit identifier).
# ==============================================================================

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(countrycode)
})

# Paths ------------------------------------------------------------------------
# All paths are relative to the repository root, which is located through the
# `.here` marker file. Run scripts from anywhere inside the repository.
REPO_ROOT <- here::here()
RAW_DIR   <- file.path(REPO_ROOT, "data-raw")
INT_DIR   <- file.path(REPO_ROOT, "data", "intermediate")
FINAL_DIR <- file.path(REPO_ROOT, "data", "final")
dir.create(INT_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(FINAL_DIR, recursive = TRUE, showWarnings = FALSE)

raw_path <- function(...) file.path(RAW_DIR, ...)
int_path <- function(name) file.path(INT_DIR, paste0(name, ".rds"))

# Analysis window --------------------------------------------------------------
YEAR_START <- 1971L
YEAR_END   <- 2022L
ANALYSIS_YEARS <- YEAR_START:YEAR_END

# Source versions --------------------------------------------------------------
# V-Dem is read from the vdemdata package data file at a pinned commit; see
# data-raw/README.md for how to fetch it. v16 is the primary release; v14 is
# kept only to reproduce the legacy (pre-November 2026) index.
VDEM_VERSION        <- "v16"
VDEM_FILE           <- raw_path("vdem", "vdem_v16.RData")
VDEM_LEGACY_FILE    <- raw_path("vdem", "vdem_v14.RData")

# Fragility index settings -----------------------------------------------------
# Components are oriented so that higher values mean LESS fragility.
VDEM_COMPONENTS <- c(
  VDEM_REGIME_CORRUPTION  = "v2xnp_regcorr",  # reversed before the PCA
  VDEM_POLITICAL_VIOLENCE = "v2caviol",       # orientation checked in 02
  VDEM_RULE_OF_LAW        = "v2x_rule",
  VDEM_IMPARTIAL_ADMIN    = "v2clrspct"
)
STATUS_LOW_PERCENTILE  <- 0.30   # at or below: Fragile
STATUS_HIGH_PERCENTILE <- 0.35   # above: Non Fragile; in between: Transitioning
QUANTILE_TYPE          <- 7L     # R default; equals pandas linear interpolation

STATUS_LABELS <- c(fragile = "Fragile", transitioning = "Transitioning",
                   nonfragile = "Non Fragile")

# Helpers ----------------------------------------------------------------------

# Annual percentile status. Thresholds are computed within each year over the
# country-years supplied, using the hazard pipeline's rule:
#   score <= P30 -> Fragile; score > P35 -> Non Fragile; otherwise Transitioning.
annual_status <- function(score, year,
                          low = STATUS_LOW_PERCENTILE,
                          high = STATUS_HIGH_PERCENTILE,
                          type = QUANTILE_TYPE) {
  q <- function(p) ave(score, year, FUN = function(x)
    rep(stats::quantile(x, p, na.rm = TRUE, type = type, names = FALSE), length(x)))
  p_low <- q(low)
  p_high <- q(high)
  out <- ifelse(score <= p_low, STATUS_LABELS[["fragile"]],
                ifelse(score > p_high, STATUS_LABELS[["nonfragile"]],
                       STATUS_LABELS[["transitioning"]]))
  out[is.na(score)] <- NA_character_
  unname(out)
}

# Calendar-aware rolling "any event in the window ending in year t".
# The window includes year t (offsets 0 .. window - 1). The hazard pipeline
# lags every predictor by one year, so in model year t the window becomes
# t-1 .. t-window. Missing years inside the window return NA unless
# `missing_as_zero = TRUE`. With `prebirth_zero = TRUE`, years before the
# series' first observed year (e.g. before independence) count as no event,
# so newly independent states get a value from their first year.
recent_any <- function(event, year, window, missing_as_zero = FALSE, prebirth_zero = FALSE) {
  stopifnot(length(event) == length(year))
  first_obs <- if (any(!is.na(event))) min(year[!is.na(event)]) else Inf
  vapply(seq_along(year), function(i) {
    needed <- seq.int(year[i] - window + 1L, year[i])
    vals <- event[match(needed, year)]
    if (prebirth_zero) vals[needed < first_obs] <- 0L
    if (missing_as_zero) vals[is.na(vals)] <- 0L
    if (anyNA(vals)) return(NA_integer_)
    as.integer(any(vals == 1L))
  }, integer(1))
}

# First sustained exit per country: the country has
# completed at least `history` consecutive Fragile years strictly before t,
# year t-1 is Fragile or Transitioning, and years t .. t+K-1 are Non Fragile.
first_exits <- function(df, status_col, country_col = "ISO_CODE_3",
                        year_col = "YEAR", K = 5L, history = 5L) {
  out <- list()
  for (cn in unique(df[[country_col]])) {
    d <- df[df[[country_col]] == cn, , drop = FALSE]
    yrs <- seq.int(min(d[[year_col]]), max(d[[year_col]]))
    s <- d[[status_col]][match(yrs, d[[year_col]])]
    streak <- 0L; completed <- FALSE
    for (i in seq_along(s)) {
      completed_before <- completed
      if (!is.na(s[i]) && s[i] == STATUS_LABELS[["fragile"]]) streak <- streak + 1L else streak <- 0L
      if (streak >= history) completed <- TRUE
      if (i >= 2L && i + K - 1L <= length(s) && completed_before &&
          !is.na(s[i - 1L]) && s[i - 1L] != STATUS_LABELS[["nonfragile"]] &&
          all(!is.na(s[i:(i + K - 1L)]) & s[i:(i + K - 1L)] == STATUS_LABELS[["nonfragile"]])) {
        out[[length(out) + 1L]] <- data.frame(ISO_CODE_3 = cn, EXIT_YEAR = yrs[i])
        break
      }
    }
  }
  if (length(out) == 0L) return(data.frame(ISO_CODE_3 = character(), EXIT_YEAR = integer()))
  do.call(rbind, out)
}

# Unique country-year keys or stop.
assert_unique_keys <- function(df, keys = c("ISO_CODE_3", "YEAR"), label = "data") {
  dup <- duplicated(df[keys])
  if (any(dup)) stop(label, ": ", sum(dup), " duplicated ", paste(keys, collapse = "-"),
                     " keys, e.g. ", paste(utils::head(do.call(paste, df[dup, keys]), 5), collapse = "; "),
                     call. = FALSE)
  invisible(TRUE)
}

# Binary 0/1 (NA allowed) or stop.
assert_binary <- function(df, vars, label = "data") {
  bad <- vars[!vapply(df[vars], function(x) all(is.na(x) | x %in% c(0, 1)), logical(1))]
  if (length(bad)) stop(label, ": non-binary values in ", paste(bad, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Save an intermediate object with a one-line log.
save_intermediate <- function(x, name) {
  saveRDS(x, int_path(name))
  message(sprintf("[saved] %s: %d rows x %d columns", name, nrow(x), ncol(x)))
  invisible(x)
}

read_intermediate <- function(name) {
  f <- int_path(name)
  if (!file.exists(f)) stop("Missing intermediate '", name, "'. Run its script first (see run_all.R).", call. = FALSE)
  readRDS(f)
}

# Load the V-Dem country-year data frame from a vdemdata .RData file.
load_vdem <- function(path = VDEM_FILE) {
  if (!file.exists(path)) stop("V-Dem file not found: ", path, ". See data-raw/README.md.", call. = FALSE)
  e <- new.env()
  suppressWarnings(load(path, envir = e))
  e$vdem
}
