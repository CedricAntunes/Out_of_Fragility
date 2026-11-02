# ==============================================================================
# Alternative democracy measures and sustained exit from fragility (K = 5)
# Appendix robustness: V-Dem and WGI fragility outcomes
# Revised November 2026
# ==============================================================================
#
# PURPOSE
#   Show that the democracy result in the main hazard table does not depend on
#   (a) the Boix-Miller-Rosato coding of democracy, (b) the control set, (c) the
#   estimation sample implied by each measure's coverage, or (d) the V-Dem data
#   source used to build the outcome.
#
# OUTCOMES (each rebuilt from the continuous index with the Koami annual
# P30 / P35 rule, T = K = 5, first qualifying exit only)
#   vdem_full : V-Dem index with rule of law      (36 exits, exact set asserted)
#   vdem_norl : V-Dem index without rule of law   (39 exits, count asserted)
#   wgi_full  : WGI index (CC, RL, GE, PV)        ( 9 exits, exact set asserted)
#   wgi_norl  : WGI index without rule of law     (optional, count reported only)
#   WGI years with zero cross-country variance (unpublished 1999, 2001 values
#   carried as one constant) are treated as unscored, and unscored interior years
#   receive the Koami-equivalent status, exactly as in Analysis/wgi_hazard_model.R.
#
# DEMOCRACY MEASURES (each lagged one year, one at a time)
#   Binary: Boix-Miller-Rosato (main), Geddes-Wright-Frantz, Democracy-
#           Dictatorship, Freedom House.
#   Continuous (1 SD): Polity democracy (DEMOC, 0-10), V-Dem electoral democracy.
#
# SPECIFICATIONS (bias-reduced logit, AS_mixed; duration spline; country-
# clustered sandwich SEs, as in the main pipelines)
#   bivariate : democracy only
#   gdp       : + log GDP per capita
#   main      : the full control set of Analysis/hazard_model.R with the
#               democracy measure swapped in (V-Dem outcomes only; the WGI
#               universe of 9 exits cannot support 10 substantive parameters)
#
# SAMPLES
#   paired : within outcome x measure, every specification uses the same rows,
#            namely complete cases for the richest specification estimated for
#            that outcome. Changes across specifications are then due to the
#            controls, not to row composition.
#   Boix benchmark : for every non-Boix measure, Boix is re-estimated on that
#            measure's paired sample. The gap between the measure's estimate and
#            the same-sample Boix estimate is the measure effect; the gap between
#            the same-sample Boix estimate and the headline Boix estimate is the
#            sample effect (DD ends in 2008, GWF in 2010).
#   common : rows complete on every democracy measure (reported separately;
#            coverage is bounded by DD, so post-2009 exits drop out).
#
# CHANGES RELATIVE TO THE PREVIOUS VERSION
#   (1) Polity DEMOC carried the special codes -66 / -77 / -88 (interruption,
#       interregnum, transition) as standardized values. 292 country-years were
#       therefore scored about 4 to 5 SD below full autocracy, concentrated in
#       collapsed and transitional states, i.e. inside the risk set. The codes
#       are now recovered and set to missing before standardization.
#   (2) The main specification is added, so the appendix speaks to the headline
#       estimate rather than to bivariate correlations only.
#   (3) Same-sample Boix benchmark and common-sample estimates separate measure
#       effects from coverage effects.
#   (4) WGI outcomes added, with the W3 / W4 rules of the WGI pipeline.
#   (5) The headline Boix estimate is reproduced and asserted, and every
#       outcome's event universe is checked before estimation.
#   (6) An event-retention audit records which exits each measure loses.
# ==============================================================================

# Packages ---------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "tibble", "rlang", "ggplot2",
                       "brglm2", "sandwich", "knitr", "kableExtra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the following packages before running the script: ",
       paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(rlang)
  library(ggplot2)
})

# User settings ----------------------------------------------------------------
# The Serbia-patched panel (Data-Construction/Serbia_fix.R) is the analysis
# file. The RDS committed to the repository predates the patch: Serbia's
# 1992-2005 Boix, durability and factionalism values are missing there, so the
# Serbia 2000 exit drops from every main-specification sample.
DATA_CANDIDATES <- c(
  "standardized_data_final_serbia_fixed.RDS",
  "../standardized_data_final_serbia_fixed.RDS",
  "standardized_data_final.RDS",
  "../standardized_data_final.RDS"
)
# Set TRUE only to run on the unpatched file (results are then one exit short).
if (!exists("ALLOW_UNPATCHED_SERBIA")) ALLOW_UNPATCHED_SERBIA <- FALSE
OUTPUT_DIR <- "democracy_alternative_measures_K5"

K_SUSTAIN          <- 5L
LAG_YEARS          <- 1L
FRAG_HISTORY_YEARS <- 5L
START_PERCENTILE   <- 0.30
EXIT_PERCENTILE    <- 0.35
QUANTILE_TYPE      <- 7L
CI_LEVEL           <- 0.95

# Fits with fewer modeled events than this are estimated and saved, but drawn
# hollow and flagged in the table as descriptive only.
MIN_EVENTS_FOR_INFERENCE <- 10L

INCLUDE_WGI_NORL <- TRUE

BRGLM_MAXIT   <- 2000L
BRGLM_EPSILON <- 1e-7
BRGLM_SLOWIT  <- 0.5

COUNTRY  <- "COUNTRY_NAME"
YEAR     <- "YEAR"
NF_LAB   <- "Non Fragile"
FRAG_LAB <- "Fragile"
FRAG_SET <- c("Fragile", "Transitioning")

# Main specification of Analysis/hazard_model.R, with democracy removed.
MAIN_DEMOCRACY <- "BOIX_DEMOCRACY"
MAIN_CONTROLS <- c(
  "N_TOTAL_TROOPS",
  "REGIME_DURABILITY_YEARS",
  "LOG_GDP_PER_CAPITA",
  "PARTIAL_DEMOCRACY_WITH_FACTIONALISM",
  "GDP_DEFLATOR",
  "UN_PKO_ONSET_RECENT3",
  "NAVCO_N_INCUMBENT_ONSET",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3",
  "WAR_RECENT5"
)
GDP_VAR <- "LOG_GDP_PER_CAPITA"
BINARY_CONTROLS <- c(
  "PARTIAL_DEMOCRACY_WITH_FACTIONALISM", "UN_PKO_ONSET_RECENT3",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3", "WAR_RECENT5"
)

DEMOCRACY_MEASURES <- tribble(
  ~var,                 ~source_var,                     ~type,        ~label,
  "BOIX_DEMOCRACY",     "BOIX_DEMOCRACY",                "binary",     "Boix-Miller-Rosato (main)",
  "GWF_DEMOCRACY",      "GWF_DEMOCRACY",                 "binary",     "Geddes-Wright-Frantz",
  "DD_DEMOCRACY",       "DD_DEMOCRACY",                  "binary",     "Democracy-Dictatorship",
  "FH_DEMOCRACY",       "FH_DEMOCRACY",                  "binary",     "Freedom House (binary)",
  "POLITY_DEMOC",       "INSTITUTIONAL_DEMOCRACY_SCORE", "continuous", "Polity democracy (1 SD)",
  "VDEM_POLYARCHY",     "ELECTORAL_DEMOCRACY_SCORE",     "continuous", "V-Dem electoral democracy (1 SD)"
)
DEM_VARS   <- DEMOCRACY_MEASURES$var
DEM_LABELS <- setNames(DEMOCRACY_MEASURES$label, DEMOCRACY_MEASURES$var)

# Koami K = 5 first-exit universes taken from the validated pipelines.
VDEM_FULL_K5_EXITS <- c(
  "Angola" = 2018L, "Argentina" = 1983L, "Armenia" = 2018L, "Bolivia" = 1985L,
  "Bosnia and Herzegovina" = 2000L, "Brazil" = 1988L, "China" = 1977L,
  "Colombia" = 1992L, "Dominican Republic" = 1997L, "Ecuador" = 1979L,
  "El Salvador" = 2009L, "Ethiopia" = 1991L, "Gabon" = 1990L,
  "Gambia, The" = 2017L, "Georgia" = 2004L, "Ghana" = 2000L,
  "Indonesia" = 2003L, "Iran, Islamic Rep." = 1990L, "Kenya" = 2009L,
  "Kyrgyz Republic" = 2011L, "Madagascar" = 1993L, "Mauritania" = 1992L,
  "Mexico" = 1994L, "Myanmar" = 2015L, "Nepal" = 2007L, "Nicaragua" = 1990L,
  "Panama" = 1990L, "Paraguay" = 2008L, "Peru" = 2001L, "Philippines" = 1987L,
  "Serbia" = 2000L, "Sierra Leone" = 2007L, "Sri Lanka" = 2015L,
  "Thailand" = 1995L, "Tunisia" = 2011L, "Uganda" = 1986L
)
WGI_FULL_K5_EXITS <- c(
  "Colombia" = 2010L, "Ecuador" = 2012L, "Georgia" = 2005L,
  "Indonesia" = 2009L, "Kazakhstan" = 2005L, "Lao PDR" = 2013L,
  "Paraguay" = 2016L, "Rwanda" = 2006L, "Serbia" = 2004L
)

OUTCOME_SPECS <- tribble(
  ~outcome,    ~outcome_label,                     ~family, ~index_var,                                  ~expected_events, ~wgi_rules,
  "vdem_full", "V-Dem index, with rule of law",    "V-Dem", "NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y",    36L,              FALSE,
  "vdem_norl", "V-Dem index, without rule of law", "V-Dem", "NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED", 39L,              FALSE,
  "wgi_full",  "WGI index, with rule of law",      "WGI",   "WGI_LOADING_FACTOR_1_NORMALIZED",           9L,               TRUE,
  "wgi_norl",  "WGI index, without rule of law",   "WGI",   "WGI_NORL_LOADING_FACTOR_1_NORMALIZED",      NA_integer_,      TRUE
) %>%
  mutate(status_var = paste0("STATUS_KOAMI_", toupper(outcome)))
EXPECTED_SETS <- list(vdem_full = VDEM_FULL_K5_EXITS, wgi_full = WGI_FULL_K5_EXITS)
# The WGI universe (9 exits) cannot support the 10 substantive parameters of the
# main specification, so WGI outcomes stop at the GDP-adjusted model.
SPECS_BY_FAMILY <- list(`V-Dem` = c("bivariate", "gdp", "main"), WGI = c("bivariate", "gdp"))
if (!INCLUDE_WGI_NORL) OUTCOME_SPECS <- filter(OUTCOME_SPECS, outcome != "wgi_norl")

SPEC_LABELS <- c(
  bivariate = "Democracy only",
  gdp       = "+ log GDP per capita",
  main      = "Main specification"
)

# Headline Boix estimates of Analysis/hazard_model.R (K = 5, bias-reduced, no
# FE). The paired Boix fit in the main specification must reproduce the row for
# the data version in use. The unpatched row was obtained by running
# hazard_model.R on the repository RDS. Delete a row to skip its check.
REFERENCE_MAIN <- tribble(
  ~data_version,   ~outcome,    ~odds_ratio, ~conf_low, ~conf_high, ~observations, ~events,
  "serbia_fixed",  "vdem_full", 4.77,        2.26,      10.04,      1786L,         36L,
  "unpatched",     "vdem_full", 4.82,        2.27,      10.24,      1782L,         35L
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# General helpers --------------------------------------------------------------
resolve_existing_path <- function(candidates) {
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Data file not found. Checked: ", paste(candidates, collapse = "; "))
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

is_binary01 <- function(x) {
  observed <- unique(x[!is.na(x)])
  length(observed) <= 2L && all(observed %in% c(0, 1))
}

assert_binary01 <- function(x, variable_name) {
  if (!is.numeric(x)) stop(variable_name, " must be numeric 0/1.")
  if (!is_binary01(x)) stop(variable_name, " must be binary and coded 0/1.")
  as.numeric(x)
}

standardize_numeric <- function(x, variable_name) {
  if (!is.numeric(x)) stop(variable_name, " must be numeric.")
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) stop(variable_name, " has zero or undefined SD.")
  as.numeric(scale(x))
}

# Polity DEMOC is stored on a linear transform of its raw scale, special codes
# included. Recover the raw 0-10 scale from the 11 equally spaced valid values,
# confirm that the three outlying values map to -66 / -77 / -88 under the same
# transform, and return the raw score with special codes set to missing.
recover_polity_democ <- function(z) {
  u <- sort(unique(z[!is.na(z)]))
  if (length(u) != 14L) {
    stop("Expected 11 valid DEMOC values plus 3 special codes; found ", length(u),
         " distinct values. Inspect INSTITUTIONAL_DEMOCRACY_SCORE.")
  }
  valid <- u[4:14]
  step <- diff(valid)
  if (max(abs(step - mean(step))) > 1e-6) stop("Valid DEMOC values are not equally spaced.")
  zero <- valid[1]; unit <- mean(step)
  raw <- (z - zero) / unit
  specials <- round((u[1:3] - zero) / unit)
  if (!identical(as.integer(specials), c(-88L, -77L, -66L))) {
    stop("Outlying DEMOC values do not map to the -88 / -77 / -66 codes: ",
         paste(specials, collapse = ", "))
  }
  raw <- round(raw)
  n_special <- sum(raw %in% c(-66, -77, -88), na.rm = TRUE)
  raw[raw %in% c(-66, -77, -88)] <- NA_real_
  attr(raw, "n_special") <- n_special
  raw
}

# Koami classification and WGI-specific rules ---------------------------------
add_koami_status <- function(df, index_var, status_var) {
  if (!(index_var %in% names(df))) stop("Missing index variable: ", index_var)
  df %>%
    group_by(.data[[YEAR]]) %>%
    mutate(
      .start = stats::quantile(.data[[index_var]], START_PERCENTILE,
                               na.rm = TRUE, type = QUANTILE_TYPE, names = FALSE),
      .exit  = stats::quantile(.data[[index_var]], EXIT_PERCENTILE,
                               na.rm = TRUE, type = QUANTILE_TYPE, names = FALSE),
      !!status_var := case_when(
        is.na(.data[[index_var]])          ~ NA_character_,
        .data[[index_var]] <= .start       ~ FRAG_LAB,
        .data[[index_var]] >  .exit        ~ NF_LAB,
        TRUE                               ~ "Transitioning"
      )
    ) %>%
    ungroup() %>%
    select(-.start, -.exit)
}

# (W3) Years with zero cross-country variance carry no information.
blank_degenerate_years <- function(x, years) {
  sd_by_year <- tapply(x, years, function(v) {
    if (sum(!is.na(v)) > 1L) stats::sd(v, na.rm = TRUE) else NA_real_
  })
  bad <- as.integer(names(sd_by_year)[!is.na(sd_by_year) & sd_by_year == 0])
  x[years %in% bad] <- NA_real_
  attr(x, "degenerate_years") <- bad
  x
}

# (W4) Koami-equivalent status for unscored interior years.
fill_koami_status_gaps <- function(df, status_var) {
  df %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      .st   = .data[[status_var]],
      .prev = tidyr::fill(tibble(x = .st), x, .direction = "down")$x,
      .next = tidyr::fill(tibble(x = .st), x, .direction = "up")$x,
      !!status_var := case_when(
        !is.na(.st)                                       ~ .st,
        is.na(.prev) | is.na(.next)                       ~ NA_character_,
        .prev == .next & .prev %in% c(FRAG_LAB, NF_LAB)   ~ .prev,
        TRUE                                              ~ "Transitioning"
      )
    ) %>%
    ungroup() %>%
    select(-.st, -.prev, -.next)
}

complete_country_year <- function(df) {
  df %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    tidyr::complete(!!rlang::sym(YEAR) := seq(min(.data[[YEAR]], na.rm = TRUE),
                                              max(.data[[YEAR]], na.rm = TRUE), by = 1L)) %>%
    ungroup() %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]])
}

as_sustained_nf_start <- function(status_vec, k) {
  n <- length(status_vec); out <- integer(n)
  nf <- status_vec == NF_LAB; nf[is.na(nf)] <- FALSE
  if (n < k) return(out)
  for (t in seq_len(n - k + 1L)) if (nf[t] && all(nf[t:(t + k - 1L)])) out[t] <- 1L
  out
}

has_completed_fragile_run <- function(status_vec, min_run, include_current = FALSE) {
  n <- length(status_vec); out <- logical(n)
  streak <- 0L; ever <- FALSE
  for (i in seq_len(n)) {
    if (!include_current) out[i] <- ever
    if (!is.na(status_vec[i]) && status_vec[i] == FRAG_LAB) streak <- streak + 1L else streak <- 0L
    if (streak >= min_run) ever <- TRUE
    if (include_current) out[i] <- ever
  }
  out
}

choose_spline_df <- function(x, max_df = 3L) {
  max(1L, min(max_df, length(unique(x[!is.na(x)])) - 1L))
}

# Exact K = 5 first-exit risk set of the validated pipelines --------------------
build_hazard <- function(df_full, predictors, status_var) {
  end_year <- df_full %>%
    group_by(.data[[COUNTRY]]) %>%
    summarize(last_year_cty = suppressWarnings(
      max(.data[[YEAR]][!is.na(.data[[status_var]])], na.rm = TRUE)), .groups = "drop") %>%
    mutate(last_year_cty = as.integer(if_else(is.finite(last_year_cty), last_year_cty, NA_real_)),
           last_eligible = last_year_cty - (K_SUSTAIN - 1L)) %>%
    filter(!is.na(last_year_cty))

  lag_suffix <- paste0("_L", LAG_YEARS)

  df1 <- df_full %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      nf_start        = as_sustained_nf_start(.data[[status_var]], K_SUSTAIN),
      prev_status     = dplyr::lag(.data[[status_var]]),
      prior_fragile5  = has_completed_fragile_run(.data[[status_var]], FRAG_HISTORY_YEARS, FALSE),
      fragile5_by_now = has_completed_fragile_run(.data[[status_var]], FRAG_HISTORY_YEARS, TRUE),
      across(all_of(predictors), ~ dplyr::lag(.x, LAG_YEARS), .names = paste0("{.col}", lag_suffix))
    ) %>%
    ungroup() %>%
    mutate(QUALIFYING_EXIT = if_else(
      nf_start == 1L & !is.na(prev_status) & prev_status %in% FRAG_SET & prior_fragile5, 1L, 0L)) %>%
    left_join(end_year, by = setNames(COUNTRY, COUNTRY)) %>%
    filter(!is.na(last_eligible), .data[[YEAR]] <= last_eligible) %>%
    select(-last_year_cty, -last_eligible)

  df1_est <- df1 %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      first_exit_year = {
        yy <- .data[[YEAR]][QUALIFYING_EXIT == 1L]
        if (length(yy)) min(yy) else NA_integer_
      },
      EXIT_SUSTAINED = as.integer(QUALIFYING_EXIT == 1L & .data[[YEAR]] == first_exit_year),
      at_risk = if_else(
        .data[[status_var]] %in% FRAG_SET & fragile5_by_now &
          (is.na(first_exit_year) | .data[[YEAR]] < first_exit_year), 1L, 0L)
    ) %>%
    ungroup()

  haz <- df1_est %>%
    filter(at_risk == 1L | EXIT_SUSTAINED == 1L) %>%
    group_by(.data[[COUNTRY]]) %>%
    filter(any(at_risk == 1L) | any(EXIT_SUSTAINED == 1L)) %>%
    ungroup() %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      lag_at_risk  = dplyr::lag(at_risk, default = 0L),
      spell_start  = at_risk == 1L & lag_at_risk == 0L,
      spell_id_tmp = cumsum(spell_start),
      spell_id     = replace_na(if_else(at_risk == 1L, spell_id_tmp, dplyr::lag(spell_id_tmp)), 0L)
    ) %>%
    group_by(.data[[COUNTRY]], spell_id) %>%
    mutate(t_at_risk = row_number() - 1L) %>%
    ungroup() %>%
    select(-lag_at_risk, -spell_start, -spell_id_tmp)

  stopifnot(all(haz$EXIT_SUSTAINED %in% c(0L, 1L)))
  stopifnot(!anyDuplicated(haz[[COUNTRY]][haz$EXIT_SUSTAINED == 1L]))
  haz
}

check_event_universe <- function(haz, outcome, expected_events, expected_set) {
  observed <- haz %>%
    filter(EXIT_SUSTAINED == 1L) %>%
    transmute(COUNTRY = as.character(.data[[COUNTRY]]), YEAR = as.integer(.data[[YEAR]]))
  message(outcome, ": ", nrow(haz), " risk-set/event rows; ", nrow(observed), " first exits.")
  if (!is.na(expected_events) && nrow(observed) != expected_events) {
    stop("Event-universe mismatch for ", outcome, ": expected ", expected_events,
         " K = 5 first exits, reconstructed ", nrow(observed), ".")
  }
  if (!is.null(expected_set)) {
    expected <- tibble(COUNTRY = names(expected_set), YEAR = as.integer(expected_set))
    missing <- anti_join(expected, observed, by = c("COUNTRY", "YEAR"))
    extra   <- anti_join(observed, expected, by = c("COUNTRY", "YEAR"))
    if (nrow(missing) || nrow(extra)) {
      print(missing); print(extra)
      stop("K = 5 first-exit set for ", outcome, " does not match the Koami table.")
    }
    message(outcome, ": exact country-year match to the Koami K = 5 table.")
  } else if (is.na(expected_events)) {
    message(outcome, ": event universe not validated against Koami_check.ipynb; ",
            "treat as descriptive.")
  }
  observed
}

# Estimation -------------------------------------------------------------------
fit_brglm_stable <- function(formula, data) {
  X <- stats::model.matrix(formula, data = data)
  stats::glm(
    formula = formula, data = data, family = stats::binomial("logit"),
    method = brglm2::brglmFit, start = rep(0, ncol(X)),
    control = brglm2::brglmControl(type = "AS_mixed", maxit = BRGLM_MAXIT,
                                   epsilon = BRGLM_EPSILON, slowit = BRGLM_SLOWIT)
  )
}

capture_expression <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    return(list(value = NULL, error = conditionMessage(value), warnings = unique(warnings)))
  }
  list(value = value, error = NA_character_, warnings = unique(warnings))
}

spec_terms <- function(spec) {
  switch(spec,
         bivariate = character(),
         gdp       = GDP_VAR,
         main      = MAIN_CONTROLS,
         stop("Unknown specification: ", spec))
}

lagged <- function(x) if (length(x)) paste0(x, "_L", LAG_YEARS) else character()

# Fit one bias-reduced model and return a one-row tibble for the democracy term.
fit_one <- function(model_data, democracy_var, spec) {
  dem_term <- lagged(democracy_var)
  control_terms <- lagged(spec_terms(spec))
  n_obs <- nrow(model_data)
  n_cty <- n_distinct(model_data[[COUNTRY]])
  n_evt <- sum(model_data$EXIT_SUSTAINED == 1L)
  n_par <- 1L + length(control_terms)

  base <- tibble(
    term = dem_term, beta = NA_real_, std_error = NA_real_, odds_ratio = NA_real_,
    conf_low = NA_real_, conf_high = NA_real_, p_value = NA_real_,
    observations = n_obs, countries = n_cty, events = n_evt,
    substantive_parameters = n_par, events_per_parameter = n_evt / n_par,
    spline_df = NA_integer_, converged = FALSE, usable = FALSE,
    warnings = NA_character_, error = NA_character_
  )
  if (!n_obs || n_evt == 0L || n_evt == n_obs) {
    base$error <- "Empty or constant-outcome estimation sample."; return(base)
  }
  if (length(unique(model_data[[dem_term]])) < 2L) {
    base$error <- "Democracy measure has no variation in the estimation sample."; return(base)
  }
  constant_controls <- control_terms[vapply(model_data[control_terms],
                                            function(v) length(unique(v)) < 2L, logical(1))]
  if (length(constant_controls)) {
    base$error <- paste("Constant control(s):", paste(constant_controls, collapse = ", "))
    return(base)
  }

  spline_df <- choose_spline_df(model_data$t_at_risk)
  formula <- stats::as.formula(paste0(
    "EXIT_SUSTAINED ~ ", paste(c(dem_term, control_terms), collapse = " + "),
    " + splines::ns(t_at_risk, ", spline_df, ")"))

  fit <- capture_expression(fit_brglm_stable(formula, model_data))
  base$spline_df <- spline_df
  if (is.null(fit$value)) {
    base$error <- fit$error; base$warnings <- paste(fit$warnings, collapse = " | ")
    return(base)
  }
  model <- fit$value
  inf <- capture_expression({
    V <- sandwich::vcovCL(model, cluster = model_data[[COUNTRY]])
    c(beta = unname(stats::coef(model)[[dem_term]]), se = sqrt(unname(V[dem_term, dem_term])))
  })
  warning_text <- paste(unique(c(fit$warnings, inf$warnings)), collapse = " | ")
  base$converged <- isTRUE(model$converged)
  base$warnings  <- warning_text
  if (is.null(inf$value)) { base$error <- inf$error; return(base) }

  z <- stats::qnorm(1 - (1 - CI_LEVEL) / 2)
  b <- inf$value[["beta"]]; s <- inf$value[["se"]]
  bad_warning <- grepl(
    "algorithm did not converge|failed to converge|non-positive-definite|false convergence|singular|NaN",
    warning_text, ignore.case = TRUE)
  is_usable <- base$converged && is.finite(b) && is.finite(s) && s > 0 && !bad_warning &&
    max(abs(stats::coef(model)), na.rm = TRUE) <= 1e6

  base$beta <- b; base$std_error <- s; base$odds_ratio <- exp(b)
  base$conf_low <- exp(b - z * s); base$conf_high <- exp(b + z * s)
  base$p_value <- 2 * stats::pnorm(abs(b / s), lower.tail = FALSE)
  base$usable <- is_usable
  base$error <- if (is_usable) NA_character_ else "Fit failed numerical screening."
  base
}

complete_rows <- function(haz, vars) {
  haz %>% filter(if_all(all_of(vars), ~ !is.na(.x))) %>% as.data.frame()
}

# Plot and table helpers -------------------------------------------------------
fmt_cell <- function(or, lo, hi, usable, events) {
  if (is.na(or) || !isTRUE(usable)) return("--")
  cell <- sprintf("%.2f [%.2f, %.2f]", or, lo, hi)
  if (lo > 1 || hi < 1) cell <- paste0("\\textbf{", cell, "}")
  if (events < MIN_EVENTS_FOR_INFERENCE) cell <- paste0(cell, "$^{\\dagger}$")
  cell
}

save_plot_pair <- function(plot_object, stem, width, height) {
  ggsave(file.path(OUTPUT_DIR, paste0(stem, ".png")), plot_object,
         width = width, height = height, units = "in", dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_DIR, paste0(stem, ".pdf")), plot_object,
         width = width, height = height, units = "in",
         device = grDevices::cairo_pdf, bg = "white")
}

# Load and prepare data --------------------------------------------------------
if (!exists("standardized_data_final", inherits = TRUE)) {
  data_path <- resolve_existing_path(DATA_CANDIDATES)
  message("Reading: ", data_path)
  standardized_data_final <- readRDS(data_path)
}
df <- standardized_data_final %>%
  mutate(!!YEAR := suppressWarnings(as.integer(.data[[YEAR]])))

serbia_1999 <- df %>% filter(.data[[COUNTRY]] == "Serbia", .data[[YEAR]] == 1999L)
DATA_VERSION <- if (nrow(serbia_1999) == 1L && !is.na(serbia_1999$BOIX_DEMOCRACY)) "serbia_fixed" else "unpatched"
if (DATA_VERSION == "unpatched") {
  if (!ALLOW_UNPATCHED_SERBIA) {
    stop("Serbia 1999 BOIX_DEMOCRACY is missing: this is the pre-patch panel. Run ",
         "Data-Construction/Serbia_fix.R first (it writes standardized_data_final_serbia_fixed.RDS), ",
         "or set ALLOW_UNPATCHED_SERBIA <- TRUE to proceed one exit short.")
  }
  warning("Running on the unpatched panel: Serbia 2000 is excluded from samples that need ",
          "Serbia's 1999 Boix, durability or factionalism values.")
}
message("Data version: ", DATA_VERSION)

required_columns <- unique(c(COUNTRY, YEAR, OUTCOME_SPECS$index_var,
                             DEMOCRACY_MEASURES$source_var, MAIN_CONTROLS))
missing_columns <- setdiff(required_columns, names(df))
if (length(missing_columns)) stop("Required columns are missing: ", paste(missing_columns, collapse = ", "))

# Outcomes: classify each index before any predictor is touched.
for (i in seq_len(nrow(OUTCOME_SPECS))) {
  o <- OUTCOME_SPECS[i, ]
  idx <- df[[o$index_var]]
  if (o$wgi_rules) {
    idx <- blank_degenerate_years(idx, df[[YEAR]])
    message(o$outcome, ": years treated as unscored (zero variance): ",
            paste(attr(idx, "degenerate_years"), collapse = ", "))
  }
  df[[".index_tmp"]] <- as.numeric(idx)
  df <- add_koami_status(df, ".index_tmp", o$status_var)
}
df[[".index_tmp"]] <- NULL

# Democracy measures.
for (j in seq_len(nrow(DEMOCRACY_MEASURES))) {
  m <- DEMOCRACY_MEASURES[j, ]
  x <- df[[m$source_var]]
  if (m$var == "POLITY_DEMOC") {
    x <- recover_polity_democ(x)
    message("Polity DEMOC: ", attr(x, "n_special"),
            " country-years with special codes -66/-77/-88 set to missing.")
  }
  df[[m$var]] <- if (m$type == "binary") assert_binary01(x, m$var) else
    standardize_numeric(as.numeric(x), m$var)
}

# Controls: binary checks and standardization exactly as in hazard_model.R.
for (v in BINARY_CONTROLS) df[[v]] <- assert_binary01(df[[v]], v)
for (v in setdiff(MAIN_CONTROLS, BINARY_CONTROLS)) df[[v]] <- standardize_numeric(df[[v]], v)

df_full <- complete_country_year(df)
for (i in which(OUTCOME_SPECS$wgi_rules)) {
  df_full <- fill_koami_status_gaps(df_full, OUTCOME_SPECS$status_var[i])
}
all_predictors <- unique(c(DEM_VARS, MAIN_CONTROLS))

# Estimate ---------------------------------------------------------------------
HAZARD_DATA  <- list()
RESULTS      <- tibble()
EVENT_AUDIT  <- tibble()

for (i in seq_len(nrow(OUTCOME_SPECS))) {
  o <- OUTCOME_SPECS[i, ]
  specs <- SPECS_BY_FAMILY[[o$family]]
  richest <- unique(c(spec_terms(tail(specs, 1))))

  haz <- build_hazard(df_full, all_predictors, o$status_var)
  universe <- check_event_universe(haz, o$outcome, o$expected_events, EXPECTED_SETS[[o$outcome]])
  HAZARD_DATA[[o$outcome]] <- haz

  # Common sample across measures (diagnostic).
  common_data <- complete_rows(haz, lagged(c(DEM_VARS, richest)))

  for (dv in DEM_VARS) {
    paired <- complete_rows(haz, lagged(c(dv, richest)))
    paired_boix <- complete_rows(haz, lagged(c(dv, MAIN_DEMOCRACY, richest)))

    retained <- paired %>%
      filter(EXIT_SUSTAINED == 1L) %>%
      transmute(COUNTRY = as.character(.data[[COUNTRY]]), YEAR = as.integer(.data[[YEAR]]))
    EVENT_AUDIT <- bind_rows(EVENT_AUDIT, universe %>%
      mutate(outcome = o$outcome, democracy_var = dv,
             retained = paste(COUNTRY, YEAR) %in% paste(retained$COUNTRY, retained$YEAR)))

    for (spec in specs) {
      own <- fit_one(paired, dv, spec) %>% mutate(sample = "paired")
      boix_same <- if (dv != MAIN_DEMOCRACY) {
        fit_one(paired_boix, MAIN_DEMOCRACY, spec) %>% mutate(sample = "boix_on_measure_sample")
      } else NULL
      common <- fit_one(common_data, dv, spec) %>% mutate(sample = "common")
      RESULTS <- bind_rows(RESULTS, bind_rows(own, boix_same, common) %>%
        mutate(outcome = o$outcome, outcome_label = o$outcome_label, family = o$family,
               democracy_var = dv, democracy_label = DEM_LABELS[[dv]],
               specification = spec, specification_label = SPEC_LABELS[[spec]],
               .before = 1))
    }
  }
}

RESULTS <- RESULTS %>%
  mutate(low_events = events < MIN_EVENTS_FOR_INFERENCE,
         outcome_label = factor(outcome_label, levels = OUTCOME_SPECS$outcome_label),
         democracy_label = factor(democracy_label, levels = rev(DEMOCRACY_MEASURES$label)),
         specification_label = factor(specification_label, levels = SPEC_LABELS))

# Reproduce the headline estimate ---------------------------------------------
headline <- RESULTS %>%
  filter(sample == "paired", democracy_var == MAIN_DEMOCRACY, specification == "main")
reference_rows <- REFERENCE_MAIN %>% filter(data_version == DATA_VERSION)
for (r in seq_len(nrow(reference_rows))) {
  ref <- reference_rows[r, ]
  got <- headline %>% filter(outcome == ref$outcome)
  ok <- nrow(got) == 1L &&
    isTRUE(all.equal(round(c(got$odds_ratio, got$conf_low, got$conf_high), 2),
                     c(ref$odds_ratio, ref$conf_low, ref$conf_high), tolerance = 1e-8)) &&
    got$observations == ref$observations && got$events == ref$events
  if (!ok) {
    print(got %>% select(outcome, odds_ratio, conf_low, conf_high, observations, events))
    stop("Main-specification Boix estimate for ", ref$outcome,
         " does not reproduce the main hazard table.")
  }
  message(ref$outcome, ": main-specification Boix estimate reproduces the main table.")
}

# Save -------------------------------------------------------------------------
results_out <- RESULTS %>% mutate(across(where(is.factor), as.character))
utils::write.csv(results_out, file.path(OUTPUT_DIR, "democracy_alternative_measures_K5_estimates.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(EVENT_AUDIT, file.path(OUTPUT_DIR, "democracy_alternative_measures_K5_event_retention.csv"),
                 row.names = FALSE, na = "")
saveRDS(list(results = RESULTS, event_audit = EVENT_AUDIT, hazard_data = HAZARD_DATA,
             settings = list(data_version = DATA_VERSION, K = K_SUSTAIN, lag_years = LAG_YEARS,
                             fragile_history_years = FRAG_HISTORY_YEARS,
                             min_events_for_inference = MIN_EVENTS_FOR_INFERENCE,
                             main_controls = MAIN_CONTROLS)),
        file.path(OUTPUT_DIR, "democracy_alternative_measures_K5.rds"))

# Figure 1: paired estimates, all outcomes and specifications ------------------
spec_colours <- c("Democracy only" = "#7F7F7F", "+ log GDP per capita" = "#1F5A99",
                  "Main specification" = "#B40000")
spec_shapes  <- c("Democracy only" = 21, "+ log GDP per capita" = 22, "Main specification" = 23)
X_LIMITS <- c(0.05, 50)

plot_data <- RESULTS %>%
  filter(sample == "paired", usable) %>%
  mutate(fill_key = if_else(low_events, "white", as.character(spec_colours[as.character(specification_label)])),
         conf_low_plot = pmax(conf_low, X_LIMITS[1]),
         conf_high_plot = pmin(conf_high, X_LIMITS[2]))

dodge <- position_dodge(width = 0.6)
fig_main <- ggplot(plot_data, aes(x = odds_ratio, y = democracy_label,
                                  colour = specification_label, shape = specification_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_errorbar(aes(xmin = conf_low_plot, xmax = conf_high_plot), width = 0, linewidth = 0.6,
                orientation = "y", position = dodge) +
  geom_point(aes(fill = I(fill_key)), size = 2.4, stroke = 0.8, position = dodge) +
  scale_colour_manual(values = spec_colours, name = NULL) +
  scale_shape_manual(values = spec_shapes, name = NULL) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
                labels = c("0.1", "0.25", "0.5", "1", "2", "4", "8", "16", "32")) +
  coord_cartesian(xlim = X_LIMITS) +
  facet_wrap(~ outcome_label, ncol = 2) +
  labs(
    x = "Odds ratio of sustained exit (log scale; 95% country-clustered CI)", y = NULL,
    caption = paste0(
      "K = 5, first qualifying exit. Bias-reduced logits with a duration spline; democracy lagged one year. ",
      "Within each outcome and measure, all specifications share one sample.\n",
      "Main specification adds the controls of the main hazard table (V-Dem outcomes only). ",
      "Hollow markers: fewer than ", MIN_EVENTS_FOR_INFERENCE,
      " modeled exits (descriptive only). Intervals are truncated at the plot limits."
    )
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = "black"),
        plot.caption = element_text(hjust = 0, size = 7.5), plot.caption.position = "plot",
        axis.text.y = element_text(colour = "black"))
save_plot_pair(fig_main, "fig_democracy_alternative_measures_K5", width = 10,
               height = if (nrow(OUTCOME_SPECS) > 2) 8 else 4.8)

# Figure 2: measure effect versus sample effect (main specification) -----------
decomp <- RESULTS %>%
  filter(family == "V-Dem", specification == "main", usable,
         sample %in% c("paired", "boix_on_measure_sample")) %>%
  mutate(estimate = if_else(sample == "paired", "Measure on its own sample",
                            "Boix on the same sample"),
         estimate = factor(estimate, levels = c("Measure on its own sample", "Boix on the same sample")))
headline_or <- headline %>% filter(family == "V-Dem") %>% select(outcome_label, odds_ratio)

fig_decomp <- ggplot(decomp, aes(x = odds_ratio, y = democracy_label, colour = estimate, shape = estimate)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_vline(data = headline_or, aes(xintercept = odds_ratio), colour = "#B40000",
             linetype = "dotted", linewidth = 0.6) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0, linewidth = 0.6,
                orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(size = 2.4, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("#B40000", "#4D4D4D"), name = NULL) +
  scale_shape_manual(values = c(16, 1), name = NULL) +
  scale_x_log10() +
  facet_wrap(~ outcome_label, ncol = 2) +
  labs(x = "Odds ratio (log scale; 95% country-clustered CI)", y = NULL,
       caption = paste0(
         "Main specification, K = 5. Open markers re-estimate the Boix-Miller-Rosato measure on the rows used ",
         "for each alternative measure.\nThe dotted line is the headline Boix estimate on the full sample.")) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = "black"),
        plot.caption = element_text(hjust = 0, size = 7.5), plot.caption.position = "plot",
        axis.text.y = element_text(colour = "black"))
save_plot_pair(fig_decomp, "fig_democracy_measure_vs_sample_K5", width = 10, height = 4.4)

# LaTeX table: paired estimates -------------------------------------------------
make_table <- function(results, sample_tag, caption, label, file_out) {
  cols <- bind_rows(lapply(seq_len(nrow(OUTCOME_SPECS)), function(i) tibble(
    outcome = OUTCOME_SPECS$outcome[i], outcome_label = OUTCOME_SPECS$outcome_label[i],
    specification = SPECS_BY_FAMILY[[OUTCOME_SPECS$family[i]]]))) %>%
    mutate(col = paste(outcome, specification, sep = "__"))
  cell_data <- results %>%
    filter(sample == sample_tag) %>%
    mutate(col = paste(outcome, specification, sep = "__"),
           cell = mapply(fmt_cell, odds_ratio, conf_low, conf_high, usable, events),
           n_cell = sprintf("%s / %d", format(observations, big.mark = ","), events))
  est <- cell_data %>% select(democracy_var, col, cell) %>%
    pivot_wider(names_from = col, values_from = cell)
  nn  <- cell_data %>% select(democracy_var, col, n_cell) %>%
    pivot_wider(names_from = col, values_from = n_cell)
  rows <- bind_rows(lapply(DEMOCRACY_MEASURES$var, function(v) bind_rows(
    est %>% filter(democracy_var == v) %>% mutate(Measure = DEM_LABELS[[v]]),
    nn  %>% filter(democracy_var == v) %>% mutate(Measure = "\\quad\\textit{Obs. / exits}")
  ))) %>% select(Measure, all_of(cols$col))
  rows[is.na(rows)] <- ""

  header_spans <- cols %>% count(outcome_label, sort = FALSE)
  header_spans <- header_spans[match(unique(cols$outcome_label), header_spans$outcome_label), ]
  spec_short <- c(bivariate = "Dem. only", gdp = "+ GDP", main = "Main")
  kb <- knitr::kable(rows, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
                     caption = caption, label = label,
                     col.names = c("Democracy measure", unname(spec_short[cols$specification])),
                     align = c("l", rep("c", nrow(cols)))) %>%
    kableExtra::add_header_above(c(" " = 1, setNames(header_spans$n, header_spans$outcome_label))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 7) %>%
    kableExtra::footnote(general = paste0(
      "Odds ratios with 95\\\\% confidence intervals from country-clustered standard errors. ",
      "Outcome is the first sustained exit (K = 5) after at least five consecutive Fragile years. ",
      "Bias-reduced logits (brglm2, AS\\\\_mixed) with a natural spline in time at risk; democracy lagged one year. ",
      "Binary measures are 0/1; Polity and V-Dem electoral democracy are in standard deviations. ",
      "Polity special codes (-66, -77, -88) are treated as missing. ",
      "Main: the control set of the main hazard table. ",
      if (sample_tag == "paired") "Within each outcome and measure, all specifications use the same rows. " else
        "All measures use the rows complete on every democracy measure. ",
      "WGI outcomes start in 1996; the WGI no-rule-of-law universe is not validated against the Koami table. ",
      "Bold: interval excludes one. $\\\\dagger$: fewer than ", MIN_EVENTS_FOR_INFERENCE,
      " modeled exits, descriptive only."),
      threeparttable = TRUE, escape = FALSE)
  writeLines(as.character(kb), file.path(OUTPUT_DIR, file_out))
  kb
}

make_table(RESULTS, "paired",
           "Alternative democracy measures and sustained exit from fragility (K = 5)",
           "democracy_alternative_measures_K5", "table_democracy_alternative_measures_K5.tex")
make_table(RESULTS, "common",
           "Alternative democracy measures on a common sample (K = 5)",
           "democracy_alternative_measures_common_K5", "table_democracy_alternative_measures_common_K5.tex")

# Console summary --------------------------------------------------------------
print(
  RESULTS %>%
    filter(sample == "paired") %>%
    transmute(outcome, specification, democracy_var,
              OR = round(odds_ratio, 2), lo = round(conf_low, 2), hi = round(conf_high, 2),
              p = signif(p_value, 2), observations, events, usable),
  n = Inf
)
message("Finished. Outputs written to: ", normalizePath(OUTPUT_DIR, winslash = "/"))
