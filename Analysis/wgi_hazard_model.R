# ==============================================================================
# Hazard model pipeline (v5 stabilized): Koami-consistent first-exit revision
# Sustained exit from fragility after >= 5 consecutive Fragile years
# ------------------------------------------------------------------------------
# CHANGES vs v1:
#  (1) Predictors lagged on the FULL year-complete panel, before risk-set
#      subsetting -> fixes calendar-gap misalignment for relapse/re-entry cases.
#  (2) Pooled year-FE, absorbed two-way FE, random-intercept, and ordinary logits
#      are retained as diagnostics and recorded in MODEL_STATUS.
#  (3) 2% leverage-trim model is actually re-estimated (brglm2, no FE) & collected.
#  (4) make_robust_table() compares only the two credible bias-reduced variants.
#  (5) The modeled event is now strictly each country's first qualifying exit.
#      All raw qualifying exits, including later post-relapse exits, are audited.
#  (6) Model convergence and interpretability are recorded before estimates are
#      admitted to any table; unstable diagnostic estimators are not robustness.
#  (7) Every displayed model reports observations, countries, events, and events
#      per substantive and total slope parameter. Ordinary-logit AMEs are omitted.
#  (8) brglm2 uses a neutral zero start and damped quasi-Fisher steps. This
#      stabilizes AS_mixed under the quasi-separation induced by troop presence.
#  (9) Fragile / Transitioning / Non-Fragile status is reconstructed directly
#      from the continuous full-index score using Koami's annual 30th / 35th
#      percentile rule. The raw K = 5 first-exit universe is asserted to be 36.
# ------------------------------------------------------------------------------
# WGI REPLICATION (this file). Identical specification to hazard_model.R; only
# the WGI-specific pieces below differ.
#  (W1) Index, legacy status, status, and output tag point to the WGI full index.
#  (W2) Expected first-exit sets come from Koami_check.ipynb run on the WGI index
#       (K = 5: 9 cases). K = 3 / K = 7 counts are this pipeline's own counts,
#       as in the V-Dem version (Koami uses one horizon for history and sustain).
#  (W3) WGI was not published for 1997, 1999, 2001. In standardized_data_final
#       the 1999 and 2001 values are one constant for every country, which codes
#       every country Fragile in those years. Years with zero within-year
#       variance in the index are treated as unscored (DROP_DEGENERATE_INDEX_YEARS).
#  (W4) Koami linearly interpolates the 0/1 flags across unscored interior
#       years. The equivalent status rule is applied here: an interior gap year
#       is Fragile if both bracketing scored years are Fragile, Non Fragile if
#       both are Non Fragile, Transitioning otherwise (FILL_INTERIOR_STATUS_GAPS).
#       V-Dem has no interior gaps, so this rule is inert there.
# ==============================================================================

# Settings ---------------------------------------------------------------------
COUNTRY <- "COUNTRY_NAME"
YEAR    <- "YEAR"
INDEX_VAR     <- "WGI_LOADING_FACTOR_1_NORMALIZED"
LEGACY_STATUS <- "WGI_STATUS_IDEAL"
STATUS        <- "WGI_STATUS_IDEAL_KOAMI"
DROP_DEGENERATE_INDEX_YEARS <- TRUE  # (W3) blank WGI 1999 / 2001 constant fill
FILL_INTERIOR_STATUS_GAPS   <- TRUE  # (W4) Koami interpolation of gap years

START_PERCENTILE <- 0.30
EXIT_PERCENTILE  <- 0.35
QUANTILE_TYPE     <- 7L  # R type 7 matches pandas quantile(interpolation = "linear")

NF_LAB   <- "Non Fragile"
FRAG_LAB <- "Fragile"
FRAG_SET <- c("Fragile", "Transitioning")

K_GRID              <- c(3L, 5L, 7L)
LAG_YEARS           <- 1L
DROP_TOP_LEVERAGE   <- 0.02
FRAG_HISTORY_YEARS  <- 5L
ROBUST_K_FOCUS      <- 5L            # horizon shown in the estimator-robustness table
OUTCOME_TAG         <- "wgi_full_index"
CLASSIFIED_EPISODE_UNIVERSE <- 9L
EXPECTED_FIRST_EXIT_COUNTS <- c(`3` = 13L, `5` = 9L, `7` = 8L)
# Koami_check.ipynb logic on WGI_LOADING_FACTOR_1_NORMALIZED (T = 5, P30 / P35).
EXPECTED_K5_FIRST_EXITS <- c(
  "Colombia" = 2010L,
  "Ecuador" = 2012L,
  "Georgia" = 2005L,
  "Indonesia" = 2009L,
  "Kazakhstan" = 2005L,
  "Lao PDR" = 2013L,
  "Paraguay" = 2016L,
  "Rwanda" = 2006L,
  "Serbia" = 2004L
)
OUTPUT_DIR          <- "."

FINAL_VARS_BASE <- c(
  "N_TOTAL_TROOPS",
  #"BMR_ANY_TRANSITION_RECENT5",
  "BOIX_DEMOCRACY",
  "REGIME_DURABILITY_YEARS",
  "LOG_GDP_PER_CAPITA",
  #"LOG_ODA_RECEIVED_PER_CAPITA",
  #"WAR_SHORT",                       
  #"WAR_CHRONIC",
  "PARTIAL_DEMOCRACY_WITH_FACTIONALISM",
  "GDP_DEFLATOR",
  #"REIGN_LEADER_CHANGE_RECENT3",
  "UN_PKO_ONSET_RECENT3",
  "NAVCO_N_INCUMBENT_ONSET",
  #"CONFLICT_EPISODE_END_RECENT3",
  #"POST_CONFLICT_5"
  #"UCDP_SUBSTANTIVE_TERMINATION_RECENT3",
  "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3",
  "WAR_RECENT5"
  #"WAR_PREV5"
)

POOL_FE_SPEC   <- "year"
BRGLM_FE_SPECS <- c("none", "fiveyear", "decade")
BRGLM_MAXIT    <- 2000L
BRGLM_EPSILON  <- 1e-7
BRGLM_SLOWIT   <- 0.5

ROBUST_MODEL_LEVELS <- c(
  "Bias-reduced (no FE)",
  "Bias-reduced (2% trimmed)"
)

# ------------------------------------------------------------------------------
# Shared label / format machinery (hoisted to top level) -----------------------
# ------------------------------------------------------------------------------
OMIT_RE <- paste(c(
  "^\\(Intercept\\)$", "splines::ns\\(", "^ns\\(", "^factor\\(",
  "^i\\(", "^YEAR::", "^YEAR5::", "^DECADE::"
), collapse = "|")

VAR_LABS_OVERRIDE <- c(
  INSTITUTIONAL_AUTOCRACY_L1 = "Institutional autocracy (t-1)",
  BOIX_DEMOCRACY_L1 = "Democracy (t-1)",
  AUTOCRATIZATION_EPISODE_L1 = "Autocratization episode (t-1)",
  GDP_GROWTH_L1 = "Real GDP per capita growth (t-1)",
  N_TOTAL_TROOPS_L1 = "Total troops (t-1)",
  OIL_RENTS_L1 = "Oil rents (GDP) (t-1)",
  LIBERAL_DEMOCRACY_SCORE_L1 = "Liberal democracy (t-1)",
  REGIME_DURABILITY_YEARS_L1 = "Regime durability, years (t-1)",
  LOG_GDP_PER_CAPITA_L1 = "Log GDP per capita, current US\\$ (t-1)",
  LOG_ODA_RECEIVED_PER_CAPITA_L1 = "Log ODA per capita, current US\\$ (t-1)",
  ELECTORAL_DEMOCRACY_SCORE_L1 = "Electoral democracy (t-1)",
  INSTITUTIONAL_DEMOCRACY_SOCRE_L1 = "Institutional democracy (t-1)",
  CONFLICT_INTENSITY_YEAR_L1 = "Acute conflict intensity, standardized (t-1)",
  AVG_CONFLICT_INTENSITY_L1 = "Chronic conflict intensity, standardized avg. (t-1)",
  FH_DEMOCRACY_L1 = "Democracy (t-1)",
  PARTIAL_DEMOCRACY_WITH_FACTIONALISM_L1 = "Partial democracy w/ factionalism (t-1)",
  GDP_DEFLATOR_L1 = "GDP deflator (t-1)",
  N_WAR_FRONTS_L1 = "Number of war fronts (t-1)",
  REIGN_LEADER_CHANGE_RECENT3_L1 = "Leadership change, past three years (t-1)",
  UN_PKO_ONSET_RECENT3_L1 = "UN peacekeeping onset within past three years (t-1)",
  NAVCO_N_INCUMBENT_ONSET_L1 = "Incumbent-targeting mass campaign onset (t-1)",
  UCDP_SUBSTANTIVE_TERMINATION_RECENT3_L1 = "Conflict termination, past three years (t-1)"
)

auto_label <- function(x) {
  x %>%
    stringr::str_replace("_L1$", " (t-1)") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_sentence()
}

make_label <- function(v) {
  v <- as.character(v)
  lbl <- unname(VAR_LABS_OVERRIDE[v])
  miss <- is.na(lbl)
  lbl[miss] <- auto_label(v[miss])
  lbl
}

fmt_or_ci <- function(b, s, z) {
  if (is.na(b) || is.na(s)) return("")
  OR  <- exp(b); LCL <- exp(b - z * s); UCL <- exp(b + z * s)
  cell <- sprintf("%.2f [%.2f, %.2f]", OR, LCL, UCL)
  if (!is.na(LCL) && !is.na(UCL) && (UCL < 1 || LCL > 1)) {
    cell <- paste0("\\textbf{", cell, "}")
  }
  cell
}

format_model_stat <- function(statistic, value) {
  if (is.na(value)) return("")
  if (statistic %in% c("observations", "countries", "events")) {
    return(format(as.integer(round(value)), big.mark = ",", scientific = FALSE))
  }
  sprintf("%.2f", value)
}

# Tidiers -> (term, beta, se) with the SEs each estimator reports ---------------
tidy_fixest <- function(m) {
  co <- coef(m); V <- vcov(m); se <- sqrt(diag(V))
  tibble(term = names(co), beta = unname(co), se = unname(se[names(co)]))
}
tidy_glmmTMB <- function(m) {
  fe <- glmmTMB::fixef(m)$cond
  V  <- vcov(m)$cond; se <- sqrt(diag(V))
  tibble(term = names(fe), beta = unname(fe), se = unname(se[names(fe)]))
}
tidy_brglm_cl <- function(m, data, cluster_col) {
  cl <- cluster_from_glm(m, data, cluster_col)
  V  <- sandwich::vcovCL(m, cluster = cl)
  co <- coef(m); se <- sqrt(diag(V))
  tibble(term = names(co), beta = unname(co), se = unname(se[names(co)]))
}

# ------------------------------------------------------------------------------
# Helper functions -------------------------------------------------------------
# ------------------------------------------------------------------------------
apply_alias_renames <- function(df, alias_map) {
  for (canon in names(alias_map)) {
    if (!(canon %in% names(df))) {
      alias <- alias_map[[canon]]
      hit <- alias[alias %in% names(df)]
      if (length(hit) >= 1L) df <- df %>% rename(!!canon := !!sym(hit[1]))
    }
  }
  df
}

# Reconstruct the annual percentile classification used by Koami_check.ipynb.
# R's type-7 quantile is the same linear interpolation rule used by pandas.
add_koami_status <- function(df, index_var, status_var,
                             start_percentile = START_PERCENTILE,
                             exit_percentile = EXIT_PERCENTILE,
                             quantile_type = QUANTILE_TYPE) {
  if (!(index_var %in% names(df))) stop("Missing Koami index variable: ", index_var)
  if (!(YEAR %in% names(df))) stop("Missing year variable: ", YEAR)
  if (!(start_percentile < exit_percentile)) {
    stop("START_PERCENTILE must be smaller than EXIT_PERCENTILE.")
  }
  if (!is.numeric(df[[index_var]])) stop(index_var, " must be numeric.")
  
  df %>%
    group_by(.data[[YEAR]]) %>%
    mutate(
      KOAMI_START_THRESHOLD = stats::quantile(
        .data[[index_var]], probs = start_percentile,
        na.rm = TRUE, type = quantile_type, names = FALSE
      ),
      KOAMI_EXIT_THRESHOLD = stats::quantile(
        .data[[index_var]], probs = exit_percentile,
        na.rm = TRUE, type = quantile_type, names = FALSE
      ),
      !!status_var := case_when(
        is.na(.data[[index_var]]) ~ NA_character_,
        .data[[index_var]] <= KOAMI_START_THRESHOLD ~ FRAG_LAB,
        .data[[index_var]] >  KOAMI_EXIT_THRESHOLD  ~ NF_LAB,
        TRUE ~ "Transitioning"
      )
    ) %>%
    ungroup()
}

# (W3) A year in which every scored country has the same index value carries no
# cross-national information; percentile cutoffs collapse onto that value and
# every country is coded Fragile. Such years are treated as unscored.
drop_degenerate_index_years <- function(df, index_var) {
  sd_by_year <- df %>%
    group_by(.data[[YEAR]]) %>%
    summarize(n = sum(!is.na(.data[[index_var]])),
              sd = stats::sd(.data[[index_var]], na.rm = TRUE), .groups = "drop")
  bad_years <- sd_by_year %>% filter(n > 1L, !is.na(sd), sd == 0) %>% pull(.data[[YEAR]])
  if (length(bad_years)) {
    df[[index_var]][df[[YEAR]] %in% bad_years] <- NA_real_
  }
  attr(df, "degenerate_index_years") <- bad_years
  df
}

# (W4) Status equivalent of Koami's linear interpolation of the 0/1 flags across
# unscored interior years: an interpolated flag equals 1 only when both bracketing
# scored years equal 1. Leading and trailing unscored years stay NA.
fill_koami_status_gaps <- function(df, status_var) {
  df %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      .st   = .data[[status_var]],
      .prev = tidyr::fill(tibble(x = .st), x, .direction = "down")$x,
      .next = tidyr::fill(tibble(x = .st), x, .direction = "up")$x,
      KOAMI_STATUS_INTERPOLATED = as.integer(is.na(.st) & !is.na(.prev) & !is.na(.next)),
      !!status_var := case_when(
        !is.na(.st) ~ .st,
        is.na(.prev) | is.na(.next) ~ NA_character_,
        .prev == .next & .prev %in% c(FRAG_LAB, NF_LAB) ~ .prev,
        TRUE ~ "Transitioning"
      )
    ) %>%
    ungroup() %>%
    select(-.st, -.prev, -.next)
}

complete_country_year <- function(df) {
  df %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    tidyr::complete(
      !!rlang::sym(YEAR) := seq(min(.data[[YEAR]], na.rm = TRUE),
                                max(.data[[YEAR]], na.rm = TRUE), by = 1L)
    ) %>%
    ungroup() %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]])
}

as_sustained_nf_start <- function(status_vec, k) {
  n <- length(status_vec); out <- integer(n)
  nf <- (status_vec == NF_LAB); nf[is.na(nf)] <- FALSE
  if (n < k) return(out)
  for (t in seq_len(n - k + 1L)) if (nf[t] && all(nf[t:(t + k - 1L)])) out[t] <- 1L
  out
}

has_completed_fragile_run <- function(status_vec, min_run = FRAG_HISTORY_YEARS,
                                      fragile_lab = FRAG_LAB, include_current = FALSE) {
  n <- length(status_vec); out <- logical(n)
  streak <- 0L; ever_completed <- FALSE
  for (i in seq_len(n)) {
    if (!include_current) out[i] <- ever_completed
    if (!is.na(status_vec[i]) && status_vec[i] == fragile_lab) streak <- streak + 1L else streak <- 0L
    if (streak >= min_run) ever_completed <- TRUE
    if (include_current) out[i] <- ever_completed
  }
  out
}

choose_spline_df <- function(x, max_df = 3L) {
  u <- length(unique(x[!is.na(x)])); max(1L, min(max_df, u - 1L))
}

cluster_from_glm <- function(model, data, cluster_col) {
  model_rows <- rownames(model.frame(model))
  ridx <- match(model_rows, rownames(data))
  if (anyNA(ridx)) {
    ridx <- suppressWarnings(as.integer(model_rows))
  }
  if (anyNA(ridx) || any(!ridx %in% seq_len(nrow(data)))) {
    stop("Could not align model-frame rows to clustering data.")
  }
  data[[cluster_col]][ridx]
}

influence_drop <- function(glm_model, drop_frac = 0.02) {
  h <- hatvalues(glm_model)
  cutoff <- stats::quantile(h, 1 - drop_frac, na.rm = TRUE)
  which(h >= cutoff)
}

prune_for_model <- function(haz, model_vars) {
  model_vars <- intersect(model_vars, names(haz))
  haz2 <- haz %>% filter(!if_any(all_of(model_vars), is.na))
  nzv <- vapply(haz2[model_vars], function(x) {
    ux <- unique(x); ux <- ux[!is.na(ux)]; length(ux) > 1
  }, logical(1))
  list(haz = haz2, vars = model_vars[nzv], dropped_nzv = model_vars[!nzv])
}

add_time_fe_vars <- function(df) {
  df %>% mutate(YEAR5  = .data[[YEAR]] - (.data[[YEAR]] %% 5L),
                DECADE = .data[[YEAR]] - (.data[[YEAR]] %% 10L))
}

fe_var_from_spec <- function(spec) {
  spec <- match.arg(spec, c("none", "year", "fiveyear", "decade"))
  if (spec == "none")     return(NULL)
  if (spec == "year")     return(YEAR)
  if (spec == "fiveyear") return("YEAR5")
  if (spec == "decade")   return("DECADE")
}

# Capture warnings and errors so numerical problems become data, not prose.
capture_fit <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    return(list(model = NULL, error = conditionMessage(value), warnings = unique(warnings)))
  }
  list(model = value, error = NULL, warnings = unique(warnings))
}

# Stable AS_mixed fit for rare events and quasi-separated predictors. The
# estimator, formula, and estimation sample are unchanged; only initialization
# and the quasi-Fisher scoring controls differ from brglm2 defaults.
fit_brglm_stable <- function(formula, data) {
  X <- stats::model.matrix(formula, data = data)
  stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial("logit"),
    method = brglm2::brglmFit,
    start = rep(0, ncol(X)),
    control = brglm2::brglmControl(
      type = "AS_mixed",
      maxit = BRGLM_MAXIT,
      epsilon = BRGLM_EPSILON,
      slowit = BRGLM_SLOWIT
    )
  )
}

capture_tidy <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    return(list(data = NULL, error = conditionMessage(value), warnings = unique(warnings)))
  }
  list(data = value, error = NULL, warnings = unique(warnings))
}

print_fit_messages <- function(capture) {
  if (length(capture$warnings)) cat("[Warnings]", paste(capture$warnings, collapse = " | "), "\n")
  if (!is.null(capture$error)) cat("[Error]", capture$error, "\n")
}

assess_fit <- function(capture, tidy_capture, model_type) {
  m <- capture$model
  messages <- c(capture$warnings, tidy_capture$warnings, capture$error, tidy_capture$error)
  messages <- unique(messages[!is.na(messages) & nzchar(messages)])
  if (is.null(m)) {
    return(list(converged = FALSE, interpretable = FALSE, status = "Failed",
                reason = paste(messages, collapse = " | "), max_abs_beta = NA_real_))
  }
  
  converged <- TRUE
  if (model_type %in% c("glm", "brglm")) converged <- isTRUE(m$converged)
  if (model_type == "glmmTMB") {
    converged <- isTRUE(m$fit$convergence == 0L) && isTRUE(m$sdr$pdHess)
  }
  if (model_type == "fixest" && !is.null(m$convStatus)) {
    converged <- isTRUE(m$convStatus)
  }
  
  td <- tidy_capture$data
  finite_estimates <- !is.null(td) && nrow(td) > 0L &&
    all(is.finite(td$beta)) && all(is.finite(td$se))
  max_abs_beta <- if (!is.null(td) && nrow(td)) max(abs(td$beta), na.rm = TRUE) else NA_real_
  
  # A boundary-probability warning is disqualifying for ordinary/ML diagnostics,
  # but not by itself for brglm2: finite estimates under separation are one of
  # the reasons for using bias reduction. brglm2 is excluded only for actual
  # non-convergence, non-finite inference, or clearly degenerate magnitudes.
  extreme_limit <- if (model_type == "brglm") 1e6 else 15
  extreme_coefficients <- is.finite(max_abs_beta) && max_abs_beta > extreme_limit
  bad_warning_pattern <- if (model_type == "brglm") {
    "algorithm did not converge|failed to converge|non-positive-definite|false convergence|singular|NaN"
  } else {
    "did not converge|failed to converge|fitted probabilities numerically 0 or 1|non-positive-definite|false convergence|singular|NaN"
  }
  bad_warning <- any(grepl(bad_warning_pattern, messages, ignore.case = TRUE))
  interpretable <- converged && finite_estimates && !extreme_coefficients && !bad_warning
  
  reasons <- character()
  if (!converged) reasons <- c(reasons, "optimizer did not converge")
  if (!finite_estimates) reasons <- c(reasons, "non-finite coefficient or standard error")
  if (extreme_coefficients) reasons <- c(
    reasons, paste0("extreme coefficient magnitude (>", format(extreme_limit, scientific = TRUE), ")"))
  if (bad_warning) reasons <- c(reasons, messages)
  if (!length(reasons)) reasons <- "No numerical warning detected"
  
  list(
    converged = converged,
    interpretable = interpretable,
    status = if (interpretable) "Converged and interpretable" else
      if (converged) "Converged but unstable" else "Not converged",
    reason = paste(unique(reasons), collapse = " | "),
    max_abs_beta = max_abs_beta
  )
}

make_model_record <- function(K, estimator, capture, tidy_capture, model_type,
                              data, substantive_parameters, spline_df,
                              fe_spec = NA_character_, display_group = "diagnostic") {
  assessment <- assess_fit(capture, tidy_capture, model_type)
  td <- tidy_capture$data
  total_slope_parameters <- if (!is.null(td) && nrow(td)) {
    sum(td$term != "(Intercept)")
  } else NA_integer_
  
  data_used <- data
  if (model_type == "glm" && !is.null(capture$model)) {
    idx <- match(rownames(model.frame(capture$model)), rownames(data))
    if (length(idx) && all(!is.na(idx)) && all(idx %in% seq_len(nrow(data)))) {
      data_used <- data[idx, , drop = FALSE]
    }
  }
  n_obs <- if (!is.null(capture$model)) {
    suppressWarnings(tryCatch(as.integer(nobs(capture$model)), error = function(e) nrow(data_used)))
  } else nrow(data_used)
  n_countries <- dplyr::n_distinct(data_used[[COUNTRY]])
  n_events <- sum(data_used$EXIT_SUSTAINED == 1L, na.rm = TRUE)
  
  tibble(
    OUTCOME = OUTCOME_TAG,
    K = as.integer(K),
    estimator = estimator,
    fe_spec = fe_spec,
    display_group = display_group,
    converged = assessment$converged,
    interpretable = assessment$interpretable,
    status = assessment$status,
    reason = assessment$reason,
    warnings = paste(unique(c(capture$warnings, tidy_capture$warnings)), collapse = " | "),
    max_abs_beta = assessment$max_abs_beta,
    observations = n_obs,
    countries = n_countries,
    events = n_events,
    substantive_parameters = as.integer(substantive_parameters),
    spline_parameters = as.integer(spline_df),
    total_slope_parameters = total_slope_parameters,
    events_per_substantive_parameter = if_else(
      substantive_parameters > 0, n_events / substantive_parameters, NA_real_),
    events_per_total_slope_parameter = if_else(
      !is.na(total_slope_parameters) & total_slope_parameters > 0,
      n_events / total_slope_parameters, NA_real_)
  )
}

# ------------------------------------------------------------------------------
# Building hazard specification ------------------------------------------------
# ------------------------------------------------------------------------------
build_hazard <- function(df_full, predictors, k_sustain,
                         lag_years = 1L, fragile_history_years = FRAG_HISTORY_YEARS) {
  
  end_year <- df_full %>%
    group_by(.data[[COUNTRY]]) %>%
    summarize(last_year_cty = suppressWarnings(
      max(.data[[YEAR]][!is.na(.data[[STATUS]])], na.rm = TRUE)), .groups = "drop") %>%
    mutate(last_year_cty = ifelse(is.infinite(last_year_cty), NA_integer_, as.integer(last_year_cty))) %>%
    filter(!is.na(last_year_cty)) %>%
    mutate(last_eligible = last_year_cty - (k_sustain - 1L))
  
  predictors <- intersect(predictors, names(df_full))
  if (!length(predictors)) stop("None of the listed predictors are in the data.")
  lag_suffix <- paste0("_L", lag_years)
  
  ## (1) Indicators AND lagged predictors on the FULL, year-complete panel ------
  ## Lagging here (not on the risk-set subset) guarantees lags reference the true
  ## previous calendar year even across relapse/re-entry gaps.
  df1 <- df_full %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      nf_start    = as_sustained_nf_start(.data[[STATUS]], k = k_sustain),
      prev_status = dplyr::lag(.data[[STATUS]]),
      prior_fragile5  = has_completed_fragile_run(.data[[STATUS]], fragile_history_years, include_current = FALSE),
      fragile5_by_now = has_completed_fragile_run(.data[[STATUS]], fragile_history_years, include_current = TRUE),
      across(all_of(predictors), ~ dplyr::lag(.x, lag_years),
             .names = paste0("{.col}", lag_suffix))
    ) %>%
    ungroup() %>%
    mutate(QUALIFYING_EXIT = if_else(
      nf_start == 1L & !is.na(prev_status) & prev_status %in% FRAG_SET & prior_fragile5, 1L, 0L)) %>%
    left_join(end_year, by = setNames(COUNTRY, COUNTRY))
  
  df1_est <- df1 %>%
    filter(!is.na(last_eligible)) %>%
    filter(.data[[YEAR]] <= last_eligible) %>%
    select(-last_year_cty, -last_eligible)
  
  df1_est <- df1_est %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      first_exit_year = {
        yy <- .data[[YEAR]][QUALIFYING_EXIT == 1L]
        if (length(yy)) min(yy) else NA_integer_
      },
      IS_FIRST_QUALIFYING_EXIT = as.integer(
        QUALIFYING_EXIT == 1L & .data[[YEAR]] == first_exit_year),
      IS_LATER_QUALIFYING_EXIT = as.integer(
        QUALIFYING_EXIT == 1L & .data[[YEAR]] > first_exit_year),
      EXIT_SUSTAINED = IS_FIRST_QUALIFYING_EXIT,
      at_risk = if_else(
        .data[[STATUS]] %in% FRAG_SET & fragile5_by_now &
          (is.na(first_exit_year) | .data[[YEAR]] < first_exit_year), 1L, 0L)
    ) %>%
    ungroup()
  
  event_audit <- df1_est %>%
    filter(QUALIFYING_EXIT == 1L) %>%
    transmute(
      OUTCOME = OUTCOME_TAG,
      STATUS_VARIABLE = STATUS,
      K = as.integer(k_sustain),
      COUNTRY = .data[[COUNTRY]],
      YEAR = .data[[YEAR]],
      FIRST_EXIT_YEAR = first_exit_year,
      IS_FIRST_QUALIFYING_EXIT,
      IS_LATER_QUALIFYING_EXIT,
      INCLUDED_AS_MODELED_EVENT = EXIT_SUSTAINED,
      EXPECTED_FIRST_EXIT_COUNT = unname(
        EXPECTED_FIRST_EXIT_COUNTS[as.character(as.integer(k_sustain))]
      ),
      K5_CLASSIFIED_EPISODE_UNIVERSE = if_else(
        as.integer(k_sustain) == 5L, CLASSIFIED_EPISODE_UNIVERSE, NA_integer_
      )
    ) %>%
    arrange(COUNTRY, YEAR)
  
  haz0 <- df1_est %>% filter(at_risk == 1L | EXIT_SUSTAINED == 1L)
  haz0 <- haz0 %>%
    group_by(.data[[COUNTRY]]) %>%
    filter(any(at_risk == 1L) | any(EXIT_SUSTAINED == 1L)) %>%
    ungroup()
  
  ## Spell clock (re-entry aware; unchanged) ------------------------------------
  haz1 <- haz0 %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      lag_at_risk  = dplyr::lag(at_risk, default = 0L),
      spell_start  = (at_risk == 1L & lag_at_risk == 0L),
      spell_id_tmp = cumsum(spell_start),
      spell_id     = if_else(at_risk == 1L, spell_id_tmp, dplyr::lag(spell_id_tmp)),
      spell_id     = replace_na(spell_id, 0L)
    ) %>%
    group_by(.data[[COUNTRY]], spell_id) %>%
    mutate(t_at_risk = row_number() - 1L) %>%
    ungroup() %>%
    select(-lag_at_risk, -spell_start, -spell_id_tmp)
  
  haz1 <- add_time_fe_vars(haz1)
  stopifnot(all(haz1$EXIT_SUSTAINED %in% c(0L, 1L, NA)))
  first_event_countries <- haz1[[COUNTRY]][haz1$EXIT_SUSTAINED == 1L]
  stopifnot(!anyDuplicated(first_event_countries))
  attr(haz1, "event_audit") <- event_audit
  haz1
}

# ------------------------------------------------------------------------------
# Primary OR table (brglm2, K columns) -----------------------------------------
# ------------------------------------------------------------------------------
make_or_ci_table <- function(COLLECT, MODEL_STATS = NULL,
                             K_levels = c(3L, 5L, 7L), fe_spec = "none",
                             ci_level = 0.95, caption = NULL, label = NULL, file_out = NULL) {
  stopifnot(is.data.frame(COLLECT))
  req <- c("term", "beta", "se", "K", "fe_spec"); miss <- setdiff(req, names(COLLECT))
  if (length(miss)) stop("COLLECT is missing columns: ", paste(miss, collapse = ", "))
  z <- qnorm(1 - (1 - ci_level) / 2)
  
  dat <- COLLECT %>%
    filter(fe_spec == !!fe_spec) %>%
    mutate(term = as.character(term), K = as.integer(K)) %>%
    filter(K %in% K_levels) %>%
    filter(!str_detect(term, OMIT_RE)) %>%
    mutate(term_label = make_label(term),
           cell = mapply(function(b, s) fmt_or_ci(b, s, z), beta, se)) %>%
    select(term_label, K, cell)
  
  tab_full <- tidyr::complete(dat, term_label = sort(unique(dat$term_label)),
                              K = K_levels, fill = list(cell = ""))
  tab_wide <- tab_full %>%
    mutate(K = factor(K, levels = K_levels, labels = paste0("K = ", K_levels))) %>%
    pivot_wider(names_from = K, values_from = cell) %>%
    arrange(term_label) %>%
    select(all_of(c("term_label", paste0("K = ", K_levels))))
  
  if (!is.null(MODEL_STATS)) {
    stat_labels <- c(
      observations = "\\textit{Observations}",
      countries = "\\textit{Countries}",
      events = "\\textit{First-exit events}",
      events_per_substantive_parameter = "\\textit{Events per substantive parameter}",
      events_per_total_slope_parameter = "\\textit{Events per total slope parameter}"
    )
    stats_long <- MODEL_STATS %>%
      filter(display_group == "primary", fe_spec == !!fe_spec,
             interpretable, K %in% K_levels) %>%
      select(K, all_of(names(stat_labels))) %>%
      pivot_longer(-K, names_to = "statistic", values_to = "value") %>%
      mutate(
        term_label = unname(stat_labels[statistic]),
        cell = mapply(format_model_stat, statistic, value),
        K = factor(K, levels = K_levels, labels = paste0("K = ", K_levels))
      ) %>%
      select(term_label, K, cell)
    stats_wide <- tidyr::expand_grid(
      term_label = unname(stat_labels),
      K = factor(paste0("K = ", K_levels), levels = paste0("K = ", K_levels))
    ) %>%
      left_join(stats_long, by = c("term_label", "K")) %>%
      mutate(cell = replace_na(cell, "")) %>%
      mutate(term_label = factor(term_label, levels = unname(stat_labels))) %>%
      arrange(term_label) %>%
      pivot_wider(names_from = K, values_from = cell) %>%
      mutate(term_label = as.character(term_label)) %>%
      select(all_of(c("term_label", paste0("K = ", K_levels))))
    tab_wide <- bind_rows(tab_wide, stats_wide)
  }
  
  if (is.null(caption)) caption <- paste0(
    "Sustained Exit from Fragility (WGI) After Five Consecutive Fragile Years: Odds Ratios with ",
    round(ci_level * 100), "\\% Clustered CIs (brglm2), FE spec: ", fe_spec)
  if (is.null(label)) label <- paste0(
    "tab:hazard_", OUTCOME_TAG, "_fiveyear_fragile_history_", fe_spec)
  
  kb <- kable(tab_wide, format = "latex", booktabs = TRUE, linesep = "", caption = caption,
              col.names = c("Predictors", paste0("K = ", K_levels)), escape = FALSE,
              align = c("l", rep("c", length(K_levels))), label = label) %>%
    kable_styling(latex_options = c("hold_position", "striped", "scale_down"), font_size = 8) %>%
    column_spec(1, width = "6.2cm") %>%
    add_header_above(c(" " = 1, "Sustain horizon (years)" = length(K_levels))) %>%
    footnote(general = paste0(
      "Entries are odds ratios with ", round(ci_level * 100),
      "\\% confidence intervals from country-clustered SEs (sandwich::vcovCL). ",
      "Outcome is the first sustained exit to Non-Fragile status at year t, persisting for K consecutive years. ",
      "Status is reconstructed annually from the continuous WGI full-index score: Fragile at or below the yearly 30th percentile, ",
      "Non-Fragile above the yearly 35th percentile, and Transitioning between those cutoffs. ",
      "WGI begins in 1996; for unpublished interior years (1997, 1999, 2001) status is Fragile (Non-Fragile) only if both ",
      "adjacent scored years are Fragile (Non-Fragile), and Transitioning otherwise. ",
      "Eligibility requires at least five consecutive Fragile years before the exit; the risk set includes ",
      "Fragile and Transitioning country-years. Estimates are bias-reduced (brglm2, AS_mixed). ",
      "All non-binary substantive predictors are standardized. Logged GDP per capita and ODA are in current US dollars; ",
      "without calendar-time controls, their estimates may also reflect secular trends, inflation, and exchange-rate change. ",
      "Duration dependence is controlled with a spline in time-at-risk; spline and FE terms are omitted from the table.",
      " Blank cells indicate a fit excluded as numerically uninterpretable; see the convergence-status matrix.",
      if (OUTCOME_TAG == "no_rule_of_law" && 7L %in% K_levels) paste0(
        " At K = 7, total troops is omitted by design because it completely separates the observed events; ",
        "bias reduction could in principle return a finite estimate.") else ""),
      threeparttable = TRUE, escape = FALSE)
  
  if (!is.null(file_out)) writeLines(as.character(kb), file_out)
  kb
}

# ------------------------------------------------------------------------------
# Estimator-robustness table (models as columns, one horizon) ------------------
# ------------------------------------------------------------------------------
make_robust_table <- function(COLLECT_ROBUST, MODEL_STATS = NULL,
                              K_focus = ROBUST_K_FOCUS,
                              model_levels = ROBUST_MODEL_LEVELS, ci_level = 0.95,
                              caption = NULL, label = NULL, file_out = NULL) {
  stopifnot(is.data.frame(COLLECT_ROBUST))
  req <- c("term", "beta", "se", "K", "model"); miss <- setdiff(req, names(COLLECT_ROBUST))
  if (length(miss)) stop("COLLECT_ROBUST is missing columns: ", paste(miss, collapse = ", "))
  z <- qnorm(1 - (1 - ci_level) / 2)
  
  dat <- COLLECT_ROBUST %>%
    filter(K == K_focus) %>%
    mutate(term = as.character(term)) %>%
    filter(!str_detect(term, OMIT_RE)) %>%
    mutate(term_label = make_label(term),
           cell = mapply(function(b, s) fmt_or_ci(b, s, z), beta, se)) %>%
    select(model, term_label, cell)
  
  present <- intersect(model_levels, unique(dat$model))
  tab_full <- tidyr::complete(dat, term_label = sort(unique(dat$term_label)),
                              model = present, fill = list(cell = ""))
  tab_wide <- tab_full %>%
    mutate(model = factor(model, levels = present)) %>%
    pivot_wider(names_from = model, values_from = cell) %>%
    arrange(term_label) %>%
    select(all_of(c("term_label", present)))
  
  if (!is.null(MODEL_STATS)) {
    stat_labels <- c(
      observations = "\\textit{Observations}",
      countries = "\\textit{Countries}",
      events = "\\textit{First-exit events}",
      events_per_substantive_parameter = "\\textit{Events per substantive parameter}",
      events_per_total_slope_parameter = "\\textit{Events per total slope parameter}"
    )
    stats_long <- MODEL_STATS %>%
      filter(K == K_focus, interpretable, estimator %in% present) %>%
      select(estimator, all_of(names(stat_labels))) %>%
      pivot_longer(-estimator, names_to = "statistic", values_to = "value") %>%
      mutate(
        term_label = unname(stat_labels[statistic]),
        cell = mapply(format_model_stat, statistic, value),
        estimator = factor(estimator, levels = present)
      ) %>%
      select(term_label, estimator, cell)
    stats_wide <- tidyr::expand_grid(
      term_label = unname(stat_labels),
      estimator = factor(present, levels = present)
    ) %>%
      left_join(stats_long, by = c("term_label", "estimator")) %>%
      mutate(cell = replace_na(cell, "")) %>%
      mutate(term_label = factor(term_label, levels = unname(stat_labels))) %>%
      arrange(term_label) %>%
      pivot_wider(names_from = estimator, values_from = cell) %>%
      mutate(term_label = as.character(term_label)) %>%
      select(all_of(c("term_label", present)))
    tab_wide <- bind_rows(tab_wide, stats_wide)
  }
  
  if (is.null(caption)) caption <- paste0(
    "Sustained Exit from Fragility (K = ", K_focus, "): Sensitivity to Leverage Trimming. ",
    "Odds Ratios with ", round(ci_level * 100), "\\% CIs")
  if (is.null(label)) label <- paste0(
    "tab:hazard_", OUTCOME_TAG, "_leverage_sensitivity_K", K_focus)
  
  kb <- kable(tab_wide, format = "latex", booktabs = TRUE, linesep = "", caption = caption,
              col.names = c("Predictors", present), escape = FALSE,
              align = c("l", rep("c", length(present))), label = label) %>%
    kable_styling(latex_options = c("hold_position", "striped", "scale_down"), font_size = 8) %>%
    column_spec(1, width = "5.2cm") %>%
    footnote(general = paste0(
      "Entries are odds ratios with ", round(ci_level * 100), "\\% CIs at K = ", K_focus, ". ",
      "Columns are the bias-reduced logit without time FE (primary) and the same estimator re-fit after dropping the ",
      "top 2\\% of country-years by leverage. Both use country-clustered SEs. Other estimators are diagnostics and ",
      "are reported only in the convergence-status matrix. Spline terms are omitted."),
      threeparttable = TRUE, escape = FALSE)
  
  if (!is.null(file_out)) writeLines(as.character(kb), file_out)
  kb
}

make_status_table <- function(MODEL_STATUS, K_levels = K_GRID, file_out = NULL) {
  estimator_levels <- unique(MODEL_STATUS$estimator)
  status_long <- MODEL_STATUS %>%
    mutate(
      estimator = factor(estimator, levels = estimator_levels),
      K = factor(K, levels = K_levels, labels = paste0("K = ", K_levels)),
      cell = paste0(if_else(converged, "Yes", "No"), " / ",
                    if_else(interpretable, "Yes", "No"))
    ) %>%
    select(estimator, K, cell) %>%
    distinct()
  tab <- tidyr::expand_grid(
    estimator = factor(estimator_levels, levels = estimator_levels),
    K = factor(paste0("K = ", K_levels), levels = paste0("K = ", K_levels))
  ) %>%
    left_join(status_long, by = c("estimator", "K")) %>%
    mutate(cell = replace_na(cell, "")) %>%
    pivot_wider(names_from = K, values_from = cell) %>%
    arrange(estimator)
  
  kb <- kable(
    tab, format = "latex", booktabs = TRUE, linesep = "",
    caption = "Hazard-model convergence and interpretability by estimator and sustain horizon",
    col.names = c("Estimator", paste0("K = ", K_levels)), escape = FALSE,
    align = c("l", rep("c", length(K_levels))),
    label = paste0("tab:hazard_", OUTCOME_TAG, "_model_status")
  ) %>%
    kable_styling(latex_options = c("hold_position", "striped", "scale_down"), font_size = 8) %>%
    footnote(general = paste0(
      "Cells report converged / interpretable. All fits require finite estimates and standard errors. ",
      "For ordinary/ML diagnostics, boundary-probability warnings or coefficients above 15 in absolute value are disqualifying. ",
      "For brglm2, a boundary-probability warning is recorded but is not disqualifying when the algorithm converges and inference is finite; ",
      "only non-convergence, non-finite inference, or clearly degenerate coefficients above 1e6 are disqualifying. ",
      "The accompanying CSV records numerical warnings and reasons."),
      threeparttable = TRUE, escape = FALSE)
  if (!is.null(file_out)) writeLines(as.character(kb), file_out)
  kb
}

# ------------------------------------------------------------------------------
# Load & prep -------------------------------------------------------------------
# ------------------------------------------------------------------------------
stopifnot(exists("standardized_data_final"))

df <- standardized_data_final %>%
  mutate(!!YEAR := suppressWarnings(as.integer(.data[[YEAR]])))

# (W3) Blank degenerate (constant) index years before percentiles are computed.
if (DROP_DEGENERATE_INDEX_YEARS) {
  df <- drop_degenerate_index_years(df, INDEX_VAR)
  cat("\n[Audit] Index years with zero cross-country variance treated as unscored:",
      if (length(attr(df, "degenerate_index_years"))) paste(attr(df, "degenerate_index_years"), collapse = ", ") else "none",
      "\n")
}

df <- add_koami_status(
  df,
  index_var = INDEX_VAR,
  status_var = STATUS
)

cat("\n[Audit] Koami classification index:", INDEX_VAR, "\n")
cat("[Audit] Annual percentile rule: <=", START_PERCENTILE,
    "= Fragile; >", EXIT_PERCENTILE, "= Non Fragile; otherwise Transitioning.\n")
cat("[Audit] Unique reconstructed STATUS labels:\n"); print(sort(unique(df[[STATUS]])))
cat("[Audit] Reconstructed STATUS counts:\n"); print(table(df[[STATUS]], useNA = "ifany"))

if (LEGACY_STATUS %in% names(df)) {
  cat("\n[Audit] Legacy versus Koami-reconstructed STATUS:\n")
  print(table(
    Legacy = as.character(df[[LEGACY_STATUS]]),
    Koami = as.character(df[[STATUS]]),
    useNA = "ifany"
  ))
}

alias_map <- list(
  INSTITUTIONAL_AUTOCRACY = c("INSTITUTIONAL_AUTOCRACY", "INSTITUTIONAL_AUTOCRACY_SCORE"),
  LOG_GDP_PER_CAPITA = c("LOG_GDP_PER_CAPITA", "LOG_GDP_PC", "LOG_GDP_PER_CAP"),
  GDP_DEFLATOR = c("GDP_DEFLATOR", "GDP_DEFLATOR_L1_BASE"),
  AVG_CONFLICT_INTENSITY = c("AVG_CONFLICT_INTENSITY", "CONFLICT_INTENSITY_AVG")
)
df <- apply_alias_renames(df, alias_map)

missing_required <- setdiff(FINAL_VARS_BASE, names(df))
if (length(missing_required)) stop(
  "These required predictors are missing pre-lag: ", paste(missing_required, collapse = ", "),
  "\nFix names or extend alias_map.")

is_binary01 <- function(x) { ux <- unique(x[!is.na(x)]); all(ux %in% c(0, 1)) && length(ux) <= 2 }
binary_vars <- intersect(
  c("FH_DEMOCRACY", "BOIX_DEMOCRACY", "PARTIAL_DEMOCRACY_WITH_FACTIONALISM",
    "REIGN_LEADER_CHANGE_RECENT3", "UN_PKO_ONSET_RECENT3", "UCDP_SUBSTANTIVE_TERMINATION_RECENT3",
    "WAR_SHORT", "WAR_CHRONIC", "POST_CONFLICT_5", "CONFLICT_EPISODE_END_RECENT3", "WAR",
    "UCDP_SUBSTANTIVE_COUNTRY_CESSATION_RECENT3", "WAR_PREV5", "BMR_ANY_TRANSITION_RECENT5",
    "WAR_RECENT5"), FINAL_VARS_BASE)

for (v in binary_vars) {
  if (!is.numeric(df[[v]])) stop(v, " is not numeric. Recode to 0/1.")
  if (!is_binary01(df[[v]])) stop(v, " is numeric but not binary 0/1. Recode.")
}

num_vars <- setdiff(FINAL_VARS_BASE, binary_vars)
for (v in num_vars) if (is.numeric(df[[v]])) df[[v]] <- as.numeric(scale(df[[v]]))

cat("\n[Audit] Rows in original df:", nrow(df), "\n")
df_full <- complete_country_year(df)
cat("[Audit] Rows after country-year completion:", nrow(df_full), "\n")

# (W4) Koami-equivalent status for unscored interior years (after completion, so
# gap rows added by complete_country_year are also covered).
if (FILL_INTERIOR_STATUS_GAPS) {
  df_full <- fill_koami_status_gaps(df_full, STATUS)
  cat("[Audit] Interior unscored country-years assigned a Koami-interpolated status:",
      sum(df_full$KOAMI_STATUS_INTERPOLATED == 1L, na.rm = TRUE), "\n")
  print(table(Year = df_full[[YEAR]][df_full$KOAMI_STATUS_INTERPOLATED == 1L],
              Status = df_full[[STATUS]][df_full$KOAMI_STATUS_INTERPOLATED == 1L]))
}

# ------------------------------------------------------------------------------
# Master loop -------------------------------------------------------------------
# ------------------------------------------------------------------------------
MODELS        <- list()
COLLECT       <- tibble()   # brglm2 across FE specs & K   (primary table)
COLLECT_ROBUST <- tibble()  # interpretable bias-reduced sensitivity models only
MODEL_STATUS  <- tibble()   # convergence, interpretability, and model counts
EVENT_AUDIT   <- tibble()   # every qualifying exit, including later exits
EVENT_SUMMARY <- tibble()   # K-specific reconciliation and complete-case counts

lag_suffix     <- paste0("_L", LAG_YEARS)
FINAL_LAG_VARS <- paste0(FINAL_VARS_BASE, lag_suffix)

for (K in K_GRID) {
  cat("\n================= K =", K, "=================\n")
  
  haz_raw <- build_hazard(df_full, FINAL_VARS_BASE, K, LAG_YEARS, FRAG_HISTORY_YEARS)
  audit_k <- attr(haz_raw, "event_audit")
  if (is.null(audit_k)) stop("Event audit attribute is missing at K = ", K)
  EVENT_AUDIT <- bind_rows(EVENT_AUDIT, audit_k)
  evt_qualifying_raw <- nrow(audit_k)
  evt_first_raw <- sum(audit_k$IS_FIRST_QUALIFYING_EXIT == 1L, na.rm = TRUE)
  evt_later_raw <- sum(audit_k$IS_LATER_QUALIFYING_EXIT == 1L, na.rm = TRUE)
  expected_first_this_k <- unname(EXPECTED_FIRST_EXIT_COUNTS[as.character(K)])
  if (length(expected_first_this_k) != 1L || is.na(expected_first_this_k)) {
    stop("No expected first-exit count configured for K = ", K)
  }
  cat("[Audit] Risk-set + first-event rows:", nrow(haz_raw),
      " | raw qualifying exits:", evt_qualifying_raw,
      " | first exits:", evt_first_raw,
      " | later exits excluded:", evt_later_raw,
      " | expected first exits:", expected_first_this_k, "\n")
  if (evt_first_raw != expected_first_this_k) {
    stop(
      "Koami reconciliation failed at K = ", K,
      ": found ", evt_first_raw,
      " first exits but expected ", expected_first_this_k, "."
    )
  }
  if (K == 5L) {
    observed_k5 <- audit_k %>%
      filter(IS_FIRST_QUALIFYING_EXIT == 1L) %>%
      transmute(COUNTRY = as.character(COUNTRY), YEAR = as.integer(YEAR))
    expected_k5 <- tibble(
      COUNTRY = names(EXPECTED_K5_FIRST_EXITS),
      YEAR = as.integer(unname(EXPECTED_K5_FIRST_EXITS))
    )
    missing_k5 <- anti_join(expected_k5, observed_k5, by = c("COUNTRY", "YEAR"))
    extra_k5 <- anti_join(observed_k5, expected_k5, by = c("COUNTRY", "YEAR"))
    if (nrow(missing_k5) || nrow(extra_k5)) {
      cat("\n[Reconciliation failure] Expected Koami cases not found:\n")
      print(missing_k5)
      cat("[Reconciliation failure] Unexpected K=5 first exits:\n")
      print(extra_k5)
      stop("The K = 5 first-exit set does not match the ", CLASSIFIED_EPISODE_UNIVERSE,
           "-case Koami table.")
    }
    cat("[Reconciliation] Exact country-year match to all", CLASSIFIED_EPISODE_UNIVERSE,
        "Koami K=5 cases: YES\n")
  }
  if (evt_first_raw == 0L || evt_first_raw == nrow(haz_raw)) {
    EVENT_SUMMARY <- bind_rows(EVENT_SUMMARY, tibble(
      OUTCOME = OUTCOME_TAG, K = K, risk_set_plus_first_event_rows = nrow(haz_raw),
      raw_qualifying_event_rows = evt_qualifying_raw, raw_first_exit_events = evt_first_raw,
      raw_later_exit_rows = evt_later_raw, complete_case_rows = NA_integer_,
      complete_case_first_exit_events = NA_integer_, complete_case_countries = NA_integer_,
      expected_first_exit_count = expected_first_this_k,
      raw_first_minus_expected = evt_first_raw - expected_first_this_k,
      k5_classified_episode_universe = if_else(
        K == 5L, CLASSIFIED_EPISODE_UNIVERSE, NA_integer_
      )
    ))
    cat("Skipping K =", K, "\n")
    next
  }
  
  missing_lags <- setdiff(FINAL_LAG_VARS, names(haz_raw))
  if (length(missing_lags)) stop("Missing lagged columns: ", paste(missing_lags, collapse = ", "))
  
  pr <- prune_for_model(haz_raw, FINAL_LAG_VARS)
  haz <- pr$haz; core_used <- pr$vars
  if (length(pr$dropped_nzv)) cat("[Core] Dropped zero-variance:", paste(pr$dropped_nzv, collapse = ", "), "\n")
  evt <- sum(haz$EXIT_SUSTAINED == 1L, na.rm = TRUE)
  cty <- n_distinct(haz[[COUNTRY]])
  cat("[Core] Rows:", nrow(haz), " | countries:", cty, " | first-exit events:", evt, "\n")
  EVENT_SUMMARY <- bind_rows(EVENT_SUMMARY, tibble(
    OUTCOME = OUTCOME_TAG, K = K, risk_set_plus_first_event_rows = nrow(haz_raw),
    raw_qualifying_event_rows = evt_qualifying_raw, raw_first_exit_events = evt_first_raw,
    raw_later_exit_rows = evt_later_raw, complete_case_rows = nrow(haz),
    complete_case_first_exit_events = evt, complete_case_countries = cty,
    expected_first_exit_count = expected_first_this_k,
    raw_first_minus_expected = evt_first_raw - expected_first_this_k,
    k5_classified_episode_universe = if_else(
      K == 5L, CLASSIFIED_EPISODE_UNIVERSE, NA_integer_
    )
  ))
  if (K == 5L) cat("[Reconciliation] First qualifying K=5 exits:", evt_first_raw,
                   " | classified episode universe:", CLASSIFIED_EPISODE_UNIVERSE,
                   " | difference:", evt_first_raw - CLASSIFIED_EPISODE_UNIVERSE, "\n")
  if (evt == 0L || evt == nrow(haz)) { cat("Skipping K =", K, "(constant after pruning)\n"); next }
  
  df_spline <- choose_spline_df(haz$t_at_risk, max_df = 3L)
  cat("[Duration] spline df =", df_spline, "\n")
  
  # (1) Pooled fixest logit (year FE) -----------------------------------------
  fe_var_pool  <- fe_var_from_spec(POOL_FE_SPEC)
  fe_term_pool <- if (is.null(fe_var_pool)) "" else paste0(" + i(", fe_var_pool, ")")
  f_pool <- as.formula(paste0("EXIT_SUSTAINED ~ ", paste(core_used, collapse = " + "),
                              " + splines::ns(t_at_risk, ", df_spline, ")", fe_term_pool))
  cap_pool <- capture_fit(fixest::feglm(f_pool, data = haz, family = "logit", cluster = COUNTRY))
  m_pool <- cap_pool$model
  td_pool <- if (is.null(m_pool)) list(data = NULL, error = cap_pool$error) else capture_tidy(tidy_fixest(m_pool))
  cat("\n=== (1) Pooled logit (fixest) | FE:", POOL_FE_SPEC, "===\n")
  print_fit_messages(cap_pool)
  if (!is.null(m_pool)) print(summary(m_pool))
  pool_record <- make_model_record(
    K, "Pooled logit (year FE)", cap_pool, td_pool, "fixest", haz,
    length(core_used), df_spline, display_group = "diagnostic")
  MODEL_STATUS <- bind_rows(MODEL_STATUS, pool_record)
  cat("[Status]", pool_record$status, "-", pool_record$reason, "\n")
  if (!is.null(m_pool)) MODELS[[paste0("K", K, "_pool")]] <- m_pool
  
  # (1b) Influence via plain GLM (no FE) --------------------------------------
  f_glm <- as.formula(paste0("EXIT_SUSTAINED ~ ", paste(core_used, collapse = " + "),
                             " + splines::ns(t_at_risk, ", df_spline, ")"))
  cap_glm <- capture_fit(glm(f_glm, data = haz, family = binomial("logit")))
  m_glm <- cap_glm$model
  td_glm <- if (is.null(m_glm)) list(data = NULL, error = cap_glm$error) else capture_tidy({
    co <- coef(m_glm); se <- sqrt(diag(vcov(m_glm)))
    tibble(term = names(co), beta = unname(co), se = unname(se[names(co)]))
  })
  glm_record <- make_model_record(
    K, "Ordinary logit (leverage diagnostic)", cap_glm, td_glm, "glm", haz,
    length(core_used), df_spline, display_group = "diagnostic")
  MODEL_STATUS <- bind_rows(MODEL_STATUS, glm_record)
  idx_hi <- if (is.null(m_glm)) integer() else tryCatch(
    influence_drop(m_glm, drop_frac = DROP_TOP_LEVERAGE), error = function(e) integer())
  if (!is.null(m_glm)) MODELS[[paste0("K", K, "_glm")]] <- m_glm
  
  # (2) brglm2 rare-events models (none / fiveyear / decade) ------------------
  for (fe_spec in BRGLM_FE_SPECS) {
    fe_var  <- fe_var_from_spec(fe_spec)
    fe_term <- if (is.null(fe_var)) "" else paste0(" + factor(", fe_var, ")")
    f_br <- as.formula(paste0("EXIT_SUSTAINED ~ ", paste(core_used, collapse = " + "),
                              " + splines::ns(t_at_risk, ", df_spline, ")", fe_term))
    cap_br <- capture_fit(fit_brglm_stable(f_br, haz))
    m_brglm <- cap_br$model
    td_br <- if (is.null(m_brglm)) list(data = NULL, error = cap_br$error) else
      capture_tidy(tidy_brglm_cl(m_brglm, haz, COUNTRY))
    cat("\n=== (2) brglm2 | FE:", fe_spec, "===\n")
    print_fit_messages(cap_br)
    estimator_name <- paste0("Bias-reduced (", if_else(fe_spec == "none", "no FE", paste0(fe_spec, " FE")), ")")
    br_record <- make_model_record(
      K, estimator_name, cap_br, td_br, "brglm", haz, length(core_used), df_spline,
      fe_spec = fe_spec, display_group = "primary")
    MODEL_STATUS <- bind_rows(MODEL_STATUS, br_record)
    cat("[Status]", br_record$status, "-", br_record$reason, "\n")
    if (isTRUE(br_record$interpretable)) {
      tmp <- td_br$data %>% mutate(K = K, fe_spec = fe_spec)
      COLLECT <- bind_rows(COLLECT, tmp %>% select(K, fe_spec, term, beta, se))
      if (fe_spec == "none") COLLECT_ROBUST <- bind_rows(COLLECT_ROBUST,
                                                         tmp %>% transmute(K, term, beta, se, model = "Bias-reduced (no FE)"))
    }
    if (!is.null(m_brglm)) {
      print(lmtest::coeftest(m_brglm, vcov. = sandwich::vcovCL(
        m_brglm, cluster = cluster_from_glm(m_brglm, haz, COUNTRY))))
      MODELS[[paste0("K", K, "_brglm_", fe_spec)]] <- m_brglm
    }
  }
  
  # (2b) Leverage-trimmed re-estimation of the PRIMARY brglm2 (no FE) ----------
  if (length(idx_hi)) {
    haz_rb <- haz[-idx_hi, , drop = FALSE]
    evt_rb <- sum(haz_rb$EXIT_SUSTAINED == 1L, na.rm = TRUE)
    cat("\n--- Influence: dropped top ", round(100 * DROP_TOP_LEVERAGE, 1),
        "% leverage (n = ", length(idx_hi), "); events now = ", evt_rb, " ---\n", sep = "")
    cap_trim <- capture_fit(fit_brglm_stable(f_glm, haz_rb))
    m_brglm_trim <- cap_trim$model
    td_trim <- if (is.null(m_brglm_trim)) list(data = NULL, error = cap_trim$error) else
      capture_tidy(tidy_brglm_cl(m_brglm_trim, haz_rb, COUNTRY))
    trim_record <- make_model_record(
      K, "Bias-reduced (2% trimmed)", cap_trim, td_trim, "brglm", haz_rb,
      length(core_used), df_spline, fe_spec = "none", display_group = "robustness")
    MODEL_STATUS <- bind_rows(MODEL_STATUS, trim_record)
    print_fit_messages(cap_trim)
    cat("[Status]", trim_record$status, "-", trim_record$reason, "\n")
    if (isTRUE(trim_record$interpretable)) {
      COLLECT_ROBUST <- bind_rows(COLLECT_ROBUST,
                                  td_trim$data %>%
                                    mutate(K = K, model = "Bias-reduced (2% trimmed)"))
    }
    if (!is.null(m_brglm_trim)) MODELS[[paste0("K", K, "_brglm_trim")]] <- m_brglm_trim
  } else {
    cap_trim <- list(model = NULL, error = "No leverage observations selected for trimming", warnings = character())
    td_trim <- list(data = NULL, error = cap_trim$error)
    MODEL_STATUS <- bind_rows(MODEL_STATUS, make_model_record(
      K, "Bias-reduced (2% trimmed)", cap_trim, td_trim, "brglm", haz,
      length(core_used), df_spline, fe_spec = "none", display_group = "robustness"))
  }
  
  # (3) Random-intercept logit -------------------------------------------------
  f_re <- as.formula(paste0("EXIT_SUSTAINED ~ ", paste(core_used, collapse = " + "),
                            " + splines::ns(t_at_risk, ", df_spline, ") + (1 | ", COUNTRY, ")"))
  cap_re <- capture_fit(glmmTMB::glmmTMB(f_re, data = haz, family = binomial(),
                                         control = glmmTMB::glmmTMBControl(optimizer = optim,
                                                                           optArgs = list(method = "BFGS"), optCtrl = list(maxit = 1e4))))
  m_re <- cap_re$model
  td_re <- if (is.null(m_re)) list(data = NULL, error = cap_re$error) else capture_tidy(tidy_glmmTMB(m_re))
  cat("\n=== (3) Random-intercept logit (country) ===\n")
  print_fit_messages(cap_re)
  if (!is.null(m_re)) print(summary(m_re))
  re_record <- make_model_record(
    K, "Random intercept", cap_re, td_re, "glmmTMB", haz,
    length(core_used), df_spline, display_group = "diagnostic")
  MODEL_STATUS <- bind_rows(MODEL_STATUS, re_record)
  cat("[Status]", re_record$status, "-", re_record$reason, "\n")
  if (!is.null(m_re)) MODELS[[paste0("K", K, "_re")]] <- m_re
  
  # (4) Absorbed two-way FE logit ---------------------------------------------
  fe_var_abs <- fe_var_from_spec(POOL_FE_SPEC)
  abs_rhs <- if (is.null(fe_var_abs)) COUNTRY else paste0(COUNTRY, " + ", fe_var_abs)
  f_abs <- as.formula(paste0("EXIT_SUSTAINED ~ ", paste(core_used, collapse = " + "),
                             " + splines::ns(t_at_risk, ", df_spline, ") | ", abs_rhs))
  cap_abs <- capture_fit(fixest::feglm(f_abs, data = haz, family = "logit", cluster = COUNTRY))
  m_abs <- cap_abs$model
  td_abs <- if (is.null(m_abs)) list(data = NULL, error = cap_abs$error) else capture_tidy(tidy_fixest(m_abs))
  cat("\n=== (4) Absorbed FE logit | FE:", POOL_FE_SPEC, "===\n")
  print_fit_messages(cap_abs)
  if (!is.null(m_abs)) print(summary(m_abs))
  abs_record <- make_model_record(
    K, "Two-way FE logit", cap_abs, td_abs, "fixest", haz,
    length(core_used), df_spline, display_group = "diagnostic")
  MODEL_STATUS <- bind_rows(MODEL_STATUS, abs_record)
  cat("[Status]", abs_record$status, "-", abs_record$reason, "\n")
  if (!is.null(m_abs)) MODELS[[paste0("K", K, "_abs")]] <- m_abs
  
  cat("\n================= End K =", K, "=================\n")
}

cat("\n[Done] Primary estimates are in COLLECT; leverage sensitivity estimates are in ",
    "COLLECT_ROBUST; diagnostic status and counts are in MODEL_STATUS.\n", sep = "")

# ------------------------------------------------------------------------------
# Build LaTeX tables ------------------------------------------------------------
# ------------------------------------------------------------------------------
if (nrow(COLLECT) == 0L) {
  cat("\n[Primary-model status]\n")
  print(MODEL_STATUS %>%
          filter(display_group == "primary") %>%
          select(K, estimator, converged, interpretable, status, reason, warnings))
  stop(paste0(
    "No bias-reduced model passed the estimator-specific reporting checks. ",
    "Inspect the primary-model status printed above; no table was generated."),
    call. = FALSE)
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

stopifnot(!any(EVENT_AUDIT$IS_LATER_QUALIFYING_EXIT == 1L &
                 EVENT_AUDIT$INCLUDED_AS_MODELED_EVENT == 1L))
stopifnot(!anyDuplicated(EVENT_AUDIT %>%
                           filter(INCLUDED_AS_MODELED_EVENT == 1L) %>%
                           select(K, COUNTRY)))

utils::write.csv(
  EVENT_AUDIT,
  file.path(OUTPUT_DIR, paste0("hazard_event_audit_", OUTCOME_TAG, ".csv")),
  row.names = FALSE, na = ""
)
utils::write.csv(
  EVENT_SUMMARY,
  file.path(OUTPUT_DIR, paste0("hazard_event_summary_", OUTCOME_TAG, ".csv")),
  row.names = FALSE, na = ""
)
utils::write.csv(
  MODEL_STATUS,
  file.path(OUTPUT_DIR, paste0("hazard_model_status_", OUTCOME_TAG, ".csv")),
  row.names = FALSE, na = ""
)

# Primary table (brglm2, no FE) + FE-variant tables
kb_no_fe <- make_or_ci_table(
  COLLECT, MODEL_STATUS, fe_spec = "none",
  file_out = file.path(OUTPUT_DIR, paste0("table_hazard_", OUTCOME_TAG, "_primary_noFE.tex"))
)
kb_fiveyear_fe <- if (any(COLLECT$fe_spec == "fiveyear")) make_or_ci_table(
  COLLECT, MODEL_STATUS, fe_spec = "fiveyear",
  file_out = file.path(OUTPUT_DIR, paste0("table_hazard_", OUTCOME_TAG, "_fiveyearFE.tex"))
) else NULL
kb_decade_fe <- if (any(COLLECT$fe_spec == "decade")) make_or_ci_table(
  COLLECT, MODEL_STATUS, fe_spec = "decade",
  file_out = file.path(OUTPUT_DIR, paste0("table_hazard_", OUTCOME_TAG, "_decadeFE.tex"))
) else NULL
cat(as.character(kb_no_fe))

# Leverage sensitivity and convergence-status tables
kb_robust <- if (nrow(COLLECT_ROBUST)) make_robust_table(
  COLLECT_ROBUST, MODEL_STATUS, K_focus = ROBUST_K_FOCUS,
  file_out = file.path(OUTPUT_DIR, paste0(
    "table_hazard_", OUTCOME_TAG, "_leverage_sensitivity_K", ROBUST_K_FOCUS, ".tex"))
) else NULL
if (!is.null(kb_robust)) cat(as.character(kb_robust))

kb_status <- make_status_table(
  MODEL_STATUS,
  file_out = file.path(OUTPUT_DIR, paste0("table_hazard_", OUTCOME_TAG, "_model_status.tex"))
)
cat(as.character(kb_status))
