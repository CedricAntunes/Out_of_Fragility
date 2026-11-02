# ==============================================================================
# Alternative democracy measures and sustained exit from fragility (K = 5)
# ==============================================================================
#
# This standalone script follows the two Koami-validated hazard pipelines:
#   * full index (with rule of law): annual 30th/35th percentile classification
#   * no-rule-of-law index:         annual 30th/35th percentile classification
#
# For each outcome it estimates five separate bias-reduced discrete-time logit
# models, first with democracy as the only substantive predictor and then with
# standardized log GDP per capita added. Every model also includes the duration
# spline used in the validated pipelines. Predictors are lagged one calendar
# year on the completed country-year panel, and 95% confidence intervals use
# country-clustered sandwich standard errors.
#
# Binary democracy measures:
#   GWF_DEMOCRACY, DD_DEMOCRACY, FH_DEMOCRACY, BOIX_DEMOCRACY
# Continuous democracy measure (standardized before lagging):
#   INSTITUTIONAL_DEMOCRACY
#
# The alias block below recognizes the spelling variants found in the project,
# including INSTITUTIONAL_DEMOCRACY_SOCRE and INSITUTIONAL_DEMOCRACY.
# ==============================================================================

# Packages ---------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "tibble", "rlang", "ggplot2",
                       "brglm2", "sandwich")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the following packages before running the script: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(rlang)
  library(ggplot2)
})

# User settings ----------------------------------------------------------------
DATA_CANDIDATES <- c(
  "standardized_data_final.RDS",
  "project_sources/11-standardized_data_final.RDS"
)
OUTPUT_DIR <- "democracy_coefplots_K5"

K_SUSTAIN            <- 5L
LAG_YEARS             <- 1L
FRAG_HISTORY_YEARS    <- 5L
START_PERCENTILE      <- 0.30
EXIT_PERCENTILE       <- 0.35
QUANTILE_TYPE         <- 7L
CI_LEVEL              <- 0.95

# Keep TRUE for a clean comparison between the bivariate and GDP-adjusted
# estimates: for a given democracy measure, both models then use the rows with
# nonmissing democracy and GDP. Set FALSE if the bivariate models should instead
# use every row with nonmissing democracy, even when GDP is missing.
PAIRED_SAMPLES_WITH_GDP <- TRUE

# Stable brglm2 controls copied from the Koami-validated pipelines.
BRGLM_MAXIT   <- 2000L
BRGLM_EPSILON <- 1e-7
BRGLM_SLOWIT  <- 0.5

COUNTRY <- "COUNTRY_NAME"
YEAR    <- "YEAR"
NF_LAB   <- "Non Fragile"
FRAG_LAB <- "Fragile"
FRAG_SET <- c("Fragile", "Transitioning")

FULL_INDEX_VAR <- "NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y"
NORL_INDEX_VAR <- "NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED"
FULL_STATUS    <- "VDEM_STATUS_IDEAL_KOAMI"
NORL_STATUS    <- "VDEM_STATUS_NORL_IDEAL_KOAMI"

DEM_VARS <- c(
  "GWF_DEMOCRACY",
  "DD_DEMOCRACY",
  "FH_DEMOCRACY",
  "BOIX_DEMOCRACY",
  "INSTITUTIONAL_DEMOCRACY"
)
GDP_VAR <- "LOG_GDP_PER_CAPITA"

DEM_LABELS <- c(
  GWF_DEMOCRACY           = "GWF democracy",
  DD_DEMOCRACY            = "DD democracy",
  FH_DEMOCRACY            = "Freedom House democracy",
  BOIX_DEMOCRACY          = "Boix democracy",
  INSTITUTIONAL_DEMOCRACY = "Institutional democracy (1 SD)"
)

OUTCOME_SPECS <- tribble(
  ~outcome,          ~outcome_label,                  ~status_var, ~expected_events,
  "full_index",     "Index with rule of law",        FULL_STATUS, 36L,
  "no_rule_of_law", "Index without rule of law",     NORL_STATUS, 39L
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# General helpers --------------------------------------------------------------
resolve_existing_path <- function(candidates) {
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Data file not found. Checked: ", paste(candidates, collapse = "; "))
  }
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

apply_alias_renames <- function(df, alias_map) {
  for (canonical_name in names(alias_map)) {
    if (!(canonical_name %in% names(df))) {
      candidates <- alias_map[[canonical_name]]
      hit <- candidates[candidates %in% names(df)]
      if (length(hit)) {
        message("Using ", hit[1], " as ", canonical_name, ".")
        df <- dplyr::rename(df, !!canonical_name := !!rlang::sym(hit[1]))
      }
    }
  }
  df
}

is_binary01 <- function(x) {
  observed <- unique(x[!is.na(x)])
  length(observed) <= 2L && all(observed %in% c(0, 1))
}

coerce_binary01 <- function(x, variable_name) {
  if (is.logical(x)) x <- as.integer(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    bad <- unique(x[!is.na(x) & !(x %in% c("0", "1"))])
    if (length(bad)) {
      stop(variable_name, " is not coded 0/1. Unexpected values: ",
           paste(head(bad, 10L), collapse = ", "))
    }
    x <- as.numeric(x)
  }
  if (!is.numeric(x) || !is_binary01(x)) {
    stop(variable_name, " must be binary and coded 0/1.")
  }
  as.numeric(x)
}

standardize_numeric <- function(x, variable_name) {
  if (!is.numeric(x)) stop(variable_name, " must be numeric.")
  observed_sd <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(observed_sd) || observed_sd == 0) {
    stop(variable_name, " has zero or undefined standard deviation.")
  }
  as.numeric(scale(x))
}

# Koami annual percentile classification --------------------------------------
add_koami_status <- function(df, index_var, status_var) {
  if (!(index_var %in% names(df))) stop("Missing Koami index variable: ", index_var)
  if (!is.numeric(df[[index_var]])) stop(index_var, " must be numeric.")
  
  df %>%
    group_by(.data[[YEAR]]) %>%
    mutate(
      .koami_start = stats::quantile(
        .data[[index_var]], probs = START_PERCENTILE,
        na.rm = TRUE, type = QUANTILE_TYPE, names = FALSE
      ),
      .koami_exit = stats::quantile(
        .data[[index_var]], probs = EXIT_PERCENTILE,
        na.rm = TRUE, type = QUANTILE_TYPE, names = FALSE
      ),
      !!status_var := case_when(
        is.na(.data[[index_var]])       ~ NA_character_,
        .data[[index_var]] <= .koami_start ~ FRAG_LAB,
        .data[[index_var]] >  .koami_exit  ~ NF_LAB,
        TRUE ~ "Transitioning"
      )
    ) %>%
    ungroup() %>%
    select(-.koami_start, -.koami_exit)
}

complete_country_year <- function(df) {
  df %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    tidyr::complete(
      !!rlang::sym(YEAR) := seq(
        min(.data[[YEAR]], na.rm = TRUE),
        max(.data[[YEAR]], na.rm = TRUE),
        by = 1L
      )
    ) %>%
    ungroup() %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]])
}

as_sustained_nf_start <- function(status_vec, k) {
  n <- length(status_vec)
  out <- integer(n)
  nf <- status_vec == NF_LAB
  nf[is.na(nf)] <- FALSE
  if (n < k) return(out)
  for (t in seq_len(n - k + 1L)) {
    if (nf[t] && all(nf[t:(t + k - 1L)])) out[t] <- 1L
  }
  out
}

has_completed_fragile_run <- function(status_vec, min_run, include_current = FALSE) {
  n <- length(status_vec)
  out <- logical(n)
  streak <- 0L
  ever_completed <- FALSE
  for (i in seq_len(n)) {
    if (!include_current) out[i] <- ever_completed
    if (!is.na(status_vec[i]) && status_vec[i] == FRAG_LAB) {
      streak <- streak + 1L
    } else {
      streak <- 0L
    }
    if (streak >= min_run) ever_completed <- TRUE
    if (include_current) out[i] <- ever_completed
  }
  out
}

choose_spline_df <- function(x, max_df = 3L) {
  unique_n <- length(unique(x[!is.na(x)]))
  max(1L, min(max_df, unique_n - 1L))
}

# Exact K=5, first-exit risk-set construction used in the validated pipelines.
build_hazard <- function(df_full, predictors, status_var, outcome_tag) {
  end_year <- df_full %>%
    group_by(.data[[COUNTRY]]) %>%
    summarize(
      last_year_cty = suppressWarnings(
        max(.data[[YEAR]][!is.na(.data[[status_var]])], na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      last_year_cty = ifelse(
        is.infinite(last_year_cty), NA_integer_, as.integer(last_year_cty)
      ),
      last_eligible = last_year_cty - (K_SUSTAIN - 1L)
    ) %>%
    filter(!is.na(last_year_cty))
  
  predictors <- intersect(predictors, names(df_full))
  if (!length(predictors)) stop("None of the requested predictors are present.")
  lag_suffix <- paste0("_L", LAG_YEARS)
  
  df1 <- df_full %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      nf_start = as_sustained_nf_start(.data[[status_var]], K_SUSTAIN),
      prev_status = dplyr::lag(.data[[status_var]]),
      prior_fragile5 = has_completed_fragile_run(
        .data[[status_var]], FRAG_HISTORY_YEARS, include_current = FALSE
      ),
      fragile5_by_now = has_completed_fragile_run(
        .data[[status_var]], FRAG_HISTORY_YEARS, include_current = TRUE
      ),
      across(
        all_of(predictors),
        ~ dplyr::lag(.x, LAG_YEARS),
        .names = paste0("{.col}", lag_suffix)
      )
    ) %>%
    ungroup() %>%
    mutate(
      QUALIFYING_EXIT = if_else(
        nf_start == 1L & !is.na(prev_status) &
          prev_status %in% FRAG_SET & prior_fragile5,
        1L, 0L
      )
    ) %>%
    left_join(end_year, by = setNames(COUNTRY, COUNTRY)) %>%
    filter(!is.na(last_eligible), .data[[YEAR]] <= last_eligible) %>%
    select(-last_year_cty, -last_eligible)
  
  df1_est <- df1 %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      first_exit_year = {
        event_years <- .data[[YEAR]][QUALIFYING_EXIT == 1L]
        if (length(event_years)) min(event_years) else NA_integer_
      },
      IS_FIRST_QUALIFYING_EXIT = as.integer(
        QUALIFYING_EXIT == 1L & .data[[YEAR]] == first_exit_year
      ),
      EXIT_SUSTAINED = IS_FIRST_QUALIFYING_EXIT,
      at_risk = if_else(
        .data[[status_var]] %in% FRAG_SET & fragile5_by_now &
          (is.na(first_exit_year) | .data[[YEAR]] < first_exit_year),
        1L, 0L
      )
    ) %>%
    ungroup()
  
  event_audit <- df1_est %>%
    filter(IS_FIRST_QUALIFYING_EXIT == 1L) %>%
    transmute(
      outcome = outcome_tag,
      country = .data[[COUNTRY]],
      year = .data[[YEAR]]
    )
  
  haz0 <- df1_est %>%
    filter(at_risk == 1L | EXIT_SUSTAINED == 1L) %>%
    group_by(.data[[COUNTRY]]) %>%
    filter(any(at_risk == 1L) | any(EXIT_SUSTAINED == 1L)) %>%
    ungroup()
  
  haz1 <- haz0 %>%
    arrange(.data[[COUNTRY]], .data[[YEAR]]) %>%
    group_by(.data[[COUNTRY]]) %>%
    mutate(
      lag_at_risk = dplyr::lag(at_risk, default = 0L),
      spell_start = at_risk == 1L & lag_at_risk == 0L,
      spell_id_tmp = cumsum(spell_start),
      spell_id = if_else(
        at_risk == 1L, spell_id_tmp, dplyr::lag(spell_id_tmp)
      ),
      spell_id = replace_na(spell_id, 0L)
    ) %>%
    group_by(.data[[COUNTRY]], spell_id) %>%
    mutate(t_at_risk = row_number() - 1L) %>%
    ungroup() %>%
    select(-lag_at_risk, -spell_start, -spell_id_tmp)
  
  stopifnot(all(haz1$EXIT_SUSTAINED %in% c(0L, 1L, NA)))
  event_countries <- haz1[[COUNTRY]][haz1$EXIT_SUSTAINED == 1L]
  stopifnot(!anyDuplicated(event_countries))
  attr(haz1, "event_audit") <- event_audit
  haz1
}

# Estimation helpers -----------------------------------------------------------
fit_brglm_stable <- function(formula, data) {
  design_matrix <- stats::model.matrix(formula, data = data)
  stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial("logit"),
    method = brglm2::brglmFit,
    start = rep(0, ncol(design_matrix)),
    control = brglm2::brglmControl(
      type = "AS_mixed",
      maxit = BRGLM_MAXIT,
      epsilon = BRGLM_EPSILON,
      slowit = BRGLM_SLOWIT
    )
  )
}

cluster_from_glm <- function(model, data, cluster_col) {
  model_rows <- rownames(stats::model.frame(model))
  row_index <- match(model_rows, rownames(data))
  if (anyNA(row_index)) row_index <- suppressWarnings(as.integer(model_rows))
  if (anyNA(row_index) || any(!row_index %in% seq_len(nrow(data)))) {
    stop("Could not align model-frame rows to clustering data.")
  }
  data[[cluster_col]][row_index]
}

capture_expression <- function(expr) {
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
    return(list(value = NULL, error = conditionMessage(value),
                warnings = unique(warnings)))
  }
  list(value = value, error = NA_character_, warnings = unique(warnings))
}

empty_result <- function(outcome, outcome_label, democracy_var, adjusted,
                         error_message, n_obs = NA_integer_,
                         n_countries = NA_integer_, n_events = NA_integer_) {
  tibble(
    outcome = outcome,
    outcome_label = outcome_label,
    specification = if_else(adjusted, "GDP-adjusted", "Bivariate"),
    adjusted_for_gdp = adjusted,
    democracy_var = democracy_var,
    democracy_label = unname(DEM_LABELS[[democracy_var]]),
    term = paste0(democracy_var, "_L", LAG_YEARS),
    beta = NA_real_,
    std_error = NA_real_,
    odds_ratio = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_value = NA_real_,
    observations = n_obs,
    countries = n_countries,
    events = n_events,
    spline_df = NA_integer_,
    converged = FALSE,
    usable = FALSE,
    warnings = NA_character_,
    error = error_message
  )
}

fit_democracy_model <- function(hazard_data, outcome, outcome_label,
                                democracy_var, adjusted_for_gdp) {
  democracy_lag <- paste0(democracy_var, "_L", LAG_YEARS)
  gdp_lag <- paste0(GDP_VAR, "_L", LAG_YEARS)
  
  required_for_sample <- democracy_lag
  if (adjusted_for_gdp || PAIRED_SAMPLES_WITH_GDP) {
    required_for_sample <- c(required_for_sample, gdp_lag)
  }
  
  model_data <- hazard_data %>%
    filter(if_all(all_of(required_for_sample), ~ !is.na(.x))) %>%
    as.data.frame()
  
  n_obs <- nrow(model_data)
  n_countries <- dplyr::n_distinct(model_data[[COUNTRY]])
  n_events <- sum(model_data$EXIT_SUSTAINED == 1L, na.rm = TRUE)
  
  if (!n_obs || n_events == 0L || n_events == n_obs) {
    return(list(
      result = empty_result(
        outcome, outcome_label, democracy_var, adjusted_for_gdp,
        "Empty or constant-outcome estimation sample.",
        n_obs, n_countries, n_events
      ),
      model = NULL
    ))
  }
  if (length(unique(model_data[[democracy_lag]])) < 2L) {
    return(list(
      result = empty_result(
        outcome, outcome_label, democracy_var, adjusted_for_gdp,
        "Democracy predictor has no variation in the estimation sample.",
        n_obs, n_countries, n_events
      ),
      model = NULL
    ))
  }
  
  spline_df <- choose_spline_df(model_data$t_at_risk, max_df = 3L)
  substantive_terms <- democracy_lag
  if (adjusted_for_gdp) substantive_terms <- c(substantive_terms, gdp_lag)
  formula <- stats::as.formula(paste0(
    "EXIT_SUSTAINED ~ ",
    paste(substantive_terms, collapse = " + "),
    " + splines::ns(t_at_risk, ", spline_df, ")"
  ))
  
  fit_capture <- capture_expression(fit_brglm_stable(formula, model_data))
  model <- fit_capture$value
  if (is.null(model)) {
    result <- empty_result(
      outcome, outcome_label, democracy_var, adjusted_for_gdp,
      fit_capture$error, n_obs, n_countries, n_events
    )
    result$warnings <- paste(fit_capture$warnings, collapse = " | ")
    return(list(result = result, model = NULL))
  }
  
  inference_capture <- capture_expression({
    cluster <- cluster_from_glm(model, model_data, COUNTRY)
    variance <- sandwich::vcovCL(model, cluster = cluster)
    coefficients <- stats::coef(model)
    standard_errors <- sqrt(diag(variance))
    c(beta = unname(coefficients[[democracy_lag]]),
      std_error = unname(standard_errors[[democracy_lag]]))
  })
  
  if (is.null(inference_capture$value)) {
    result <- empty_result(
      outcome, outcome_label, democracy_var, adjusted_for_gdp,
      inference_capture$error, n_obs, n_countries, n_events
    )
    result$converged <- isTRUE(model$converged)
    result$warnings <- paste(
      unique(c(fit_capture$warnings, inference_capture$warnings)),
      collapse = " | "
    )
    return(list(result = result, model = model))
  }
  
  beta <- unname(inference_capture$value[["beta"]])
  std_error <- unname(inference_capture$value[["std_error"]])
  z_critical <- stats::qnorm(1 - (1 - CI_LEVEL) / 2)
  z_statistic <- beta / std_error
  converged <- isTRUE(model$converged)
  finite_inference <- all(is.finite(c(beta, std_error))) && std_error > 0
  warning_text <- paste(
    unique(c(fit_capture$warnings, inference_capture$warnings)),
    collapse = " | "
  )
  bad_warning <- grepl(
    "algorithm did not converge|failed to converge|non-positive-definite|false convergence|singular|NaN",
    warning_text,
    ignore.case = TRUE
  )
  usable <- converged && finite_inference && !bad_warning && abs(beta) <= 1e6
  
  result <- tibble(
    outcome = outcome,
    outcome_label = outcome_label,
    specification = if_else(adjusted_for_gdp, "GDP-adjusted", "Bivariate"),
    adjusted_for_gdp = adjusted_for_gdp,
    democracy_var = democracy_var,
    democracy_label = unname(DEM_LABELS[[democracy_var]]),
    term = democracy_lag,
    beta = beta,
    std_error = std_error,
    odds_ratio = exp(beta),
    conf_low = exp(beta - z_critical * std_error),
    conf_high = exp(beta + z_critical * std_error),
    p_value = 2 * stats::pnorm(abs(z_statistic), lower.tail = FALSE),
    observations = n_obs,
    countries = n_countries,
    events = n_events,
    spline_df = spline_df,
    converged = converged,
    usable = usable,
    warnings = warning_text,
    error = if_else(usable, NA_character_, "Fit failed numerical screening.")
  )
  
  list(result = result, model = model)
}

# Plotting ---------------------------------------------------------------------
make_faceted_coefplot <- function(results) {
  failed_models <- results %>% filter(!usable)
  if (nrow(failed_models)) {
    warning(
      "The faceted plot omits failed models: ",
      paste(
        paste(failed_models$outcome,
              failed_models$specification,
              failed_models$democracy_var,
              sep = " / "),
        collapse = "; "
      )
    )
  }
  
  panel_levels <- c(
    "Index with rule of law: Bivariate",
    "Index with rule of law: GDP-adjusted",
    "Index without rule of law: Bivariate",
    "Index without rule of law: GDP-adjusted"
  )
  
  plot_data <- results %>%
    filter(usable) %>%
    mutate(
      democracy_label = factor(
        democracy_label,
        levels = rev(unname(DEM_LABELS))
      ),
      panel = factor(
        paste(outcome_label, specification, sep = ": "),
        levels = panel_levels
      )
    )
  
  paired_note <- if (PAIRED_SAMPLES_WITH_GDP) {
    " Bivariate and GDP-adjusted models use paired samples within each democracy measure."
  } else {
    " Bivariate models use all available observations for each democracy measure."
  }
  
  ggplot(plot_data, aes(x = odds_ratio, y = democracy_label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55",
               linewidth = 0.5) +
    geom_errorbar(
      aes(xmin = conf_low, xmax = conf_high),
      width = 0.14,
      linewidth = 0.7,
      orientation = "y",
      colour = "#B40000"
    ) +
    geom_point(size = 2.8, colour = "#B40000") +
    scale_x_log10() +
    facet_wrap(~ panel, ncol = 2, scales = "fixed") +
    labs(
      title = "Democracy and sustained exit from fragility (K = 5)",
      subtitle = paste0(
        "Separate bias-reduced hazard models for each democracy measure; ",
        "GDP-adjusted specifications control for log GDP per capita"
      ),
      x = "Odds ratio (log scale; 95% country-clustered CI)",
      y = NULL,
      caption = paste0(
        "",
        paired_note
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.background = element_rect(fill = "grey92", colour = "black"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = 8.5),
      axis.text.y = element_text(colour = "black")
    )
}

save_plot_pair <- function(plot_object, stem) {
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(stem, ".png")),
    plot = plot_object, width = 10, height = 7.5, units = "in",
    dpi = 300, bg = "white"
  )
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(stem, ".pdf")),
    plot = plot_object, width = 10, height = 7.5, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
}

# Load and prepare data --------------------------------------------------------
if (!exists("standardized_data_final", inherits = TRUE)) {
  data_path <- resolve_existing_path(DATA_CANDIDATES)
  message("Reading: ", data_path)
  standardized_data_final <- readRDS(data_path)
}

df <- standardized_data_final %>%
  mutate(!!YEAR := suppressWarnings(as.integer(.data[[YEAR]])))

alias_map <- list(
  LOG_GDP_PER_CAPITA = c(
    "LOG_GDP_PER_CAPITA", "LOG_GDP_PC", "LOG_GDP_PER_CAP"
  ),
  INSTITUTIONAL_DEMOCRACY = c(
    "INSTITUTIONAL_DEMOCRACY",
    "INSITUTIONAL_DEMOCRACY",
    "INSTITUTIONAL_DEMOCRACY_SCORE",
    "INSTITUTIONAL_DEMOCRACY_SOCRE"
  )
)
df <- apply_alias_renames(df, alias_map)

required_columns <- c(
  COUNTRY, YEAR, FULL_INDEX_VAR, NORL_INDEX_VAR, DEM_VARS, GDP_VAR
)
missing_columns <- setdiff(required_columns, names(df))
if (length(missing_columns)) {
  democracy_like <- grep("DEMOCR", names(df), value = TRUE, ignore.case = TRUE)
  stop(
    "Required columns are missing: ", paste(missing_columns, collapse = ", "),
    "\nDemocracy-like columns available in the data: ",
    paste(democracy_like, collapse = ", ")
  )
}

# Reconstruct both validated outcomes before completing the panel.
df <- add_koami_status(df, FULL_INDEX_VAR, FULL_STATUS)
df <- add_koami_status(df, NORL_INDEX_VAR, NORL_STATUS)

# Preserve 0/1 interpretation for binary indicators; express both continuous
# predictors in standard-deviation units, matching the pipeline's treatment of
# continuous regressors.
for (variable in DEM_VARS[DEM_VARS != "INSTITUTIONAL_DEMOCRACY"]) {
  df[[variable]] <- coerce_binary01(df[[variable]], variable)
}
df[["INSTITUTIONAL_DEMOCRACY"]] <- standardize_numeric(
  df[["INSTITUTIONAL_DEMOCRACY"]], "INSTITUTIONAL_DEMOCRACY"
)
df[[GDP_VAR]] <- standardize_numeric(df[[GDP_VAR]], GDP_VAR)

df_full <- complete_country_year(df)
all_predictors <- c(DEM_VARS, GDP_VAR)

# Build both risk sets and estimate the 20 models ------------------------------
HAZARD_DATA <- list()
MODELS <- list()
RESULTS <- tibble()

for (outcome_index in seq_len(nrow(OUTCOME_SPECS))) {
  outcome <- OUTCOME_SPECS$outcome[[outcome_index]]
  outcome_label <- OUTCOME_SPECS$outcome_label[[outcome_index]]
  status_var <- OUTCOME_SPECS$status_var[[outcome_index]]
  expected_events <- OUTCOME_SPECS$expected_events[[outcome_index]]
  
  hazard_data <- build_hazard(
    df_full = df_full,
    predictors = all_predictors,
    status_var = status_var,
    outcome_tag = outcome
  )
  HAZARD_DATA[[outcome]] <- hazard_data
  
  raw_events <- sum(hazard_data$EXIT_SUSTAINED == 1L, na.rm = TRUE)
  message(
    outcome, ": ", nrow(hazard_data), " risk-set/event rows; ",
    raw_events, " raw first-exit events."
  )
  if (!identical(as.integer(raw_events), as.integer(expected_events))) {
    stop(
      "Koami event-universe mismatch for ", outcome, ": expected ",
      expected_events, " K=5 first exits but reconstructed ", raw_events, "."
    )
  }
  
  for (democracy_var in DEM_VARS) {
    for (adjusted in c(FALSE, TRUE)) {
      fitted <- fit_democracy_model(
        hazard_data = hazard_data,
        outcome = outcome,
        outcome_label = outcome_label,
        democracy_var = democracy_var,
        adjusted_for_gdp = adjusted
      )
      RESULTS <- bind_rows(RESULTS, fitted$result)
      model_name <- paste(
        outcome,
        democracy_var,
        if (adjusted) "gdp_adjusted" else "bivariate",
        sep = "__"
      )
      MODELS[[model_name]] <- fitted$model
    }
  }
}

# Save estimates, diagnostics, model objects, and faceted coefficient plot -----
readr_available <- requireNamespace("readr", quietly = TRUE)
results_path <- file.path(OUTPUT_DIR, "democracy_alternative_measures_K5_estimates.csv")
if (readr_available) {
  readr::write_csv(RESULTS, results_path, na = "")
} else {
  utils::write.csv(RESULTS, results_path, row.names = FALSE, na = "")
}

saveRDS(
  list(models = MODELS, hazard_data = HAZARD_DATA, settings = list(
    K = K_SUSTAIN,
    lag_years = LAG_YEARS,
    fragile_history_years = FRAG_HISTORY_YEARS,
    paired_samples_with_gdp = PAIRED_SAMPLES_WITH_GDP
  )),
  file.path(OUTPUT_DIR, "democracy_alternative_measures_K5_models.rds")
)

COMBINED_PLOT <- make_faceted_coefplot(RESULTS)
save_plot_pair(
  COMBINED_PLOT,
  "coefplot_democracy_alternative_measures_K5_faceted"
)

print(
  RESULTS %>%
    select(outcome, specification, democracy_var, odds_ratio, conf_low,
           conf_high, p_value, observations, countries, events,
           converged, usable, error)
)

print(COMBINED_PLOT)

message("Finished. Outputs written to: ", normalizePath(OUTPUT_DIR, winslash = "/"))
