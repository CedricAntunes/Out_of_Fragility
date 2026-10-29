# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: August 2026 (WGI estimates added October 2026) -------------------------
#
# Purpose: Pearson correlation for state capacity and fragility: V-Dem full and
# no-rule-of-law indices, and WGI full and no-rule-of-law indices. Figures are
# pooled, year-adjusted, and country/year-adjusted Pearson r. Each index family
# (V-Dem, WGI) uses its own complete-case sample, shared by its full and
# no-rule-of-law indices and by all specifications. The V-Dem sample is
# therefore unchanged by the addition of WGI, which only starts in 1996.
# 95% percentile intervals: pairs bootstrap of entire country histories.
# Pooled Spearman rho is exported as a sensitivity check, on the same sample.

# Settings ---------------------------------------------------------------------
YEAR_MIN <- 1971L
YEAR_MAX <- 2022L
BOOT_REPS <- 2000L
BOOT_SEED <- 20260909L
CONF_LEVEL <- 0.95
OUTPUT_DIR <- "state_capacity_fragility_vdem_outputs"

# Explicit orientation: -1 assumes the supplied raw index is higher = LESS fragile
FRAGILITY_MULTIPLIER <- c(vdem_full = -1, vdem_norl = -1,
                          wgi_full = -1, wgi_norl = -1)
CAPACITY_MULTIPLIER <- 1  # STATE_CAPACITY is higher = greater capacity.

# Index families. Each family is estimated on its own shared complete-case sample.
INDEX_FAMILIES <- list(
  vdem = list(label = "V-Dem", outcomes = c("vdem_full", "vdem_norl")),
  wgi  = list(label = "WGI",   outcomes = c("wgi_full", "wgi_norl"))
)

# Packages ---------------------------------------------------------------------
suppressPackageStartupMessages({
  library(fixest)
  library(ggplot2)
}
)

# Data dependency --------------------------------------------------------------
# `standardized_data_final` must already be in the environment (country-year
# panel with ISO_CODE_3, YEAR, STATE_CAPACITY, and the V-Dem and WGI fragility
# indices). Load it here if running standalone, e.g.:
# standardized_data_final <- readRDS("standardized_data_final.RDS")
stopifnot(exists("standardized_data_final"))

stopifnot(YEAR_MIN <= YEAR_MAX, BOOT_REPS >= 100L,
          BOOT_REPS == as.integer(BOOT_REPS),
          CONF_LEVEL > 0, CONF_LEVEL < 1,
          identical(names(FRAGILITY_MULTIPLIER),
                    unlist(lapply(INDEX_FAMILIES, `[[`, "outcomes"), use.names = FALSE)),
          all(FRAGILITY_MULTIPLIER %in% c(-1, 1)),
          CAPACITY_MULTIPLIER %in% c(-1, 1))

source_vars <- c(
  state_capacity = "STATE_CAPACITY",
  vdem_full = "NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y",
  vdem_norl = "NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED",
  wgi_full = "WGI_LOADING_FACTOR_1_NORMALIZED",
  wgi_norl = "WGI_NORL_LOADING_FACTOR_1_NORMALIZED"
)

src <- as.data.frame(standardized_data_final)
for (v in unname(source_vars)) {
  if (!v %in% names(src)) stop(v, " is missing from standardized_data_final.")
  if (!is.numeric(src[[v]])) stop(v, " must be numeric; inspect its import first.")
}

year_num <- suppressWarnings(as.numeric(as.character(src$YEAR)))
if (any(!is.na(src$YEAR) & (!is.finite(year_num) | year_num != trunc(year_num)))) {
  stop("YEAR contains invalid or non-integer values.")
}

d_all <- data.frame(
  source_row = seq_len(nrow(src)),
  iso3 = trimws(as.character(src$ISO_CODE_3)),
  year = as.integer(year_num),
  state_capacity = src[[source_vars[["state_capacity"]]]] * CAPACITY_MULTIPLIER
)
for (v in names(FRAGILITY_MULTIPLIER)) {
  d_all[[v]] <- src[[source_vars[[v]]]] * FRAGILITY_MULTIPLIER[[v]]
}

valid_keys <- !is.na(d_all$iso3) & nzchar(d_all$iso3) & !is.na(d_all$year)
in_period <- valid_keys & !is.na(d_all$year) & d_all$year >= YEAR_MIN & d_all$year <= YEAR_MAX
in_period[is.na(in_period)] <- FALSE
period_data <- d_all[in_period, , drop = FALSE]

dup <- duplicated(period_data[c("iso3", "year")]) |
  duplicated(period_data[c("iso3", "year")], fromLast = TRUE)

# Optional sign diagnostics, run on each family's analysis sample.
# Assumes *_FRAGILE_IDEAL is coded 1 = fragile, 0 = otherwise.
binary_vars <- c(vdem_full = "VDEM_FRAGILE_IDEAL",
                 vdem_norl = "VDEM_NORL_FRAGILE_IDEAL",
                 wgi_full = "WGI_FRAGILE_IDEAL",
                 wgi_norl = "WGI_NORL_FRAGILE_IDEAL")

# ------------------------------------------------------------------------------
# Estimation helpers -----------------------------------------------------------
# ------------------------------------------------------------------------------
adjustments <- c("pooled", "year", "country_year")

make_specs <- function(outcomes) {
  specs <- expand.grid(outcome = outcomes, adjustment = adjustments,
                       stringsAsFactors = FALSE)
  specs$method <- "pearson"
  specs <- rbind(specs, data.frame(outcome = outcomes, adjustment = "pooled",
                                   method = "spearman"))
  specs$id <- paste(specs$method, specs$adjustment, specs$outcome, sep = "__")
  specs
}

safe_cor <- function(x, y, method = "pearson") {
  # Residuals can be numerically zero when a resample has no identifying variation.
  if (length(x) < 3L || any(!is.finite(x)) || any(!is.finite(y)) ||
      stats::sd(x) < 1e-10 || stats::sd(y) < 1e-10) return(NA_real_)
  unname(stats::cor(x, y, method = method))
}

estimate_all <- function(data, country_id, measure_cols, specs) {
  X <- as.matrix(data[measure_cols])
  # Joint demeaning preserves row alignment and uses the same sample for all columns.
  # In an unbalanced panel, country/year adjustment is NOT simple double subtraction.
  year_resid <- fixest::demean(
    X, f = list(data$year), as.matrix = TRUE, nthreads = 1,
    tol = 1e-10, iter = 10000, notes = FALSE
  )
  country_year_resid <- fixest::demean(
    X, f = list(country_id, data$year), as.matrix = TRUE, nthreads = 1,
    tol = 1e-10, iter = 10000, notes = FALSE
  )
  # Demean is matrix-based, so singleton rows remain aligned (their within
  # residuals can be zero). No independent-observation cor.test() is used.
  matrices <- list(pooled = X, year = year_resid, country_year = country_year_resid)
  ans <- vapply(seq_len(nrow(specs)), function(j) {
    M <- matrices[[specs$adjustment[j]]]
    k <- match(specs$outcome[j], measure_cols)
    safe_cor(M[, 1L], M[, k], method = specs$method[j])
  }, numeric(1))
  stats::setNames(ans, specs$id)
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
file_out <- function(name) file.path(OUTPUT_DIR, name)
alpha <- 1 - CONF_LEVEL
FAMILY_COLOURS <- c(
  "V-Dem" = "#8B1A1A",  # dark red, as in the original V-Dem figure
  "WGI"   = "#1F3A5F"   # dark navy
)

# ------------------------------------------------------------------------------
# Run one index family on its own shared complete-case sample ------------------
# ------------------------------------------------------------------------------
run_family <- function(family) {
  fam <- INDEX_FAMILIES[[family]]
  outcomes <- fam$outcomes
  measure_cols <- c("state_capacity", outcomes)
  specs <- make_specs(outcomes)
  
  complete <- Reduce(`&`, lapply(period_data[measure_cols], is.finite))
  d <- period_data[complete, c("source_row", "iso3", "year", measure_cols), drop = FALSE]
  d <- d[order(d$iso3, d$year), , drop = FALSE]
  rownames(d) <- NULL
  G <- length(unique(d$iso3))
  if (G < 3L || length(unique(d$year)) < 2L || nrow(d) < 10L) {
    stop(fam$label, ": too few countries, years, or complete observations for this analysis.")
  }
  if (G < 30L) warning(fam$label, ": fewer than 30 countries; cluster-bootstrap intervals may be unstable.")
  
  orientation_audit <- do.call(rbind, lapply(outcomes, function(v) {
    binary_name <- binary_vars[[v]]
    r <- NA_real_
    n <- 0L
    status <- "Binary unavailable; verify orientation from index construction"
    if (binary_name %in% names(src)) {
      b <- suppressWarnings(as.numeric(as.character(src[[binary_name]][d$source_row])))
      ok <- is.finite(b) & b %in% c(0, 1)
      n <- sum(ok)
      if (n >= 10L && length(unique(b[ok])) == 2L && stats::sd(d[[v]][ok]) > 0) {
        r <- stats::cor(d[[v]][ok], b[ok])
        status <- "Checked against 1 = fragile binary"
        if (r < 0) stop("Orientation mismatch for ", v,
                        ": oriented index correlates negatively with ", binary_name,
                        ". Verify index and binary coding, then set FRAGILITY_MULTIPLIER.")
      } else status <- "Binary check inconclusive; verify documented orientation"
    }
    data.frame(outcome = v, multiplier = FRAGILITY_MULTIPLIER[[v]],
               diagnostic_n = n, correlation_with_fragile_binary = r, status = status)
  }))
  
  point_estimates <- estimate_all(d, d$iso3, measure_cols, specs)
  if (any(!is.finite(point_estimates))) {
    stop("Undefined correlation in the original sample: ",
         paste(names(point_estimates)[!is.finite(point_estimates)], collapse = ", "),
         ". Inspect residual variation.")
  }
  
  # Country pairs bootstrap ----------------------------------------------------
  # Draw G countries with replacement. Each selected history is copied intact.
  # Repeated draws receive distinct country FE identifiers; original year labels
  # remain unchanged. Re-estimate year and country/year effects in EVERY draw.
  # The same draw is used for both indices of a family and all specifications.
  # The seed is reset for each family, so V-Dem draws are identical to the
  # V-Dem-only version of this script.
  rows_by_country <- split(seq_len(nrow(d)), d$iso3)
  set.seed(BOOT_SEED)
  boot_estimates <- matrix(NA_real_, nrow = BOOT_REPS, ncol = nrow(specs),
                           dimnames = list(NULL, specs$id))
  message(sprintf("%s shared sample: %s country-years; %s countries; observed years %s-%s.",
                  fam$label, nrow(d), G, min(d$year), max(d$year)))
  for (b in seq_len(BOOT_REPS)) {
    sampled <- sample.int(G, size = G, replace = TRUE)
    row_blocks <- rows_by_country[sampled]
    row_idx <- unlist(row_blocks, use.names = FALSE)
    draw_country_id <- rep(seq_len(G), lengths(row_blocks))
    boot_estimates[b, ] <- estimate_all(d[row_idx, , drop = FALSE], draw_country_id,
                                        measure_cols, specs)
    if (b %% 100L == 0L || b == BOOT_REPS) {
      message(sprintf("%s bootstrap: %s / %s", fam$label, b, BOOT_REPS))
    }
  }
  
  # Results and coverage -------------------------------------------------------
  settings <- list(index_family = fam$label, year_min = YEAR_MIN, year_max = YEAR_MAX,
                   boot_reps = BOOT_REPS, seed = BOOT_SEED, confidence = CONF_LEVEL,
                   fragility_multiplier = FRAGILITY_MULTIPLIER[outcomes],
                   capacity_multiplier = CAPACITY_MULTIPLIER)
  # Preserve draws even if a degenerate sample prevents reporting valid intervals.
  saveRDS(list(specs = specs, estimates = boot_estimates, settings = settings),
          file_out(sprintf("%s_state_capacity_bootstrap_draws.rds", family)))
  valid_reps <- colSums(is.finite(boot_estimates))
  if (any(valid_reps < ceiling(0.95 * BOOT_REPS))) {
    stop(fam$label, ": more than 5% undefined bootstrap estimates in at least one ",
         "specification. Draws saved for diagnosis; inspect residual variation ",
         "before reporting CIs.")
  }
  if (any(valid_reps < BOOT_REPS)) {
    warning(fam$label, ": some bootstrap correlations were undefined. ",
            "Valid replicate counts are reported.")
  }
  ci <- t(vapply(seq_len(ncol(boot_estimates)), function(j) {
    stats::quantile(boot_estimates[, j], probs = c(alpha / 2, 1 - alpha / 2),
                    na.rm = TRUE, names = FALSE)
  }, numeric(2)))
  cor_results <- data.frame(
    index_family = fam$label,
    specs, estimate = unname(point_estimates), conf_low = ci[, 1L], conf_high = ci[, 2L],
    n_country_years = nrow(d), n_countries = G,
    year_min = min(d$year), year_max = max(d$year),
    bootstrap_requested = BOOT_REPS, bootstrap_valid = unname(valid_reps),
    confidence_level = CONF_LEVEL, ci_method = "Country-pairs bootstrap percentile"
  )
  country_coverage <- do.call(rbind, lapply(split(d, d$iso3), function(z) {
    data.frame(iso3 = z$iso3[1L], n_years = nrow(z),
               first_year = min(z$year), last_year = max(z$year))
  }))
  sample_audit <- data.frame(
    index_family = fam$label,
    stage = c("Input rows", "Invalid or missing keys", "Valid keys outside period",
              "Rows in requested period", "Excluded: missing/nonfinite measures",
              "Shared complete-case country-years", "Countries in shared sample"),
    n = c(nrow(src), sum(!valid_keys), sum(valid_keys & !in_period),
          nrow(period_data), sum(!complete), nrow(d), G)
  )
  
  print(cor_results, row.names = FALSE)
  
  capture.output(settings, utils::sessionInfo(),
                 file = file_out(sprintf("%s_state_capacity_run_info.txt", family)))
  
  list(sample = d, n_countries = G, orientation_audit = orientation_audit,
       cor_results = cor_results, country_coverage = country_coverage,
       sample_audit = sample_audit, boot_estimates = boot_estimates)
}

# ------------------------------------------------------------------------------
# Run both families ------------------------------------------------------------
# ------------------------------------------------------------------------------
results_by_family <- lapply(stats::setNames(names(INDEX_FAMILIES), names(INDEX_FAMILIES)),
                            run_family)

# Combined results (V-Dem and WGI rows; samples differ across families).
cor_results <- do.call(rbind, lapply(results_by_family, `[[`, "cor_results"))
rownames(cor_results) <- NULL
sample_audit <- do.call(rbind, lapply(results_by_family, `[[`, "sample_audit"))
rownames(sample_audit) <- NULL
orientation_audit <- do.call(rbind, lapply(results_by_family, `[[`, "orientation_audit"))
rownames(orientation_audit) <- NULL

print(cor_results[, c("index_family", "outcome", "adjustment", "method",
                      "estimate", "conf_low", "conf_high",
                      "n_country_years", "n_countries", "year_min", "year_max")],
      row.names = FALSE, digits = 3)

# ------------------------------------------------------------------------------
# Plot: V-Dem and WGI in one figure --------------------------------------------
# ------------------------------------------------------------------------------
# Samples differ ACROSS sources (WGI starts in 1996) but are shared WITHIN each
# source. The legend and caption state each source's sample explicitly.
family_labels <- vapply(INDEX_FAMILIES, `[[`, character(1), "label")
row_levels <- unlist(lapply(INDEX_FAMILIES, function(f) {
  paste0(f$label, ": ", c("Full index", "Excluding rule of law"))
}), use.names = FALSE)

sample_info <- unique(cor_results[c("index_family", "n_country_years", "n_countries",
                                    "year_min", "year_max")])
sample_info <- sample_info[match(family_labels, sample_info$index_family), , drop = FALSE]
sample_text <- sprintf("%s: %s country-years, %s countries, %s-%s",
                       sample_info$index_family,
                       format(sample_info$n_country_years, big.mark = ",", trim = TRUE),
                       sample_info$n_countries, sample_info$year_min, sample_info$year_max)
legend_labels <- stats::setNames(
  sprintf("%s (%s-%s)", sample_info$index_family, sample_info$year_min, sample_info$year_max),
  sample_info$index_family
)

plot_data <- cor_results[cor_results$method == "pearson", , drop = FALSE]
plot_data$index_type <- ifelse(grepl("_norl$", plot_data$outcome),
                               "Excluding rule of law", "Full index")
plot_data$index <- factor(paste0(plot_data$index_family, ": ", plot_data$index_type),
                          levels = rev(row_levels))
plot_data$index_family <- factor(plot_data$index_family, levels = family_labels)
plot_data$panel <- factor(
  plot_data$adjustment, levels = adjustments,
  labels = c("Pooled association", "Year-adjusted association",
             "Within-country association\n(country and year adjusted)")
)
plot_data$label <- sprintf("%.2f", plot_data$estimate)

caption <- paste0(
  "Samples differ by source; within each source, both indices and all panels share one sample.\n",
  paste(sample_text, collapse = "; "), ".\n",
  "Higher fragility scores = more fragile; higher capacity scores = greater capacity.\n",
  sprintf("Whiskers: %.0f%% country-cluster bootstrap percentile intervals (%s draws per source).",
          100 * CONF_LEVEL, format(BOOT_REPS, big.mark = ",", trim = TRUE))
)

p_state_capacity_cor <- ggplot(
  plot_data, aes(x = estimate, y = index, colour = index_family)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_segment(aes(x = conf_low, xend = conf_high, yend = index), linewidth = 0.9) +
  geom_point(size = 2.6) +
  geom_text(aes(label = label), nudge_y = 0.28,
            size = 3.3, colour = "grey20") +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = FAMILY_COLOURS, labels = legend_labels,
                      name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5),
                     expand = expansion(mult = 0.04)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.75))) +
  labs(title = sprintf("State capacity and Fragility Status: V-Dem and WGI indices (%s-%s)",
                       min(sample_info$year_min), max(sample_info$year_max)),
       x = "Pearson correlation", y = NULL, caption = caption) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey75"),
    axis.text = element_text(colour = "black"),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(12, 12, 12, 12)
  )
print(p_state_capacity_cor)

ggsave(file_out("vdem_wgi_state_capacity_correlations.png"), p_state_capacity_cor,
       width = 12, height = 6, dpi = 300, bg = "white")

ggsave(file_out("vdem_wgi_state_capacity_correlations.pdf"), p_state_capacity_cor,
       width = 12, height = 6, bg = "white")
