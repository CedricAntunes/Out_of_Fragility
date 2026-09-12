# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: April, 2026  (revised) -------------------------------------------------
#
# Purpose ----------------------------------------------------------------------
# Descriptive validity check: show that regime type (democracy / autocracy) and 
# fragility CROSS-CUT. Concretely, that there exist democratic-yet-fragile and 
# autocratic-yet-non-fragile country-years.
#
# Design note ------------------------------------------------------------------
# The paper's MAIN fragility measure is a PCA composite of four V-Dem components
# with percentile thresholds. This script uses a deliberately DEMANDING
# alternative: a conjunctive rule requiring all four components (strict, 4-of-4)
# or at least three (relaxed, 3-of-4) to be jointly weak / strong within year.
# Framed this way it is a hard test of the concept, not a competing measure:
# if regime type and fragility cross-cut even under the strictest conjunctive
# definition, the point is robust. Report it as such in the text.
# ------------------------------------------------------------------------------

# Packages  --------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(vdemdata)
library(knitr)
library(kableExtra)
library(ggplot2)

# Data dependency --------------------------------------------------------------
# `standardized_data_final` must already be in the environment (country-year
# panel with COUNTRY_NAME, ISO_CODE_3, YEAR, BOIX_DEMOCRACY). Load it here if
# running standalone, e.g.:
# standardized_data_final <- readRDS("final_new_clean_output_data.RDS")
stopifnot(exists("standardized_data_final"))

# V-Dem indicators -------------------------------------------------------------
vdem_df <- vdem |>
  transmute(
    ISO_CODE_3 = toupper(country_text_id),
    YEAR       = as.integer(year),
    v2caviol,      # political violence
    v2clrspct,     # rigorous & impartial public administration
    v2xnp_regcorr, # regime corruption
    v2x_rule       # rule of law
  ) |>
  filter(YEAR >= 1970, YEAR <= 2022)

# Join -------------------------------------------------------------------------
df_descriptive <- standardized_data_final |>
  mutate(
    ISO_CODE_3 = toupper(ISO_CODE_3),
    YEAR       = as.integer(YEAR)
  ) |>
  left_join(vdem_df, by = c("ISO_CODE_3", "YEAR"))

# Data preparation -------------------------------------------------------------
cases_df <- df_descriptive |>
  transmute(
    country        = COUNTRY_NAME,
    iso3           = ISO_CODE_3,
    year           = YEAR,
    boix_democracy = BOIX_DEMOCRACY,
    v2caviol, v2clrspct, v2xnp_regcorr, v2x_rule
  ) |>
  filter(
    !is.na(country), !is.na(year), !is.na(boix_democracy),
    !is.na(v2caviol), !is.na(v2clrspct),
    !is.na(v2xnp_regcorr), !is.na(v2x_rule)
  )

# ------------------------------------------------------------------------------
# 1. Orient all four components so HIGHER = less fragile ("better") ------------
# ------------------------------------------------------------------------------
orient_higher_is_better <- c(
  v2x_rule      = TRUE,   # Rule of Law: higher = stronger
  v2clrspct     = TRUE,   # Impartial public administration: higher = better
  v2xnp_regcorr = FALSE,  # Regime corruption: higher = more corrupt (negate)
  v2caviol      = NA      # Political violence: higher = more violent (negate)
)

# Deterministic orientation, with a data-based warning only when undeclared
orient_var <- function(x, anchor, declared, nm = "component") {
  if (isTRUE(declared))  return(x)
  if (isFALSE(declared)) return(-x)
  s <- sign(cor(x, anchor, use = "complete.obs"))
  if (is.na(s) || s == 0) s <- 1
  message(sprintf(
    "Orientation for %s was NOT declared; inferred from correlation with rule of law (sign = %s). Verify against the codebook.",
    nm, s))
  if (s < 0) -x else x
}

cases_df <- cases_df |>
  mutate(
    rule_good       = orient_var(v2x_rule,      v2x_rule, orient_higher_is_better[["v2x_rule"]],      "v2x_rule"),
    admin_good      = orient_var(v2clrspct,     v2x_rule, orient_higher_is_better[["v2clrspct"]],     "v2clrspct"),
    corruption_good = orient_var(v2xnp_regcorr, v2x_rule, orient_higher_is_better[["v2xnp_regcorr"]], "v2xnp_regcorr"),
    violence_good   = orient_var(v2caviol,      v2x_rule, orient_higher_is_better[["v2caviol"]],      "v2caviol")
  )

# Diagnostic: after orientation, all three should correlate POSITIVELY with rule.
cases_df |>
  summarise(
    cor_vio_rule   = cor(violence_good,   rule_good, use = "complete.obs"),
    cor_corr_rule  = cor(corruption_good, rule_good, use = "complete.obs"),
    cor_admin_rule = cor(admin_good,      rule_good, use = "complete.obs")
  ) |>
  print()

# ------------------------------------------------------------------------------
# 2. Within-year percentile ranks (0 = weakest that year, 1 = strongest) -------
# ------------------------------------------------------------------------------
cases_df <- cases_df |>
  group_by(year) |>
  mutate(
    p_violence   = percent_rank(violence_good),
    p_admin      = percent_rank(admin_good),
    p_corruption = percent_rank(corruption_good),
    p_rule       = percent_rank(rule_good)
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# 3. Conjunctive classification (strict 4-of-4; relaxed 3-of-4 for robustness) -
# ------------------------------------------------------------------------------
fragile_cutoff    <- 0.30   # at/below 30th pct within year = weak on a dimension
nonfragile_cutoff <- 0.70   # at/above 70th pct within year = strong on a dimension

cases_df <- cases_df |>
  mutate(
    n_weak   = rowSums(across(c(p_violence, p_admin, p_corruption, p_rule), ~ .x <= fragile_cutoff)),
    n_strong = rowSums(across(c(p_violence, p_admin, p_corruption, p_rule), ~ .x >= nonfragile_cutoff)),
    fragile_4of4    = n_weak   == 4,
    nonfragile_4of4 = n_strong == 4,
    fragile_3of4    = n_weak   >= 3,
    nonfragile_3of4 = n_strong >= 3,
    # For a conjunctive concept the MINIMUM is the on-concept summary
    # ("as weak as the weakest link"); mean kept as a secondary key.
    mean_integrity = rowMeans(across(c(p_violence, p_admin, p_corruption, p_rule))),
    min_integrity  = pmin(p_violence, p_admin, p_corruption, p_rule)
  )

# Quick counts -----------------------------------------------------------------
cases_df |>
  summarise(
    democratic_fragile_4of4_n    = sum(boix_democracy == 1 & fragile_4of4,    na.rm = TRUE),
    democratic_fragile_3of4_n    = sum(boix_democracy == 1 & fragile_3of4,    na.rm = TRUE),
    autocratic_nonfragile_4of4_n = sum(boix_democracy == 0 & nonfragile_4of4, na.rm = TRUE),
    autocratic_nonfragile_3of4_n = sum(boix_democracy == 0 & nonfragile_3of4, na.rm = TRUE)
  ) |>
  print()

# ------------------------------------------------------------------------------
# 4. Main cross-classification (robust to empty cells) -------------------------
# ------------------------------------------------------------------------------
class_counts <- cases_df |>
  transmute(
    regime_type = if_else(boix_democracy == 1, "Democracy", "Autocracy"),
    fragility_class = factor(
      case_when(
        fragile_4of4    ~ "Fragile",
        nonfragile_4of4 ~ "Non-fragile",
        TRUE            ~ "Intermediate"
      ),
      levels = c("Fragile", "Intermediate", "Non-fragile")
    )
  ) |>
  count(regime_type, fragility_class, name = "n") |>
  complete(regime_type, fragility_class, fill = list(n = 0)) |>
  group_by(regime_type) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

classification_table_4of4 <- class_counts |>
  mutate(entry = sprintf("%.1f\\%% (n = %s)", 100 * prop, format(n, big.mark = ","))) |>
  select(regime_type, fragility_class, entry) |>
  pivot_wider(names_from = fragility_class, values_from = entry) |>
  select(regime_type, Fragile, Intermediate, `Non-fragile`)

tab_main <- classification_table_4of4 |>
  kbl(format = "latex", booktabs = TRUE, align = c("l", "c", "c", "c"),
      caption = "Regime Type and Conjunctive Fragility Classification (Strict 4-of-4 Rule)",
      label = "tab:regime_fragility_crossclass_4of4", escape = FALSE) |>
  kable_styling(latex_options = c("hold_position"), font_size = 9) |>
  footnote(
    general = paste(
      "Entries report the share of country-years in each regime category",
      "classified as fragile, intermediate, or non-fragile under the strict",
      "conjunctive rule, which requires all four dimensions to fall below the",
      "30th percentile (fragile) or above the 70th percentile (non-fragile)",
      "within year."),
    threeparttable = TRUE, general_title = "Note: ")
tab_main

# ------------------------------------------------------------------------------
# 5. Off-diagonal country summaries: ONE table per pathway, strict and relaxed -
# folded into columns ----------------------------------------------------------
# ------------------------------------------------------------------------------
summarise_offdiag <- function(df, strict_flag) {
  df |>
    group_by(country, iso3) |>
    summarise(
      first_year    = min(year),
      last_year     = max(year),
      n_years_3of4  = n(),                              # rows are the 3-of-4 superset
      n_years_4of4  = sum({{ strict_flag }}),           # strict subset
      avg_integrity = round(mean(mean_integrity, na.rm = TRUE), 3),
      avg_minimum   = round(mean(min_integrity,  na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    arrange(desc(n_years_4of4), desc(n_years_3of4), avg_minimum)
}

demo_frag_cols <- c("Country", "ISO3", "First yr", "Last yr",
                    "Yrs (3/4)", "Yrs (4/4)", "Avg. integrity", "Avg. minimum")

# Democratic yet fragile -------------------------------------------------------
democratic_fragile <- cases_df |>
  ungroup() |>
  filter(boix_democracy == 1, fragile_3of4) |>
  summarise_offdiag(fragile_4of4)

tab_demo_frag <- democratic_fragile |>
  kbl(format = "latex", booktabs = TRUE,
      align = c("l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = demo_frag_cols,
      caption = "Democratic Country-Years Classified as Fragile: Country Summaries",
      label = "tab:democratic_fragile_countries", escape = FALSE) |>
  kable_styling(latex_options = c("hold_position"), font_size = 9) |>
  footnote(
    general = paste(
      "A democratic country-year is fragile when at least three (relaxed) or all",
      "four (strict) institutional dimensions fall at or below the 30th percentile",
      "of the contemporaneous yearly distribution. `Yrs (3/4)` and `Yrs (4/4)`",
      "count qualifying years under each rule. Average integrity is the mean of the",
      "four within-year percentile scores; average minimum is the mean of the",
      "weakest dimension (the on-concept summary for a conjunctive definition)."),
    threeparttable = TRUE, general_title = "Note: ")
tab_demo_frag

# Autocratic yet non-fragile ---------------------------------------------------
autocratic_nonfragile <- cases_df |>
  ungroup() |>
  filter(boix_democracy == 0, nonfragile_3of4) |>
  summarise_offdiag(nonfragile_4of4) |>
  # for the non-fragile side, higher integrity is the natural ordering
  arrange(desc(n_years_4of4), desc(n_years_3of4), desc(avg_minimum))

tab_aut_nonfrag <- autocratic_nonfragile |>
  kbl(format = "latex", booktabs = TRUE,
      align = c("l", "c", "c", "c", "c", "c", "c", "c"),
      col.names = demo_frag_cols,
      caption = "Autocratic Country-Years Classified as Non-fragile: Country Summaries",
      label = "tab:autocratic_nonfragile_countries", escape = FALSE) |>
  kable_styling(latex_options = c("hold_position"), font_size = 9) |>
  footnote(
    general = paste(
      "An autocratic country-year is non-fragile when at least three (relaxed) or",
      "all four (strict) institutional dimensions fall at or above the 70th",
      "percentile of the contemporaneous yearly distribution. Columns as in the",
      "democratic-fragile table."),
    threeparttable = TRUE, general_title = "Note: ")
tab_aut_nonfrag

# ------------------------------------------------------------------------------
# 6. FIGURES -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 6a. Aggregate cross-classification as a stacked proportion bar.
#     Replaces the main table visually; shows both regime distributions at once.
fig_crossclass <- class_counts |>
  ggplot(aes(x = regime_type, y = prop, fill = fragility_class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(prop >= 0.03, sprintf("%.0f%%", 100 * prop), "")),
            position = position_stack(vjust = 0.5), size = 3.2, colour = "white") +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c(Fragile = "#8c2d04", Intermediate = "#bdbdbd", `Non-fragile` = "#08519c")) +
  labs(x = NULL, y = "Share of country-years", fill = NULL,
       title = "Regime type and fragility cross-cut",
       subtitle = "Conjunctive classification (strict 4-of-4 rule), within-year percentiles") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave("fig_regime_fragility_crossclass.pdf", fig_crossclass, width = 6.5, height = 4.2)

# 6b. Off-diagonal countries as a dot/bar plot (replaces the country tables).
offdiag_plot_df <- bind_rows(
  democratic_fragile    |> mutate(pathway = "Democratic yet fragile"),
  autocratic_nonfragile |> mutate(pathway = "Autocratic yet non-fragile")
) |>
  group_by(pathway) |>
  slice_max(n_years_3of4, n = 100, with_ties = FALSE) |>   # top 15 per panel for legibility
  ungroup() |>
  # order countries by qualifying years for readability (single ordering across panels)
  mutate(country = factor(country, levels = unique(country[order(n_years_3of4)])))

fig_offdiag <- offdiag_plot_df |>
  ggplot(aes(x = n_years_3of4, y = country)) +
  geom_col(aes(fill = pathway), width = 0.65, alpha = 0.35) +
  geom_col(aes(x = n_years_4of4, fill = pathway), width = 0.65) +
  facet_wrap(~ pathway, scales = "free_y") +
  scale_fill_manual(values = c("Democratic yet fragile" = "#8c2d04",
                               "Autocratic yet non-fragile" = "#08519c"),
                    guide = "none") +
  labs(x = "Qualifying country-years (pale = 3-of-4 V-Dem dimensions, solid = 4-of-4 V-Dem dimensions)", 
       y = NULL,
       title = "Autocratic Yet Non-Fragile and Democratic Yet-Fragile Cases (1791-2022)",
       subtitle = "Countries whose regime type and fragility status diverge") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave("fig_offdiagonal_cases.png", fig_offdiag, width = 8, height = 5)
