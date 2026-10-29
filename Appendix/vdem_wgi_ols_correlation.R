# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: October 2026 -----------------------------------------------------------
#
# Purpose: Agreement between the V-Dem and WGI fragility indices (full and
# no-rule-of-law) on their overlapping country-years. Reports pooled Pearson r,
# within-country r (country and year adjusted), and pooled Spearman rho, each
# with a 95% country-cluster bootstrap percentile interval.

library(fixest)
library(ggplot2)

# standardized_data_final <- readRDS("standardized_data_final.RDS")
stopifnot(exists("standardized_data_final"))

BOOT_REPS <- 2000L
set.seed(20260909L)

pairs <- list(
  "Full index"            = c(vdem = "NEW_VDEM_LOADING_FACTOR_1_NORMALIZED.y",
                              wgi  = "WGI_LOADING_FACTOR_1_NORMALIZED"),
  "Excluding rule of law" = c(vdem = "NEW_VDEM_NORL_LOADING_FACTOR_1_NORMALIZED",
                              wgi  = "WGI_NORL_LOADING_FACTOR_1_NORMALIZED")
)

# Three correlations on one sample: pooled, within-country, rank.
cors <- function(d, id = d$iso3) {
  w <- demean(as.matrix(d[c("vdem", "wgi")]), f = list(id, d$year),
              as.matrix = TRUE, notes = FALSE)
  c(pooled   = cor(d$vdem, d$wgi),
    within   = cor(w[, 1], w[, 2]),
    spearman = cor(d$vdem, d$wgi, method = "spearman"))
}

# Resample whole country histories; duplicated countries get distinct FE ids.
boot_ci <- function(d) {
  rows <- split(seq_len(nrow(d)), d$iso3)
  G <- length(rows)
  draws <- t(replicate(BOOT_REPS, {
    blk <- rows[sample.int(G, G, replace = TRUE)]
    cors(d[unlist(blk), ], id = rep(seq_len(G), lengths(blk)))
  }))
  apply(draws, 2, quantile, c(0.025, 0.975))
}

results <- list()
plot_data <- list()
for (k in names(pairs)) {
  v <- pairs[[k]]
  # Orientation: higher = MORE fragile for both sources, as in the appendix.
  d <- data.frame(iso3 = standardized_data_final$ISO_CODE_3,
                  year = as.integer(standardized_data_final$YEAR),
                  vdem = -standardized_data_final[[v[["vdem"]]]],
                  wgi  = -standardized_data_final[[v[["wgi"]]]])
  d <- d[complete.cases(d), ]
  # Drop years in which WGI has no cross-country variation (1999 and 2001, when
  # WGI was not published: the full index is filled with a single constant).
  d <- d[ave(d$wgi, d$year, FUN = function(x) length(unique(x))) > 1, ]
  est <- cors(d)
  ci  <- boot_ci(d)
  results[[k]] <- data.frame(index = k, measure = names(est), estimate = est,
                             conf_low = ci[1, ], conf_high = ci[2, ],
                             n = nrow(d), countries = length(unique(d$iso3)),
                             years = paste(range(d$year), collapse = "-"))
  d$panel <- sprintf("%s\nr = %.2f [%.2f, %.2f]; within-country r = %.2f [%.2f, %.2f]",
                     k, est[["pooled"]], ci[1, "pooled"], ci[2, "pooled"],
                     est[["within"]], ci[1, "within"], ci[2, "within"])
  plot_data[[k]] <- d
}
results <- do.call(rbind, results)
rownames(results) <- NULL
print(results, digits = 3)

plot_data <- do.call(rbind, plot_data)
plot_data$panel <- factor(plot_data$panel, levels = unique(plot_data$panel))

p <- ggplot(plot_data, aes(vdem, wgi)) +
  geom_point(alpha = 0.15, size = 0.8, colour = "grey30") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#8B1A1A", linewidth = 0.8) +
  facet_wrap(~panel) +
  labs(x = "V-Dem fragility index", y = "WGI fragility index",
       caption = paste0(
         "Country-years with both indices available. Higher scores = more fragile.\n",
         "Brackets: 95% country-cluster bootstrap percentile intervals (",
         format(BOOT_REPS, big.mark = ","), " draws). Within-country r adjusts for ",
         "country and year fixed effects.")) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 9))
print(p)

ggsave("vdem_wgi_scatter.png", p, width = 10, height = 5, dpi = 300, bg = "white")
ggsave("vdem_wgi_scatter.pdf", p, width = 10, height = 5, bg = "white")
