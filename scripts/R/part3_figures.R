# =============================================================================
# Part 3: Statistical Inference — figure generation
# =============================================================================
# Companion script for drafts/slides/part3_inference.tex
#
# Generates every data figure used in the Part 3 deck. Conceptual diagrams
# (the population -> sample -> estimate flow, the Type I / Type II grid) are
# drawn in TikZ or tabular inside the .tex file and are deliberately NOT
# produced here.
#
# Run from the project root:
#   Rscript scripts/R/part3_figures.R
#
# Data sources:
#   Berkshire Hathaway and US market excess returns, monthly 1976-2023
#   US stocks and 10-year Treasuries, monthly 1941-2023   (continuity with Part 2)
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source("scripts/R/theme.R")

# Every simulated figure seeds its OWN block, immediately before it draws, with
# BASE_SEED + a fixed offset. A single seed at the top of the script makes the
# whole run reproducible but not STABLE: editing any earlier block shifts the
# RNG stream, so unrelated figures downstream silently change and the numbers
# quoted on the slides drift out of date. Per-block seeds mean an edit here
# changes only the figure it belongs to.
BASE_SEED <- 20260818
set.seed(BASE_SEED)

# ---------- Paths and shared settings ----------------------------------------

RAW_DIR <- "data/raw"
# Buffett data now lives in the project's own data/raw/ rather than in the
# received course bundle. Same file, descriptive name.
BUFFETT_CSV <- file.path(RAW_DIR, "berkshire_market_monthly.csv")
STOCKS_BONDS_CSV <- file.path(RAW_DIR, "stocks_bonds.csv")
FIG_DIR <- "output/figures"

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# 16:9 Beamer frame at 9pt: a full-width figure is ~ 5.2in, a column figure ~ 3.4in
FIG_W <- 5.2
FIG_H <- 3.0

#' Save a plot as PDF with consistent device settings.
save_fig <- function(plot, name, width = FIG_W, height = FIG_H) {
  path <- file.path(FIG_DIR, paste0(name, ".pdf"))
  ggsave(path, plot = plot, device = cairo_pdf,
         width = width, height = height, units = "in")
  cat(sprintf("  wrote %s (%.1f x %.1f in)\n", path, width, height))
  invisible(path)
}

# Named palette shortcuts (identical to part2_figures.R)
col_blue <- OKABE_ITO[["blue"]]
col_orange <- OKABE_ITO[["orange"]]
col_verm <- OKABE_ITO[["vermillion"]]
col_green <- OKABE_ITO[["green"]]
col_sky <- OKABE_ITO[["sky_blue"]]
col_purple <- OKABE_ITO[["purple"]]
col_grey <- "#BFBFBF"

MONTHS_PER_YEAR <- 12

# Population conventions carried over from Part 2: variance is E[(X - mu)^2].
pop_var <- function(x) mean((x - mean(x))^2)
pop_sd <- function(x) sqrt(pop_var(x))

# =============================================================================
# Data
# =============================================================================

cat("Reading data ...\n")

buf <- read_csv(BUFFETT_CSV,
                col_types = cols(eom = col_date(), .default = col_double()),
                progress = FALSE) |>
  filter(!is.na(buffett_exc), !is.na(mkt_exc))

# The deck tests OUTPERFORMANCE, not the raw excess return: "did Buffett beat
# the market?" is the question students actually care about, and differencing
# out the market makes the null (zero outperformance) the interesting one.
buf <- buf |> mutate(outperf = buffett_exc - mkt_exc)

n_buf <- nrow(buf)
d_bar <- mean(buf$outperf)
d_sd <- sd(buf$outperf)                 # n-1 convention: this is an ESTIMATE
d_se <- d_sd / sqrt(n_buf)
d_t <- d_bar / d_se
d_p <- 2 * pt(-abs(d_t), df = n_buf - 1)
d_ci <- d_bar + c(-1, 1) * qt(0.975, df = n_buf - 1) * d_se

sb <- read_csv(STOCKS_BONDS_CSV,
               col_types = cols(eom = col_date(), .default = col_double()),
               progress = FALSE) |>
  filter(!is.na(us_stock_market), !is.na(treasury10yr))

stk <- sb$us_stock_market
n_sb <- length(stk)
mu_s <- mean(stk)
sd_s <- pop_sd(stk)

# Equity premium: stocks over T-bills, the parameter every asset allocation
# decision leans on.
ep <- (sb$us_stock_market - sb$tbil)
ep <- ep[!is.na(ep)]
n_ep <- length(ep)
ep_bar <- mean(ep)
ep_se <- sd(ep) / sqrt(n_ep)
ep_ci <- ep_bar + c(-1, 1) * qt(0.975, df = n_ep - 1) * ep_se

cat(sprintf("  buffett:      n = %d, %s to %s\n", n_buf,
            format(min(buf$eom)), format(max(buf$eom))))
cat(sprintf("  stocks/bonds: n = %d, %s to %s\n", n_sb,
            format(min(sb$eom)), format(max(sb$eom))))

# ---------- The running "population" for the estimator figures ---------------
# A normal calibrated to the US stock series of Part 2. Using a smooth, known
# population is the point: the whole section compares what an estimator does
# against a truth we get to see. Everything is in PERCENT PER MONTH.

# The empirical mean of the US stock series is 0.9886% per month. The deck
# quotes it as 1%, so the population is calibrated to exactly 1% and every
# figure label agrees with the slide text. The sd is left at its empirical
# value, 4.28%.
POP_MU <- round(mu_s * 100)
POP_SD <- sd_s * 100
N_SAMP <- 60          # five years of monthly data -- a realistic sample size
N_REPS <- 20000       # replications behind every sampling distribution

# The two illustration figures ("A New Sample, a New Answer" and its zoom-in)
# draw a DELIBERATELY small sample: at n = 25 the three sample means visibly
# disagree, which is the entire point of those frames. The sampling-distribution
# machinery below stays at N_SAMP.
N_ILLUS <- 25

pop_grid <- seq(POP_MU - 4 * POP_SD, POP_MU + 4 * POP_SD, length.out = 800)
pop_df <- tibble(x = pop_grid, y = dnorm(pop_grid, POP_MU, POP_SD))

#' Draw `reps` samples of size `n` from the running population and return the
#' value of `stat` on each. One helper, so every sampling distribution in the
#' deck is generated the same way.
sampling_dist <- function(n, reps = N_REPS, stat = mean) {
  m <- matrix(rnorm(n * reps, POP_MU, POP_SD), nrow = reps, ncol = n)
  apply(m, 1, stat)
}

# =============================================================================
# 1. Question of the day: Berkshire against the market
# =============================================================================
# Growth of $1 on a log axis. A linear axis compresses the first thirty years
# into the floor, which hides exactly the steady, year-after-year gap that makes
# the "skill or luck?" question interesting.

qotd <- buf |>
  transmute(eom,
            Berkshire = cumprod(1 + buffett_exc + rf),
            `US market` = cumprod(1 + mkt_exc + rf)) |>
  pivot_longer(-eom, names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = c("Berkshire", "US market")))

qotd_end <- qotd |> filter(eom == max(eom))

p_qotd <- ggplot(qotd, aes(eom, value, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("Berkshire" = col_orange,
                                 "US market" = col_blue)) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000),
                labels = c("$1", "$10", "$100", "$1,000", "$10,000")) +
  labs(x = NULL, y = "Value of $1 invested (log scale)") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.22, 0.86),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_qotd, "p3_qotd_buffett", width = 4.4, height = 2.8)

# =============================================================================
# 2. One population, one sample
# =============================================================================
# The sample is drawn once and then REUSED by figure 3 as its third panel, so
# students see the same 25 points twice.

# =============================================================================
# 2a. What i.i.d. means, by counter-example
# =============================================================================
# Two runs of 25 monthly returns with the same mean. The first is a random
# sample; the second breaks INDEPENDENCE -- an AR(1) with the same marginal sd,
# so the points wander in runs rather than resetting each month. Holding the
# marginal sd fixed is what makes this a clean counter-example: the two panels
# differ in dependence and in nothing else.
#
# A third panel breaking IDENTICALLY DISTRIBUTED (sd jumping partway through)
# was dropped -- that half is now carried by the one-draw figure on the
# preceding frame, and this frame is about independence alone.
# The dashed band is mu +/- 2 sd of the population, identical in both panels.

set.seed(BASE_SEED + 3)

iid_t <- seq_len(N_ILLUS)

r_iid <- rnorm(N_ILLUS, POP_MU, POP_SD)

# phi scaled so the AR(1) has the same MARGINAL sd as the population: the panel
# must differ in dependence only, not in spread, or it would show both faults.
PHI <- 0.85
r_dep <- POP_MU + as.numeric(stats::filter(
  rnorm(N_ILLUS, 0, POP_SD * sqrt(1 - PHI^2)), PHI, method = "recursive"))

IID_LABS <- c("A random sample (independent)", "Not independent")

iid_df <- bind_rows(
  tibble(t = iid_t, r = r_iid, panel = IID_LABS[1]),
  tibble(t = iid_t, r = r_dep, panel = IID_LABS[2])
) |>
  mutate(panel = factor(panel, levels = IID_LABS))

p_iid <- ggplot(iid_df, aes(t, r)) +
  geom_hline(yintercept = POP_MU + c(-2, 2) * POP_SD, colour = col_grey,
             linetype = 2, linewidth = 0.4) +
  geom_hline(yintercept = POP_MU, colour = col_blue, linewidth = 0.6) +
  geom_line(colour = col_orange, linewidth = 0.5, alpha = 0.9) +
  geom_point(colour = col_orange, size = 1.1) +
  # Stacked, not side by side: the two runs share one Month axis and the eye
  # compares them vertically at equal x, which is where the runs in the lower
  # panel become obvious against the resetting in the upper one.
  facet_wrap(~panel, ncol = 1) +
  coord_cartesian(ylim = c(POP_MU - 3.2 * POP_SD, POP_MU + 3.2 * POP_SD)) +
  labs(x = "Month", y = "Return (%)") +
  theme_custom() +
  theme(strip.text = element_text(size = 8.5, colour = "black"))

save_fig(p_iid, "p3_iid", width = 3.4, height = 2.4)

# =============================================================================
# 2b. One population, one sample
# =============================================================================
# All three are drawn here so that figure 3 can show the SAME samples again.
# The illustration below uses sample 3: samples 1 and 2 both land within a
# quarter of a percent of mu, which makes the one point of the picture -- that
# Xbar misses mu -- invisible. Sample 3 misses by about 1.6 standard errors,
# which is an ordinary draw and a visible one.
set.seed(BASE_SEED + 2)          # figures 2 and 3: the illustration samples
samps <- lapply(1:3, function(k) rnorm(N_ILLUS, POP_MU, POP_SD))
samp1 <- samps[[3]]

p_popsamp <- ggplot(pop_df, aes(x, y)) +
  geom_area(fill = col_blue, alpha = 0.13) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  # sides = "b" and inherit.aes = FALSE are both required: geom_rug defaults to
  # sides = "bl", and the inherited y aesthetic would otherwise draw a second
  # rug of 25 identical ticks stacked on the left axis.
  geom_rug(data = tibble(x = samp1), aes(x = x), inherit.aes = FALSE,
           sides = "b", colour = col_orange, linewidth = 0.4,
           length = unit(0.06, "npc"), alpha = 0.8) +
  geom_vline(xintercept = POP_MU, colour = col_blue, linewidth = 0.8) +
  geom_vline(xintercept = mean(samp1), colour = col_verm, linewidth = 0.8,
             linetype = 2) +
  annotate("text", x = POP_MU + 0.4, y = dnorm(POP_MU, POP_MU, POP_SD) * 1.06,
           label = sprintf('mu~"="~"%g"', POP_MU), parse = TRUE,
           colour = col_blue, size = 3.4, hjust = 0) +
  annotate("text", x = mean(samp1) - 0.4,
           y = dnorm(POP_MU, POP_MU, POP_SD) * 0.86,
           # The number is QUOTED inside the plotmath string; unquoted, R drops
           # the trailing zero and "0.30" prints as "0.3".
           label = sprintf('bar(X)~"="~"%.2f"', mean(samp1)), parse = TRUE,
           colour = col_verm, size = 3.4, hjust = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(xlim = c(POP_MU - 3.6 * POP_SD, POP_MU + 3.6 * POP_SD)) +
  labs(x = "Monthly return (%)", y = "f(x)") +
  theme_custom()

save_fig(p_popsamp, "p3_population_sample", width = 4.6, height = 2.7)

# -----------------------------------------------------------------------------
# 2c. One draw, nothing else on it
# -----------------------------------------------------------------------------
# Sample 1 of the three, drawn on its population with NO mu line and NO Xbar
# line. This figure sits beside the definition of a random sample, where the
# only thing being claimed is "these n ticks came out of that curve" -- a
# vertical line for an estimate would answer a question the slide has not asked
# yet. Same x window as every other illustration figure.

p_onedraw <- ggplot(pop_df, aes(x, y)) +
  geom_area(fill = col_blue, alpha = 0.13) +
  geom_line(colour = col_blue, linewidth = 0.7) +
  geom_rug(data = tibble(x = samps[[1]]), aes(x = x), inherit.aes = FALSE,
           sides = "b", colour = col_orange, linewidth = 0.45,
           length = unit(0.10, "npc"), alpha = 0.85) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.06))) +
  coord_cartesian(xlim = c(POP_MU - 3.6 * POP_SD, POP_MU + 3.6 * POP_SD)) +
  labs(x = "Monthly return (%)", y = "f(x)") +
  theme_custom()

save_fig(p_onedraw, "p3_one_draw", width = 2.9, height = 1.5)

# =============================================================================
# 3. Three samples, three different estimates
# =============================================================================

three <- bind_rows(
  tibble(x = samps[[1]], rep = "Sample 1"),
  tibble(x = samps[[2]], rep = "Sample 2"),
  tibble(x = samps[[3]], rep = "Sample 3")
)

three_means <- three |> group_by(rep) |> summarise(xbar = mean(x), .groups = "drop")

# Each panel shows the SAME normal population, so the only thing that differs
# across panels is the draw and the sample mean it produces. A count histogram
# was the earlier design; at n = 25 the bars are too sparse to read, and they
# also hid the fact that all three panels share one population.
three_pop <- expand_grid(rep = unique(three$rep), pop_df)
peak <- dnorm(POP_MU, POP_MU, POP_SD)

p_three <- ggplot(three_pop, aes(x, y)) +
  geom_area(fill = col_blue, alpha = 0.13) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  geom_rug(data = three, aes(x = x), inherit.aes = FALSE, sides = "b",
           colour = col_orange, linewidth = 0.4,
           length = unit(0.09, "npc"), alpha = 0.85) +
  geom_vline(xintercept = POP_MU, colour = col_blue, linewidth = 0.6) +
  geom_vline(data = three_means, aes(xintercept = xbar), colour = col_verm,
             linewidth = 0.6, linetype = 2) +
  geom_text(data = three_means,
            aes(x = POP_MU + 3.5 * POP_SD, y = peak * 1.10,
                label = sprintf("bar(X)~'='~'%.2f'", xbar)),
            inherit.aes = FALSE, parse = TRUE, colour = col_verm,
            size = 3.0, hjust = 1) +
  facet_wrap(~rep, ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.14))) +
  coord_cartesian(xlim = c(POP_MU - 3.6 * POP_SD, POP_MU + 3.6 * POP_SD)) +
  labs(x = "Monthly return (%)", y = "f(x)") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, colour = "black"))

# Shorter than the other figures on purpose: this frame carries the figure,
# two lines of text and an insightbox, and at 2.1in tall it overflowed.
save_fig(p_three, "p3_three_samples", width = 5.4, height = 1.8)

# =============================================================================
# 4. The sampling distribution of the sample mean
# =============================================================================
# Drawn on the SAME x-axis as the population, because the whole point is that
# the sampling distribution is dramatically narrower -- a rescaled axis would
# throw that away.

# n = N_ILLUS here, matching the three-sample frames: this figure IS those three
# draws repeated twenty thousand times, so a different n would break the story.
# The population density is deliberately NOT drawn -- only Xbar and E[Xbar].
# The x range is still the population's +/- 3.6 sd, unchanged from the two
# preceding figures, so the narrowness of Xbar reads off the shared axis rather
# than off a curve behind it.
set.seed(BASE_SEED + 4)          # sampling distribution of Xbar
xbar_25 <- sampling_dist(N_ILLUS)

# Annotations are placed relative to the peak of the sampling distribution
# rather than at fixed heights, so they follow the curve if n ever changes.
peak_sd <- dnorm(POP_MU, POP_MU, POP_SD / sqrt(N_ILLUS))

p_sampdist <- ggplot(tibble(xbar = xbar_25), aes(xbar)) +
  geom_density(aes(y = after_stat(density)), fill = col_orange, alpha = 0.35,
               colour = col_orange, linewidth = 0.8) +
  geom_vline(xintercept = POP_MU, colour = col_blue, linewidth = 0.9) +
  annotate("text", x = POP_MU + 3.5 * POP_SD, y = peak_sd * 0.80,
           label = sprintf("sampling distribution\nof X-bar  (n = %d)", N_ILLUS),
           colour = col_orange, size = 3.1, hjust = 1, lineheight = 0.95) +
  annotate("text", x = POP_MU - 0.35, y = peak_sd * 1.04,
           label = 'E*"["*bar(X)*"]"~"="~mu', parse = TRUE,
           colour = col_blue, size = 3.4, hjust = 1) +
  coord_cartesian(xlim = c(POP_MU - 3.6 * POP_SD, POP_MU + 3.6 * POP_SD)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Monthly return (%)", y = "Density") +
  theme_custom()

save_fig(p_sampdist, "p3_sampling_dist", width = 4.8, height = 2.7)

# =============================================================================
# 5. Unbiased versus biased
# =============================================================================
# The biased competitor is a real, recognisable mistake rather than an arbitrary
# shift: it is the sample mean of the most recent 30 of the 60 months, applied
# to a market that trended -- i.e. a "recency" estimator. Simulated here as a
# shifted, wider sampling distribution.

bias_grid <- seq(POP_MU - 4 * POP_SD / sqrt(N_SAMP),
                 POP_MU + 6 * POP_SD / sqrt(N_SAMP), length.out = 800)
se_xbar <- POP_SD / sqrt(N_SAMP)

bias_df <- bind_rows(
  tibble(x = bias_grid, y = dnorm(bias_grid, POP_MU, se_xbar),
         which = "Unbiased"),
  tibble(x = bias_grid, y = dnorm(bias_grid, POP_MU + 1.6 * se_xbar, se_xbar),
         which = "Biased")
) |>
  mutate(which = factor(which, levels = c("Unbiased", "Biased")))

p_bias <- ggplot(bias_df, aes(x, y, colour = which, fill = which)) +
  geom_area(position = "identity", alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = POP_MU, colour = "black", linewidth = 0.5,
             linetype = 2) +
  annotate("segment", x = POP_MU, xend = POP_MU + 1.6 * se_xbar,
           y = dnorm(0, 0, se_xbar) * 1.06, yend = dnorm(0, 0, se_xbar) * 1.06,
           colour = col_verm, linewidth = 0.5,
           arrow = arrow(length = unit(0.16, "cm"), ends = "both")) +
  annotate("text", x = POP_MU + 0.8 * se_xbar,
           y = dnorm(0, 0, se_xbar) * 1.15, label = "bias",
           colour = col_verm, size = 3.2) +
  annotate("text", x = POP_MU - 0.12, y = dnorm(0, 0, se_xbar) * 0.30,
           label = 'mu', parse = TRUE, size = 3.6, hjust = 1) +
  scale_colour_manual(values = c("Unbiased" = col_blue, "Biased" = col_orange)) +
  scale_fill_manual(values = c("Unbiased" = col_blue, "Biased" = col_orange)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(x = "Value of the estimator", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.87, 0.80),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_bias, "p3_unbiased_vs_biased", width = 4.6, height = 2.6)

# =============================================================================
# 6. Efficiency: two unbiased estimators, one tighter
# =============================================================================
# Sample mean versus sample median. Both unbiased for a symmetric population,
# but the median throws away information: its variance is pi/2 times larger.

# Both at N_SAMP: this comparison is about mean vs median at one fixed n, and
# the wider sampling distributions of a smaller n would only muddy it.
set.seed(BASE_SEED + 5)          # sampling distribution of Xbar at n = 60
xbar_60 <- sampling_dist(N_SAMP)
set.seed(BASE_SEED + 6)          # sampling distribution of the median
med_60 <- sampling_dist(N_SAMP, stat = median)

eff_df <- bind_rows(
  tibble(v = xbar_60, which = "Sample mean"),
  tibble(v = med_60, which = "Sample median")
) |>
  mutate(which = factor(which, levels = c("Sample mean", "Sample median")))

p_eff <- ggplot(eff_df, aes(v, colour = which, fill = which)) +
  geom_density(alpha = 0.20, linewidth = 0.8) +
  geom_vline(xintercept = POP_MU, colour = "black", linewidth = 0.5,
             linetype = 2) +
  scale_colour_manual(values = c("Sample mean" = col_blue,
                                 "Sample median" = col_orange)) +
  scale_fill_manual(values = c("Sample mean" = col_blue,
                               "Sample median" = col_orange)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Value of the estimator", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.78, 0.84),
        legend.key.width = unit(0.6, "cm"),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_eff, "p3_efficiency", width = 4.4, height = 2.6)

# =============================================================================
# 7. Consistency: the sampling distribution collapses on mu
# =============================================================================

CONS_N <- c(12, 60, 250, 1000)   # 1 year, 5 years, ~20 years, ~83 years

cons_df <- lapply(CONS_N, function(n) {
  tibble(x = seq(POP_MU - 4 * POP_SD / sqrt(12),
                 POP_MU + 4 * POP_SD / sqrt(12), length.out = 900)) |>
    mutate(y = dnorm(x, POP_MU, POP_SD / sqrt(n)),
           n = factor(sprintf("n = %s", format(n, big.mark = ",")),
                      levels = sprintf("n = %s",
                                       format(CONS_N, big.mark = ",",
                                              trim = TRUE))))
}) |>
  bind_rows()

p_cons <- ggplot(cons_df, aes(x, y, colour = n)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = POP_MU, colour = "black", linewidth = 0.5,
             linetype = 2) +
  annotate("text", x = POP_MU - 0.1, y = 3.2, label = 'mu', parse = TRUE,
           size = 3.6, hjust = 1) +
  scale_colour_manual(values = unname(OKABE_ITO[c("sky_blue", "blue",
                                                  "green", "vermillion")])) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.06))) +
  labs(x = "Value of the sample mean (%)", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.87, 0.72),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_cons, "p3_consistency", width = 4.6, height = 2.6)

# =============================================================================
# 8. Why n - 1
# =============================================================================
# Simulated bias of the two variance formulas as a function of n. The 1/n
# version is systematically too small by exactly the factor (n-1)/n, which is
# ruinous at n = 5 and invisible at n = 200 -- and the picture says so without
# anyone having to prove it.

VAR_N <- c(3, 4, 5, 6, 8, 10, 15, 20, 30, 50, 100, 200)
VAR_REPS <- 8000

set.seed(BASE_SEED + 8)          # n vs n-1 in the variance estimator
var_sim <- lapply(VAR_N, function(n) {
  m <- matrix(rnorm(n * VAR_REPS, POP_MU, POP_SD), nrow = VAR_REPS)
  ss <- rowSums((m - rowMeans(m))^2)
  tibble(n = n,
         `Divide by n` = mean(ss / n) / POP_SD^2,
         `Divide by n - 1` = mean(ss / (n - 1)) / POP_SD^2)
}) |>
  bind_rows() |>
  pivot_longer(-n, names_to = "which", values_to = "ratio") |>
  mutate(which = factor(which, levels = c("Divide by n - 1", "Divide by n")))

p_nm1 <- ggplot(var_sim, aes(n, ratio, colour = which)) +
  geom_hline(yintercept = 1, colour = "black", linewidth = 0.5, linetype = 2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  annotate("text", x = 200, y = 1.045, label = "truth", size = 3.0,
           hjust = 1, colour = "black") +
  scale_colour_manual(values = c("Divide by n - 1" = col_blue,
                                 "Divide by n" = col_orange)) +
  scale_x_log10(breaks = c(3, 10, 30, 100, 200),
                labels = c("3", "10", "30", "100", "200")) +
  scale_y_continuous(limits = c(0.6, 1.08)) +
  labs(x = "Sample size n  (log scale)",
       y = "Average estimate / true variance") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.80, 0.32),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_nm1, "p3_n_minus_1", width = 4.6, height = 2.6)

# =============================================================================
# 9. The square-root-of-n rule
# =============================================================================

sen_df <- tibble(n = seq(10, 1600, by = 5)) |>
  mutate(se = POP_SD / sqrt(n))

sen_marks <- tibble(n = c(100, 400, 1600)) |>
  mutate(se = POP_SD / sqrt(n),
         lab = sprintf("n = %d\nse = %.2f", n, se))

p_sen <- ggplot(sen_df, aes(n, se)) +
  geom_line(colour = col_blue, linewidth = 0.9) +
  geom_segment(data = sen_marks, aes(x = 0, xend = n, y = se, yend = se),
               colour = col_grey, linewidth = 0.4, linetype = 2) +
  geom_point(data = sen_marks, colour = col_verm, size = 2.2) +
  geom_text(data = sen_marks, aes(label = lab), colour = col_verm, size = 3.0,
            hjust = -0.15, vjust = -0.10, lineheight = 0.95) +
  scale_x_continuous(limits = c(0, 1900), breaks = c(0, 400, 800, 1200, 1600)) +
  scale_y_continuous(limits = c(0, 1.45), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Sample size n", y = expression("se"(bar(X))~~"(% per month)")) +
  theme_custom()

save_fig(p_sen, "p3_se_root_n", width = 4.6, height = 2.6)

# =============================================================================
# 10. The Central Limit Theorem
# =============================================================================
# The population is deliberately hideous -- a right-skewed, spiky mixture with
# nothing normal about it -- so that the normal curve appearing by n = 30 reads
# as a result rather than as an artefact of starting close to normal.

CLT_REPS <- 30000
# Last panel is n = 100, not 30: this population is ugly enough that n = 30 is
# still visibly not there, which is the honest lesson -- "n = 30 is plenty" is a
# rule for mildly non-normal data, not for this.
CLT_N <- c(1, 2, 10, 100)

#' Draw from the ugly population: 85% of the time a small exponential loss-ish
#' payoff, 15% of the time a large positive jump.
r_ugly <- function(k) {
  jump <- rbinom(k, 1, 0.15)
  (1 - jump) * rexp(k, rate = 1.6) + jump * (3 + rexp(k, rate = 0.8))
}

set.seed(BASE_SEED + 10)         # CLT on an ugly population
ugly_mu <- mean(r_ugly(2e6))
ugly_sd <- sd(r_ugly(2e6))

clt_df <- lapply(CLT_N, function(n) {
  m <- matrix(r_ugly(n * CLT_REPS), nrow = CLT_REPS, ncol = n)
  tibble(z = (rowMeans(m) - ugly_mu) / (ugly_sd / sqrt(n)),
         n_num = n,
         n = factor(sprintf("n = %d", n),
                    levels = sprintf("n = %d", CLT_N)))
}) |>
  bind_rows()

# Standardising each panel is what makes the four comparable: without it the
# distributions shrink toward a spike and the CHANGE OF SHAPE -- the actual
# content of the theorem -- becomes impossible to see.
clt_norm <- tibble(z = seq(-4, 4, length.out = 600)) |>
  mutate(y = dnorm(z)) |>
  tidyr::crossing(n = factor(sprintf("n = %d", CLT_N),
                             levels = sprintf("n = %d", CLT_N)))

p_clt <- ggplot(clt_df, aes(z)) +
  geom_histogram(aes(y = after_stat(density)), bins = 70, fill = col_orange,
                 colour = NA, alpha = 0.8) +
  geom_line(data = clt_norm, aes(z, y), colour = col_blue, linewidth = 0.8) +
  facet_wrap(~n, ncol = 4) +
  coord_cartesian(xlim = c(-3.6, 3.6), ylim = c(0, 0.62)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = expression("Standardized sample mean  "*(bar(X)-mu)/(sigma/sqrt(n))),
       y = "Density") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, colour = "black"))

save_fig(p_clt, "p3_clt_grid", width = 5.4, height = 2.2)

# =============================================================================
# 10b. The same four experiments WITHOUT the sqrt(n)
# =============================================================================
# Plots (Xbar - mu)/sigma, which is exactly the z of the previous figure divided
# by sqrt(n) -- the same 30,000 draws, one factor removed, so any difference
# between the two figures is that factor and nothing else.
#
# The point is what is NOT here: no convergence to a shape, just a collapse onto
# zero. That is the LLN. Dividing by sigma/sqrt(n) rather than sigma is the zoom
# lens that holds the picture at a fixed width while the collapse happens, and
# only under that lens is there a limiting curve to see.

clt_raw <- clt_df |> mutate(z = z / sqrt(n_num))

p_clt_raw <- ggplot(clt_raw, aes(z)) +
  geom_histogram(aes(y = after_stat(density)), bins = 70, fill = col_orange,
                 colour = NA, alpha = 0.8) +
  geom_vline(xintercept = 0, colour = col_blue, linewidth = 0.7) +
  facet_wrap(~n, ncol = 4) +
  # Same x window and the same fixed y as the CLT grid, because the two figures
  # are meant to be read against each other. The n = 100 spike leaving the top
  # of its panel IS the finding, not a fault to be scaled away.
  coord_cartesian(xlim = c(-3.6, 3.6), ylim = c(0, 1.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = expression("Sample mean, in population sd units  "*(bar(X)-mu)/sigma),
       y = "Density") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, colour = "black"))

save_fig(p_clt_raw, "p3_clt_no_sqrtn", width = 5.4, height = 2.2)

# Section 11 ("The CLT in the wild: single stocks vs portfolios") was removed
# along with its stock_returns.csv dependency. Its figure, p3_portfolio_normality,
# appeared in no deck. Numbering deliberately keeps the gap: the Beamer source
# cites these section numbers (e.g. "section 14d"), so renumbering would
# silently invalidate those pointers.

# =============================================================================
# 12. The t distribution against the normal
# =============================================================================

t_grid <- seq(-4.5, 4.5, length.out = 900)

t_df <- bind_rows(
  tibble(x = t_grid, y = dt(t_grid, df = 2), which = "t, df = 2"),
  tibble(x = t_grid, y = dt(t_grid, df = 10), which = "t, df = 10"),
  tibble(x = t_grid, y = dnorm(t_grid), which = "Normal")
) |>
  mutate(which = factor(which, levels = c("Normal", "t, df = 10", "t, df = 2")))

p_t <- ggplot(t_df, aes(x, y, colour = which, linetype = which)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Normal" = col_blue, "t, df = 10" = col_green,
                                 "t, df = 2" = col_orange)) +
  scale_linetype_manual(values = c("Normal" = 1, "t, df = 10" = 1,
                                   "t, df = 2" = 1)) +
  scale_y_continuous(limits = c(0, 0.44), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "t", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.80, 0.80),
        legend.key.width = unit(0.6, "cm"),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_t, "p3_t_vs_normal", width = 4.4, height = 2.5)

# =============================================================================
# 13. What a p-value is: the two shaded tails
# =============================================================================

# The curve here is dnorm, so the statistic on it is Z, not t -- the frame
# quantifies "unlikely" in the normal's units. Berkshire's own statistic, so
# this frame and p3_buffett_z show the same 563 months rather than asking the
# room to hold a second example. The tail labels use d_p, the deck's canonical
# Buffett p, which comes from t_562; on a normal curve the area beyond 3.14 is
# 0.00084 a side rather than 0.00089, a difference in the fifth decimal that
# matters far less than every frame quoting the same number.
Z_SHOWN <- d_t

# Same frame as p3_buffett_z (section 14e) so the two Step 4 figures read as one
# picture: identical panel bounds, identical "Distribution of Z under H_0"
# label, identical dotted-marker style. The difference is what is shaded --
# there the 5% rejection region, here everything beyond the observed Z, which
# is the p-value itself.
MX_PV <- dnorm(0)
pv_df <- tibble(x = seq(-4.6, 4.84, length.out = 1200)) |>
  mutate(y = dnorm(x))

p_pval <- ggplot(pv_df, aes(x, y)) +
  geom_area(data = filter(pv_df, x >= Z_SHOWN), fill = col_verm, alpha = 0.55) +
  geom_area(data = filter(pv_df, x <= -Z_SHOWN), fill = col_verm, alpha = 0.55) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  annotate("text", x = -2.18, y = MX_PV * 0.70,
           label = 'atop("Distribution of"~Z, "under"~H[0])',
           parse = TRUE, size = 3.1, colour = col_blue, hjust = 1) +
  # Same marker heights and wording as p3_buffett_z, so the two Step 4 figures
  # differ only in what is shaded.
  annotate("segment", x = Z_SHOWN, xend = Z_SHOWN, y = 0, yend = MX_PV * 0.33,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("point", x = Z_SHOWN, y = 0, colour = col_verm, size = 2.4) +
  annotate("text", x = Z_SHOWN, y = MX_PV * 0.365,
           label = sprintf("Buffett:\nZ = %.2f", Z_SHOWN),
           colour = col_verm, size = 3.0, hjust = 0.5, vjust = 0,
           lineheight = 0.95) +
  # Each tail is labelled with HALF the p-value, mirrored. Labelling the total
  # once means an arrow that crosses the whole panel; two short symmetric arrows
  # also make the two-sidedness of the test visible without a word of text.
  annotate("segment", x = -4.30, xend = -3.32, y = 0.085, yend = 0.006,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = -4.40, y = 0.100,
           label = sprintf("p/2 = %.4f", d_p / 2),
           colour = col_verm, size = 3.0, hjust = 0) +
  annotate("segment", x = 4.30, xend = 3.32, y = 0.085, yend = 0.006,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = 4.40, y = 0.100,
           label = sprintf("p/2 = %.4f", d_p / 2),
           colour = col_verm, size = 3.0, hjust = 1) +
  scale_y_continuous(limits = c(0, 0.44), expand = expansion(mult = c(0, 0.06))) +
  coord_cartesian(xlim = c(-4.6, 4.84)) +
  labs(x = expression(Z == (bar(X) - mu[0]) / (sigma / sqrt(n))),
       y = "Density") +
  theme_custom()

save_fig(p_pval, "p3_pvalue_tail", width = 4.6, height = 2.6)

# =============================================================================
# 14. The Buffett test, in return units
# =============================================================================
# Same picture as figure 13, but on the axis of the actual quantity being
# estimated: average monthly outperformance. The observed value is so far out
# that the curve is flat underneath it -- which is the argument.

buf_grid <- seq(-4.6 * d_se, max(4 * d_se, d_bar * 1.28), length.out = 1200)
buf_df <- tibble(x = buf_grid * 100, y = dnorm(buf_grid, 0, d_se))
crit <- qnorm(0.975) * d_se

p_buf <- ggplot(buf_df, aes(x, y)) +
  geom_area(data = filter(buf_df, x >= crit * 100), fill = col_verm,
            alpha = 0.5) +
  geom_area(data = filter(buf_df, x <= -crit * 100), fill = col_verm,
            alpha = 0.5) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  geom_vline(xintercept = d_bar * 100, colour = col_verm, linewidth = 0.9) +
  annotate("text", x = d_bar * 100 + 0.04, y = max(buf_df$y) * 0.70,
           label = sprintf("Buffett:\n%.2f%% per month", d_bar * 100),
           colour = col_verm, size = 3.1, hjust = 1, vjust = 1,
           lineheight = 0.95) +
  annotate("text", x = -0.55, y = max(buf_df$y) * 0.72,
           label = 'atop("Distribution of"~bar(X), "under"~H[0])',
           parse = TRUE, size = 3.1, colour = col_blue, hjust = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Average monthly outperformance (%)", y = "Density") +
  theme_custom()

save_fig(p_buf, "p3_buffett_test", width = 4.8, height = 2.6)

# ---------- 14b/14c. The null curve, with and without the observed mean ------
# One base plot, two annotations layered on top, so the three Buffett figures
# are literally the same curve with one thing added each time:
#   p3_buffett_null  -- step 2: the curve alone. Drawing Buffett's 0.79% here
#                       would answer step 3 two frames early.
#   p3_buffett_point -- the t-statistic frame: where the 563 months landed.
#   p3_buffett_test  -- step 4: the same, with the verdict.
# The two extra labels sit in the same corner, so they are added separately
# rather than both at once -- together they overprint.

# The shaded tails are NOT in the base, because p3_buffett_test builds its own
# version of them. Steps 2 and 3 share this pair: step 3 is step 2's frame with
# the sample laid on top, so it carries the same tails.
buf_tails <- list(
  geom_area(data = filter(buf_df, x >= crit * 100), fill = col_verm,
            alpha = 0.5),
  geom_area(data = filter(buf_df, x <= -crit * 100), fill = col_verm,
            alpha = 0.5)
)

p_bufbase <- ggplot(buf_df, aes(x, y)) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  # Left of the bell, right-aligned: centred over the peak the second line
  # crosses the curve on both shoulders.
  annotate("text", x = -0.55, y = max(buf_df$y) * 0.70,
           label = 'atop("Distribution of"~bar(X), "under"~H[0])',
           parse = TRUE, size = 3.1, colour = col_blue, hjust = 1) +
  # Right edge at 1.22, not 1.05: the observed mean's two-line label is centred
  # over 0.79 and needs the room, and both figures share this frame.
  coord_cartesian(xlim = c(min(buf_df$x), 1.22)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Average monthly outperformance (%)", y = "Density") +
  theme_custom()

p_bufnull <- p_bufbase +
  buf_tails +
  annotate("segment", x = 0.95, xend = 0.60, y = 55, yend = 12,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = 1.0, y = 68, label = "almost never,\nif H0 is true",
           colour = col_verm, size = 3.0, hjust = 1, lineheight = 0.95)

save_fig(p_bufnull, "p3_buffett_null", width = 4.4, height = 2.5)

# No tails and no "almost never" callout. The callout sat in the same corner
# the observed mean now occupies; the shading is left off so this frame asks
# only "where did the sample land?" -- marking the rejection region here would
# answer the step-5 question three frames early.
p_bufpoint <- p_bufbase +
  annotate("segment", x = d_bar * 100, xend = d_bar * 100, y = 0, yend = 52,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("point", x = d_bar * 100, y = 0, colour = col_verm, size = 2.4) +
  annotate("text", x = d_bar * 100, y = 58,
           label = sprintf("Buffett:\n%.2f%% per month", d_bar * 100),
           colour = col_verm, size = 3.0, hjust = 0.5, vjust = 0,
           lineheight = 0.95)

save_fig(p_bufpoint, "p3_buffett_point", width = 4.4, height = 2.5)

# ---------- 14d. Step 4: the p-value, in Buffett's units ---------------------
# The quantification frame. Shading runs from +/- d_bar outwards, NOT from the
# 5% critical value: the question here is "how unlikely is THIS?", and the
# answer is the area at least as far from 0 as 0.790% is, in both directions.
# The verdict figure (p3_buffett_test) shades the critical value instead --
# that is the step-5 question, "is it past the line?".
#
# At 3.14 standard errors the two slivers are ~0.7% of the panel height, i.e.
# invisible. That is the point of the frame rather than a defect of it, so the
# callout names the area instead of relying on the eye to find it.
p_bufpval <- p_bufbase +
  geom_area(data = filter(buf_df, x >= d_bar * 100), fill = col_verm,
            alpha = 0.6) +
  geom_area(data = filter(buf_df, x <= -d_bar * 100), fill = col_verm,
            alpha = 0.6) +
  annotate("segment", x = d_bar * 100, xend = d_bar * 100, y = 0, yend = 42,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("segment", x = -d_bar * 100, xend = -d_bar * 100, y = 0, yend = 42,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("text", x = d_bar * 100, y = 47, label = "+0.790%",
           colour = col_verm, size = 2.8, hjust = 0.5, vjust = 0) +
  annotate("text", x = -d_bar * 100, y = 47, label = "-0.790%",
           colour = col_verm, size = 2.8, hjust = 0.5, vjust = 0) +
  annotate("segment", x = 1.12, xend = 0.88, y = 86, yend = 7,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("text", x = 1.21, y = 92,
           label = sprintf("p = %.4f:\nthe shaded area,\nboth tails", d_p),
           colour = col_verm, size = 2.9, hjust = 1, vjust = 0,
           lineheight = 0.95)

save_fig(p_bufpval, "p3_buffett_pvalue", width = 4.4, height = 2.5)

# ---------- 14e. The same picture, standardised ------------------------------
# The test-statistic frame's twin of p3_buffett_point: identical curve, identical
# observation, x axis divided through by se. That is the whole content of Z --
# it is a change of units, not a change of picture -- so the two figures are
# built to line up. buf_grid is already in return units, so dividing by d_se
# reproduces p_bufbase's frame exactly: the panel runs -4.6 to 4.84 standard
# errors, and Berkshire lands at d_t.
z_df <- tibble(x = buf_grid / d_se, y = dnorm(buf_grid / d_se))
MX_Z <- dnorm(0)

# No shaded tails, matching p3_buffett_point: this frame introduces the test
# statistic and shows where Berkshire landed on it. The rejection region is a
# step-5 idea and is drawn there, on p3_pvalue_tail and p3_buffett_test.
p_bufz <- ggplot(z_df, aes(x, y)) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  annotate("text", x = -0.55 / d_se / 100, y = MX_Z * 0.70,
           label = 'atop("Distribution of"~Z, "under"~H[0])',
           parse = TRUE, size = 3.1, colour = col_blue, hjust = 1) +
  annotate("segment", x = d_t, xend = d_t, y = 0, yend = MX_Z * 0.33,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("point", x = d_t, y = 0, colour = col_verm, size = 2.4) +
  annotate("text", x = d_t, y = MX_Z * 0.365,
           label = sprintf("Buffett:\nZ = %.2f", d_t),
           colour = col_verm, size = 3.0, hjust = 0.5, vjust = 0,
           lineheight = 0.95) +
  coord_cartesian(xlim = c(min(z_df$x), 1.22 / d_se / 100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = expression(Z == (bar(X) - mu[0]) / (sigma / sqrt(n))),
       y = "Density") +
  theme_custom()

save_fig(p_bufz, "p3_buffett_z", width = 4.4, height = 2.5)

# ---------- 14f. Both routes, one picture ------------------------------------
# The equivalence frame. The shaded region is everything past c = 1.96, which is
# the critical-value route; its AREA is 0.05, which is the p-value route. One
# region, two ways of naming it -- so "outside the shading" and "tail area below
# 0.05" cannot disagree. Berkshire sits well outside, and the frame's job is to
# show that both readings land there for the same reason, not by coincidence.
Z_CRIT <- qnorm(0.975)

p_bufequiv <- ggplot(z_df, aes(x, y)) +
  geom_area(data = filter(z_df, x >= Z_CRIT), fill = col_verm, alpha = 0.45) +
  geom_area(data = filter(z_df, x <= -Z_CRIT), fill = col_verm, alpha = 0.45) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  # The cutoff itself, in grey and dashed so it reads as a decision boundary
  # rather than as another observation.
  annotate("segment", x = -Z_CRIT, xend = -Z_CRIT, y = 0, yend = MX_Z * 0.22,
           colour = "grey35", linewidth = 0.5, linetype = 2) +
  annotate("segment", x = Z_CRIT, xend = Z_CRIT, y = 0, yend = MX_Z * 0.22,
           colour = "grey35", linewidth = 0.5, linetype = 2) +
  annotate("text", x = -Z_CRIT, y = MX_Z * 0.245, label = "-c",
           colour = "grey35", size = 2.9, hjust = 0.5, vjust = 0) +
  # Just "c", not "c = 1.96": a label that wide, centred here, has the curve's
  # shoulder running straight through its left half. The value goes in the
  # callout instead, which sits in empty space out at the panel edge.
  annotate("text", x = Z_CRIT, y = MX_Z * 0.245, label = "c",
           colour = "grey35", size = 2.9, hjust = 0.5, vjust = 0) +
  annotate("segment", x = -4.25, xend = -2.45, y = MX_Z * 0.30,
           yend = MX_Z * 0.035, colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("text", x = -4.35, y = MX_Z * 0.325,
           label = "beyond |Z| = 1.96:\narea = 0.05",
           colour = col_verm, size = 2.9, hjust = 0, vjust = 0,
           lineheight = 0.95) +
  annotate("text", x = -2.18, y = MX_Z * 0.70,
           label = 'atop("Distribution of"~Z, "under"~H[0])',
           parse = TRUE, size = 3.1, colour = col_blue, hjust = 1) +
  annotate("segment", x = d_t, xend = d_t, y = 0, yend = MX_Z * 0.33,
           colour = col_verm, linewidth = 0.5, linetype = 3) +
  annotate("point", x = d_t, y = 0, colour = col_verm, size = 2.4) +
  annotate("text", x = d_t, y = MX_Z * 0.365,
           label = sprintf("Buffett:\nZ = %.2f", d_t),
           colour = col_verm, size = 3.0, hjust = 0.5, vjust = 0,
           lineheight = 0.95) +
  coord_cartesian(xlim = c(min(z_df$x), 1.22 / d_se / 100)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = expression(Z == (bar(X) - mu[0]) / (sigma / sqrt(n))),
       y = "Density") +
  theme_custom()

save_fig(p_bufequiv, "p3_buffett_equiv", width = 4.6, height = 2.6)

# =============================================================================
# 15. The two errors, and why you cannot shrink both
# =============================================================================

ERR_SHIFT <- 3.0     # the alternative, in standard-error units
err_grid <- seq(-4, 7, length.out = 1400)
z_crit <- qnorm(0.975)

err_df <- bind_rows(
  tibble(x = err_grid, y = dnorm(err_grid), which = "H0 true"),
  tibble(x = err_grid, y = dnorm(err_grid, ERR_SHIFT), which = "H1 true")
)

p_err <- ggplot(err_df, aes(x, y)) +
  geom_area(data = tibble(x = err_grid[err_grid >= z_crit]) |>
              mutate(y = dnorm(x)), fill = col_verm, alpha = 0.55) +
  geom_area(data = tibble(x = err_grid[err_grid <= z_crit]) |>
              mutate(y = dnorm(x, ERR_SHIFT)), fill = col_purple, alpha = 0.45) +
  geom_line(aes(colour = which), linewidth = 0.8) +
  geom_vline(xintercept = z_crit, colour = "black", linewidth = 0.5,
             linetype = 2) +
  annotate("text", x = z_crit + 0.12, y = 0.485,
           label = 'reject~H[0]~"to the right"', parse = TRUE, size = 3.0,
           hjust = 0, vjust = 1) +
  annotate("segment", x = 3.95, xend = 2.32, y = 0.075, yend = 0.014,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = 4.05, y = 0.082, label = "Type I", colour = col_verm,
           size = 3.2, hjust = 0) +
  annotate("segment", x = 0.05, xend = 1.30, y = 0.115, yend = 0.038,
           colour = col_purple, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = -0.05, y = 0.122, label = "Type II", colour = col_purple,
           size = 3.2, hjust = 1) +
  scale_colour_manual(values = c("H0 true" = col_blue, "H1 true" = col_green)) +
  scale_y_continuous(limits = c(0, 0.50), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(xlim = c(-3.4, 7.4)) +
  labs(x = "Test statistic", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.88),
        legend.key.width = unit(0.6, "cm"),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_err, "p3_error_tradeoff", width = 4.8, height = 2.6)

# =============================================================================
# 15b. The rejection region on its own
# =============================================================================
# Section 15 without the alternative: one curve (the world if H_0 is true) and
# the shaded tail. Used on "How Do We 'Reject' a Null?", which comes long before
# Type II errors exist in the deck -- drawing the H_1 curve there would raise a
# question the frame has no way to answer yet. Same grid, same cutoff and same
# fill as section 15 so the two pictures line up when the later frame adds the
# second curve back.

p_rej <- ggplot(tibble(x = err_grid, y = dnorm(err_grid)), aes(x, y)) +
  geom_area(data = tibble(x = err_grid[err_grid >= z_crit]) |>
              mutate(y = dnorm(x)), fill = col_verm, alpha = 0.55) +
  geom_line(colour = col_blue, linewidth = 0.9) +
  geom_vline(xintercept = z_crit, colour = "black", linewidth = 0.5,
             linetype = 2) +
  # Plain text, not plotmath: plotmath has no clean multi-line form, and
  # parse = FALSE on a plotmath string prints the source verbatim.
  # Short enough to sit inside the bell without the second line crossing the
  # curve: at y = 0.28 the curve is only 1.05 sd wide.
  annotate("text", x = -0.02, y = 0.275,
           label = "if H0 is true,\nX-bar lands in here",
           size = 3.0, hjust = 0.5, colour = col_blue, lineheight = 0.95) +
  annotate("segment", x = 3.6, xend = 2.45, y = 0.105, yend = 0.020,
           colour = col_verm, linewidth = 0.4,
           arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = 3.7, y = 0.128,
           label = "almost never,\nif H0 is true", colour = col_verm,
           size = 3.0, hjust = 1, lineheight = 0.95) +
  scale_y_continuous(limits = c(0, 0.44), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(xlim = c(-3.4, 4.2)) +
  # No x title: the frame states the axis in words, and dropping it buys the
  # height back for the curve.
  labs(x = NULL, y = "Density") +
  theme_custom()

save_fig(p_rej, "p3_rejection_region", width = 4.0, height = 2.2)

# =============================================================================
# 16. Multiple testing: 100 strategies that are pure noise
# =============================================================================
# Every "strategy" here is a coin flip by construction, so the correct answer is
# that none of them works. About five clear the 5% bar anyway.
#
# The count is itself random: across seeds it runs from 0 to 8, and a draw where
# nothing clears the bar tells the student the opposite of the intended lesson.
# The offset below is chosen to land on the expected count of five.

N_STRAT <- 100
N_MONTHS <- 240

set.seed(BASE_SEED + 18)         # 100 pure-noise strategies -- see note above
strat_t <- vapply(seq_len(N_STRAT), function(k) {
  r <- rnorm(N_MONTHS, mean = 0, sd = 0.05)
  mean(r) / (sd(r) / sqrt(N_MONTHS))
}, numeric(1))

strat_df <- tibble(id = seq_len(N_STRAT), t = strat_t) |>
  mutate(sig = abs(t) > qt(0.975, N_MONTHS - 1))

n_sig <- sum(strat_df$sig)

p_mt <- ggplot(strat_df, aes(id, t, colour = sig)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_hline(yintercept = c(-1, 1) * qt(0.975, N_MONTHS - 1),
             colour = col_verm, linewidth = 0.5, linetype = 2) +
  geom_point(size = 1.6) +
  annotate("text", x = 1, y = 2.55, label = "reject H0 outside these lines",
           colour = col_verm, size = 3.0, hjust = 0) +
  scale_colour_manual(values = c(`TRUE` = col_verm, `FALSE` = col_grey),
                      guide = "none") +
  scale_y_continuous(limits = c(-3.2, 3.2)) +
  labs(x = "Strategy (all 100 are pure noise)", y = "t statistic") +
  theme_custom()

save_fig(p_mt, "p3_multiple_testing", width = 5.0, height = 2.4)

# =============================================================================
# 17. What "95% confident" means
# =============================================================================

N_CI <- 100

set.seed(BASE_SEED + 16)         # 100 confidence intervals
ci_sim <- lapply(seq_len(N_CI), function(k) {
  x <- rnorm(N_SAMP, POP_MU, POP_SD)
  se <- sd(x) / sqrt(N_SAMP)
  half <- qt(0.975, N_SAMP - 1) * se
  tibble(id = k, xbar = mean(x), lo = mean(x) - half, hi = mean(x) + half)
}) |>
  bind_rows() |>
  mutate(covers = lo <= POP_MU & POP_MU <= hi)

n_miss <- sum(!ci_sim$covers)

p_ci <- ggplot(ci_sim, aes(y = id, colour = covers)) +
  geom_vline(xintercept = POP_MU, colour = "black", linewidth = 0.6) +
  geom_segment(aes(x = lo, xend = hi, yend = id), linewidth = 0.4) +
  geom_point(aes(x = xbar), size = 0.5) +
  annotate("text", x = POP_MU + 0.08, y = N_CI + 2, label = 'mu', parse = TRUE,
           size = 3.4, hjust = 0) +
  scale_colour_manual(values = c(`TRUE` = col_grey, `FALSE` = col_verm),
                      guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.06))) +
  labs(x = "95% confidence interval for the mean monthly return (%)",
       y = "Sample") +
  theme_custom() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

save_fig(p_ci, "p3_ci_coverage", width = 4.6, height = 2.9)

# =============================================================================
# 17b. Thirty years of X_t, one hundred times
# =============================================================================
# The question a track record actually poses. Draw THIRTY YEARS of Berkshire's
# monthly outperformance, build the 95% interval with c = 1.96, and see whether
# it clears zero. Bootstrapped from the real 563 months rather than simulated
# from a normal, so the fat tails come along for the ride.
#
# Highlighted are the intervals that CONTAIN zero -- the samples in which the
# very same edge, tested the very same way, does not get called skill. The full
# 563-month mean sits at d_bar and is drawn for reference: the edge is real and
# constant here by construction, so every wide interval is sampling noise, not
# a change in Buffett.

N_TENYR <- 100
# One edit changes the horizon: the axis label and the console line both read
# YEARS_SIM, so they cannot drift from the data.
YEARS_SIM <- 30
MONTHS_SIM <- YEARS_SIM * 12

set.seed(BASE_SEED + 17)
ten_sim <- lapply(seq_len(N_TENYR), function(k) {
  x <- sample(buf$outperf, MONTHS_SIM, replace = TRUE)
  half <- 1.96 * sd(x) / sqrt(MONTHS_SIM)
  tibble(id = k,
         xbar = mean(x) * 100,
         lo = (mean(x) - half) * 100,
         hi = (mean(x) + half) * 100)
}) |>
  bind_rows() |>
  mutate(includes0 = lo <= 0 & 0 <= hi)

n_incl0 <- sum(ten_sim$includes0)

p_ten <- ggplot(ten_sim, aes(y = id, colour = includes0)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.6) +
  geom_vline(xintercept = d_bar * 100, colour = col_blue, linewidth = 0.5,
             linetype = 2) +
  geom_segment(aes(x = lo, xend = hi, yend = id), linewidth = 0.4) +
  geom_point(aes(x = xbar), size = 0.5) +
  annotate("text", x = 0 - 0.06, y = N_TENYR + 2, label = "0",
           size = 3.0, hjust = 1) +
  annotate("text", x = d_bar * 100 + 0.06, y = N_TENYR + 2,
           label = "full-sample mean", colour = col_blue, size = 2.8,
           hjust = 0) +
  scale_colour_manual(values = c(`TRUE` = col_verm, `FALSE` = col_grey),
                      guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = sprintf("95%% CI for average monthly outperformance (%%), %d-year samples",
                   YEARS_SIM),
       y = "Sample") +
  theme_custom() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

save_fig(p_ten, "p3_ci_tenyear", width = 4.6, height = 2.9)

# =============================================================================
# Numbers quoted on the slides — printed so the .tex can be checked against them
# =============================================================================

cat("\nNumbers used in the deck:\n")

cat(sprintf("  Population used for the estimator figures: mu = %.2f%%, sigma = %.2f%% per month\n",
            POP_MU, POP_SD))
cat(sprintf("  Illustration samples (n = %d): Xbar = %s\n", N_ILLUS,
            paste(sprintf("%.2f", sapply(samps, mean)), collapse = ", ")))
cat(sprintf("  One sample of n = %d: Xbar = %.2f%%\n", N_ILLUS, mean(samp1)))
cat("  Three samples: ")
cat(paste(sprintf("%.2f%%", three_means$xbar), collapse = ", "), "\n")
cat(sprintf("  se(Xbar) at n = %d: %.3f%%   (sigma/sqrt(n))\n",
            N_SAMP, POP_SD / sqrt(N_SAMP)))
cat(sprintf("  sd of the simulated sampling distribution at n = %d: %.3f%%\n",
            N_ILLUS, sd(xbar_25)))
cat(sprintf("  sd of the simulated sampling distribution at n = %d: %.3f%%\n",
            N_SAMP, sd(xbar_60)))
cat(sprintf("  Efficiency: sd(median)/sd(mean) = %.3f   (theory sqrt(pi/2) = %.3f)\n",
            sd(med_60) / sd(xbar_60), sqrt(pi / 2)))
cat(sprintf("  se at n = 100 / 400 / 1600: %.2f / %.2f / %.2f\n",
            POP_SD / 10, POP_SD / 20, POP_SD / 40))

cat("\n  Buffett vs the market (monthly outperformance, %s to %s):\n" |>
      sprintf(format(min(buf$eom)), format(max(buf$eom))))
cat(sprintf("    n = %d months\n", n_buf))
cat(sprintf("    Xbar   = %.4f%% per month  = %.2f%% per year\n",
            d_bar * 100, d_bar * MONTHS_PER_YEAR * 100))
cat(sprintf("    s      = %.4f%% per month\n", d_sd * 100))
cat(sprintf("    se     = %.4f%% per month\n", d_se * 100))
cat(sprintf("    t      = %.3f\n", d_t))
cat(sprintf("    p      = %.5f\n", d_p))
cat(sprintf("    95%% CI = [%.4f%%, %.4f%%] per month = [%.2f%%, %.2f%%] per year\n",
            d_ci[1] * 100, d_ci[2] * 100,
            d_ci[1] * MONTHS_PER_YEAR * 100, d_ci[2] * MONTHS_PER_YEAR * 100))
cat(sprintf("    Berkshire total: %.2f%%/yr, market %.2f%%/yr\n",
            mean(buf$buffett_exc + buf$rf) * MONTHS_PER_YEAR * 100,
            mean(buf$mkt_exc + buf$rf) * MONTHS_PER_YEAR * 100))
cat(sprintf("    $1 grows to $%s (Berkshire) vs $%s (market)\n",
            format(round(qotd_end$value[qotd_end$series == "Berkshire"]),
                   big.mark = ","),
            format(round(qotd_end$value[qotd_end$series == "US market"]),
                   big.mark = ",")))

cat(sprintf("\n  Equity premium (stocks over T-bills), n = %d months:\n", n_ep))
cat(sprintf("    Xbar = %.4f%%/mo = %.2f%%/yr, se = %.4f%%/mo\n",
            ep_bar * 100, ep_bar * MONTHS_PER_YEAR * 100, ep_se * 100))
cat(sprintf("    95%% CI = [%.2f%%, %.2f%%] per year  (width %.2f pp)\n",
            ep_ci[1] * MONTHS_PER_YEAR * 100, ep_ci[2] * MONTHS_PER_YEAR * 100,
            (ep_ci[2] - ep_ci[1]) * MONTHS_PER_YEAR * 100))

cat(sprintf("\n  p-value figure: Z = %.2f, p = %.4f\n",
            Z_SHOWN, d_p))
# The "how big is c?" anchor on the Step 5 rejection-rule frame. Filtered on the
# stock column ONLY -- sb additionally drops months with no 10-year yield, which
# would change the mean and sd this z is measured against.
gd <- read_csv(STOCKS_BONDS_CSV,
               col_types = cols(eom = col_date(), .default = col_double()),
               progress = FALSE) |>
  filter(!is.na(us_stock_market))
gd_mu <- mean(gd$us_stock_market)
gd_sd <- sd(gd$us_stock_market)
gd_worst <- gd |> slice_min(us_stock_market, n = 1)
gd_z <- (gd_worst$us_stock_market - gd_mu) / gd_sd
gd_p <- 2 * pnorm(-abs(gd_z))

cat(sprintf("\n  Great Depression anchor: %s, return %.1f%%, z = %.2f\n",
            format(gd_worst$eom, "%B %Y"), gd_worst$us_stock_market * 100, gd_z))
cat(sprintf("    sample %s-%s, n = %d, mean %.2f%%/mo, sd %.2f%%/mo\n",
            format(min(gd$eom), "%Y"), format(max(gd$eom), "%Y"), nrow(gd),
            gd_mu * 100, gd_sd * 100))
cat(sprintf("    normal two-sided tail beyond |z|: %.1e, about 1 in %.1f million years\n",
            gd_p, 1 / gd_p / 12 / 1e6))
cat(sprintf("  Multiple testing: %d of %d pure-noise strategies are significant at 5%%\n",
            n_sig, N_STRAT))
cat(sprintf("  Confidence intervals: %d of %d miss mu\n", n_miss, N_CI))
cat(sprintf("  %d-year samples: %d of %d intervals contain 0 (fail to detect the edge)\n",
            YEARS_SIM, n_incl0, N_TENYR))
cat(sprintf("  CLT population: mean %.3f, sd %.3f, skewness %+.2f\n",
            ugly_mu, ugly_sd,
            {u <- r_ugly(2e5); mean((u - mean(u))^3) / sd(u)^3}))

cat("\nAll figures written to", FIG_DIR, "\n")
