# =============================================================================
# Part 1: Random Variables and Distributions — figure generation
# =============================================================================
# Companion script for drafts/slides/part1_random_variables.tex
#
# Generates every data figure used in the Part 1 deck. Conceptual diagrams
# (outcome-space mapping, generic shaded density) are drawn in TikZ inside the
# .tex file and are deliberately NOT produced here.
#
# Run from the project root:
#   Rscript scripts/R/part1_figures.R
#
# Data sources:
#   Shiller ie_data.xls, sheet "Data", REAL S&P price (column 8), monthly
#     1871-2026  (https://shillerdata.com -- see data/raw/shiller/README.md)
#   US stock market, monthly 1926-2023 (stocks_bonds.csv) -- supplies theta,
#     the share of months that are up months
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source("scripts/R/theme.R")

set.seed(20260812)

# ---------- Paths and shared settings ----------------------------------------

DATA_DIR <- "data/raw"
SHILLER_XLS <- file.path(DATA_DIR, "shiller", "ie_data.xls")
STOCKS_BONDS_CSV <- file.path(DATA_DIR, "stocks_bonds.csv")
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

# Named palette shortcuts
col_blue <- OKABE_ITO[["blue"]]
col_orange <- OKABE_ITO[["orange"]]
col_verm <- OKABE_ITO[["vermillion"]]
col_green <- OKABE_ITO[["green"]]
col_sky <- OKABE_ITO[["sky_blue"]]
col_grey <- "#BFBFBF"

# =============================================================================
# Data
# =============================================================================

cat("Reading data ...\n")

# --- Shiller monthly REAL S&P price ------------------------------------------
# ie_data.xls, sheet "Data": rows 1-7 are title and multi-line headers, so the
# data starts on row 8. Column 1 is the date, column 8 is the CPI-deflated
# "Real Price". Column 2 is the NOMINAL price -- a different series; see
# data/raw/shiller/README.md for the full column map.
#
# NOTE on the date: it is stored as a NUMBER, so October 1871 arrives as 1871.1
# and recent rows carry float noise (2026.0599999999999). Recover the month by
# rounding the fractional part, never by slicing the string -- string-slicing
# collapses October to month 1.
shiller_raw <- read_excel(
  SHILLER_XLS,
  sheet        = "Data",
  skip         = 7,          # rows 1-7 are title and header
  col_names    = FALSE,
  .name_repair = "unique_quiet"
)

monthly <- tibble(
  date_num = suppressWarnings(as.numeric(shiller_raw[[1]])),
  price    = suppressWarnings(as.numeric(shiller_raw[[8]]))   # col 8 = Real Price
) |>
  # Drops the trailing note rows below the data as well as any blank row.
  filter(!is.na(date_num), !is.na(price)) |>
  mutate(
    year  = as.integer(floor(date_num)),
    month = as.integer(round((date_num - year) * 100)),
    date  = sprintf("%d.%02d", year, month),
    ret   = price / lag(price) - 1
  ) |>
  filter(!is.na(ret))

stopifnot(
  all(monthly$month %in% 1:12),
  !is.unsorted(monthly$year)
)

mu_m <- mean(monthly$ret)
sd_m <- sd(monthly$ret)

# --- US monthly market return -------------------------------------------------
# theta is the share of UP MONTHS. Was the share of up DAYS from a daily file
# that is no longer a project dependency; the switch moves theta from 0.55 to
# 0.63, which moves the Binomial(20, theta) mode from 11 to 13. The slides quote
# the monthly numbers.
sb <- read_csv(
  STOCKS_BONDS_CSV,
  col_types = cols(eom = col_date(), .default = col_double()),
  progress  = FALSE
) |>
  filter(!is.na(us_stock_market)) |>
  arrange(eom)

theta <- mean(sb$us_stock_market > 0)

cat(sprintf("  monthly: n=%d, mean=%.4f, sd=%.4f\n", nrow(monthly), mu_m, sd_m))
cat(sprintf("  stocks:  n=%d, P(up month)=%.4f\n", nrow(sb), theta))

# =============================================================================
# 1. Motivating hook: 155 years of real S&P monthly returns
# =============================================================================

monthly_ts <- monthly |>
  mutate(t = year + (month - 1) / 12)

p_ts <- ggplot(monthly_ts, aes(x = t, y = ret)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(colour = col_blue, linewidth = 0.25) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = NULL, y = "Monthly return") +
  theme_custom()

save_fig(p_ts, "p1_sp500_timeseries", height = 2.6)

# =============================================================================
# 1b. Real returns vs. a simulated normal series (opening hook, side by side)
# =============================================================================
# The simulated series is drawn straight from Normal(mu_m, sd_m) -- the SAME
# mean and variance as the actual monthly returns. Matching the first two
# moments is the whole setup for the deck: the two series agree on centre and
# spread by construction, so anything the class can still see between them is
# a difference in SHAPE, which is what the fat-tails section goes on to name.
# Both panels are drawn on identical axes so neither looks calmer merely
# because it was rescaled.

sim_ts <- tibble(
  t   = monthly_ts$t,
  ret = rnorm(nrow(monthly_ts), mean = mu_m, sd = sd_m)   # sd, NOT variance
)

cat(sprintf("  simulated normal: mean=%.4f%%, sd=%.4f%% (target %.4f%%, %.4f%%)\n",
            mean(sim_ts$ret) * 100, sd(sim_ts$ret) * 100, mu_m * 100, sd_m * 100))

# Shared limits, so neither panel looks calmer merely because it is rescaled.
ret_lim <- range(c(monthly_ts$ret, sim_ts$ret))

#' Return-series panel used by the opening hook (identical scales, own title).
#' Sized for a half-width Beamer column, so the two panels sit side by side.
ts_panel <- function(df, title, subtitle, colour) {
  ggplot(df, aes(x = t, y = ret)) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
    geom_line(colour = colour, linewidth = 0.2) +
    scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    coord_cartesian(ylim = ret_lim) +
    # Short axis title: a long rotated label would reach the panel top and
    # collide with the plot title, which is left-aligned to the device.
    labs(x = NULL, y = "Return", title = title, subtitle = subtitle) +
    theme_custom() +
    # Titles span the whole device, not just the panel, so they have the full
    # half-column width to sit on one line.
    theme(
      plot.title          = element_text(size = 9.5, face = "bold", hjust = 0,
                                         margin = margin(b = 1)),
      plot.subtitle       = element_text(size = 7.5, colour = "#4D4D4D",
                                         hjust = 0, margin = margin(b = 4)),
      plot.title.position = "plot"
    )
}

PANEL_W <- 2.72   # 0.49 \\textwidth of a 16:9 frame at 9pt
PANEL_H <- 1.95   # leaves room for the question box below the two panels

# Blind pair: the deck shows these first and asks the class to guess which is
# which. The blank subtitle is deliberate -- it reserves exactly the space the
# revealed subtitle occupies, so the panels do not shift between the two frames.
save_fig(
  ts_panel(monthly_ts, "A", " ", col_blue),
  "p1_returns_a_blind", width = PANEL_W, height = PANEL_H
)

save_fig(
  ts_panel(sim_ts, "B", " ", col_orange),
  "p1_returns_b_blind", width = PANEL_W, height = PANEL_H
)

# Revealed pair: same panels, same order, labels on.
save_fig(
  ts_panel(monthly_ts, "A: Actual", "Real S&P price, 1871-2026", col_blue),
  "p1_returns_a_revealed", width = PANEL_W, height = PANEL_H
)

save_fig(
  ts_panel(sim_ts, "B: Simulated",
           "Normal draws, same mean and SD", col_orange),
  "p1_returns_b_revealed", width = PANEL_W, height = PANEL_H
)

# =============================================================================
# 1c. Outline sidebar: the series, and the same series as a distribution
# =============================================================================
# Two views of one dataset, stacked in the outline slide's right column: time on
# the x-axis above, return on the x-axis below. The kernel density uses the same
# adjust = 1.2 as every other smooth density in the deck, so the curve students
# see here is the curve they meet again in the pdf section.

OUT_W <- 2.45
OUT_H <- 1.30

#' Type scale for the small outline panels, which render at ~2.45in wide.
#' theme_custom() sizes assume a half- or full-width figure and read as
#' oversized at this scale.
theme_outline <- function() {
  theme_custom() +
    theme(
      plot.title          = element_text(size = 8, face = "bold", hjust = 0,
                                         margin = margin(b = 2)),
      axis.title          = element_text(size = 7.5, colour = black),
      axis.text           = element_text(size = 7, colour = black),
      plot.title.position = "plot",
      plot.margin         = margin(t = 2, r = 5, b = 2, l = 2)
    )
}

# Generic pmf for the "Discrete Random Variables" frame: the same
# Binomial(4, 0.55) heights drawn in TikZ on "Two Kinds of Random Variables",
# now with the probabilities printed so the properties box beside it can be read
# straight off the picture -- each height in [0, 1], and the five summing to 1.
pmf_generic <- tibble(x = 0:4, p = dbinom(0:4, size = 4, prob = 0.55))

p_pmf_generic <- ggplot(pmf_generic, aes(x = factor(x), y = p)) +
  geom_col(fill = col_blue, width = 0.55) +
  geom_text(aes(label = sprintf("%.2f", p)), vjust = -0.45, size = 2.3) +
  scale_y_continuous(limits = c(0, 0.46), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "f(x)") +
  theme_outline()

save_fig(p_pmf_generic, "p1_pmf_generic", width = 2.6, height = 1.05)

p_out_ts <- ggplot(monthly_ts, aes(x = t, y = ret)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(colour = col_blue, linewidth = 0.18) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = NULL, y = "Return", title = "Real S&P monthly returns, 1871-2026") +
  theme_outline()

save_fig(p_out_ts, "p1_outline_ts", width = OUT_W, height = OUT_H)

kde <- density(monthly$ret, adjust = 1.2, n = 2048)
kde_df <- tibble(x = kde$x, y = kde$y)

p_out_kde <- ggplot(monthly, aes(x = ret)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.01,
                 fill = col_grey, colour = NA) +
  # Blue throughout the deck for the empirical density of the actual returns,
  # matching the colour of the actual return series itself.
  geom_line(data = kde_df, aes(x = x, y = y), inherit.aes = FALSE,
            colour = col_blue, linewidth = 0.6) +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  scale_x_continuous(breaks = c(-0.2, 0, 0.2),
                     labels = c("-20%", "0%", "20%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Density",
       title = "The same returns, kernel-smoothed") +
  theme_outline()

save_fig(p_out_kde, "p1_outline_kde", width = OUT_W, height = OUT_H)

# =============================================================================
# 1d. The series, the model density, and the empirical density
# =============================================================================
# Three panels on one frame: the actual return series on the left, and on the
# right the two densities stacked -- the normal the simulation draws from above,
# the kernel estimate of the actual returns below. The two densities share x and
# y limits, which is the whole point: matched on mean and sd, the normal is
# still visibly too short in the middle and too thin in the tails.

DIST_W  <- 2.72
DIST_H  <- 1.25   # each density panel; the left series panel is twice this
DENS_X  <- c(-0.25, 0.25)
DENS_Y  <- c(0, 13)

# The distribution the simulated series is drawn from: Normal(mu_m, sd_m), the
# same mean and sd as the actual returns. Named `sim_norm_df` because `norm_df`
# below (section 11) is a different grid serving the tail-probability figure.
r_grid <- seq(-0.30, 0.30, length.out = 1200)
sim_norm_df <- tibble(
  x = r_grid,
  y = dnorm(r_grid, mean = mu_m, sd = sd_m)   # density, not cdf
)

#' Density panel for the three-graph frame. Shared limits across panels.
dens_panel <- function(df, title, colour) {
  ggplot(df, aes(x = x, y = y)) +
    geom_line(colour = colour, linewidth = 0.7) +
    coord_cartesian(xlim = DENS_X, ylim = DENS_Y) +
    scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                       labels = c("-20%", "-10%", "0%", "10%", "20%")) +
    labs(x = NULL, y = "Density", title = title) +
    theme_outline()
}

p_dist_ts <- ggplot(monthly_ts, aes(x = t, y = ret)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_line(colour = col_blue, linewidth = 0.2) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = NULL, y = "Return",
       title = "Actual: real S&P monthly returns, 1871-2026") +
  theme_outline()

save_fig(p_dist_ts, "p1_dist_actual_ts", width = DIST_W, height = 2 * DIST_H)

#' Density panel built to sit BESIDE a ts_panel, not in the outline sidebar:
#' same theme, same title/subtitle treatment, same device size. The third
#' "Random processes" frame puts this next to p1_returns_a_revealed, and the
#' sidebar styling (theme_outline, smaller type, different device) read as a
#' figure borrowed from another deck.
dens_panel_matched <- function(df, title, subtitle, colour) {
  ggplot(df, aes(x = x, y = y)) +
    geom_line(colour = colour, linewidth = 0.7) +
    coord_cartesian(xlim = DENS_X, ylim = DENS_Y) +
    scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                       labels = c("-20%", "-10%", "0%", "10%", "20%")) +
    labs(x = NULL, y = "Density", title = title, subtitle = subtitle) +
    theme_custom() +
    theme(
      plot.title          = element_text(size = 9.5, face = "bold", hjust = 0,
                                         margin = margin(b = 1)),
      plot.subtitle       = element_text(size = 7.5, colour = "#4D4D4D",
                                         hjust = 0, margin = margin(b = 4)),
      plot.title.position = "plot"
    )
}

# Titled "B: Simulated" to keep the A/B pairing the two previous frames set up:
# the class guessed between A and B, and this frame shows A's series next to the
# distribution B was drawn from.
save_fig(
  dens_panel_matched(sim_norm_df, "B: Simulated",
                     "The normal it is drawn from", col_orange),
  "p1_dist_normal", width = PANEL_W, height = PANEL_H
)

# Kernel estimate only -- no histogram bars behind it.
save_fig(
  dens_panel(kde_df, "Actual: kernel-smoothed density", col_blue),
  "p1_dist_kde", width = DIST_W, height = DIST_H
)

# =============================================================================
# 2. Free-throw pmf (Wooldridge, Figure B.1)
# =============================================================================

ft <- tibble(x = factor(0:2), p = c(.20, .44, .36))

p_ft <- ggplot(ft, aes(x = x, y = p)) +
  geom_col(fill = col_blue, width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", p)), vjust = -0.6, size = 3.4) +
  scale_y_continuous(limits = c(0, 0.55), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "x  (free throws made out of 2)", y = "f(x)") +
  theme_custom()

save_fig(p_ft, "p1_freethrow_pmf", width = 3.6, height = 2.7)

# =============================================================================
# 3. Bernoulli pmf at the real up-month probability
# =============================================================================

bern <- tibble(
  x = factor(c(0, 1), labels = c("0  (down)", "1  (up)")),
  p = c(1 - theta, theta)
)

p_bern <- ggplot(bern, aes(x = x, y = p, fill = x)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", p)), vjust = -0.6, size = 3.6) +
  scale_fill_manual(values = c(col_verm, col_green)) +
  scale_y_continuous(limits = c(0, 0.72), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "x", y = "f(x)") +
  theme_custom()

save_fig(p_bern, "p1_bernoulli_pmf", width = 3.6, height = 2.7)

# =============================================================================
# 5-6. Binomial(20, theta): plain, and with the X >= 14 tail highlighted
# =============================================================================

binom20 <- tibble(x = 0:20, p = dbinom(0:20, size = 20, prob = theta))

p_binom <- ggplot(binom20, aes(x = x, y = p)) +
  geom_col(fill = col_blue, width = 0.7) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "x  (up months out of 20)", y = "f(x)") +
  theme_custom()

save_fig(p_binom, "p1_binomial20", width = 5.0, height = 2.7)

tail_prob <- 1 - pbinom(13, 20, theta)

p_binom_tail <- ggplot(binom20, aes(x = x, y = p, fill = x >= 14)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  annotate("text", x = 17.2, y = max(binom20$p) * 0.72,
           label = sprintf("P(X >= 14) = %.3f", tail_prob),
           colour = col_verm, size = 3.4, hjust = 0.5) +
  annotate("segment", x = 17.2, xend = 15.4,
           y = max(binom20$p) * 0.65, yend = max(binom20$p) * 0.30,
           colour = col_verm, linewidth = 0.4,
           arrow = grid::arrow(length = unit(0.15, "cm"))) +
  scale_fill_manual(values = c(`FALSE` = col_grey, `TRUE` = col_verm)) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "x  (up months out of 20)", y = "f(x)") +
  theme_custom()

save_fig(p_binom_tail, "p1_binomial20_tail", width = 5.0, height = 2.7)

# =============================================================================
# 6b. Poisson pmf (appendix slide)
# =============================================================================
# Counts on an unbounded support: trades executed in a minute. lambda = 4 keeps
# the mass legible at slide size; the point is the shape and the open right
# tail, not a calibrated trading intensity.

LAMBDA <- 4

pois <- tibble(x = 0:14, p = dpois(0:14, lambda = LAMBDA))

p_pois <- ggplot(pois, aes(x = x, y = p)) +
  geom_col(fill = col_blue, width = 0.7) +
  scale_x_continuous(breaks = seq(0, 14, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "x  (trades executed in a minute)", y = "f(x)") +
  theme_custom()

save_fig(p_pois, "p1_poisson_pmf", width = 5.0, height = 2.7)

# =============================================================================
# 7. Effect of n: Binomial(n, theta) for n = 5, 20, 100
# =============================================================================

grid_n <- bind_rows(lapply(c(5, 20, 100), function(nn) {
  tibble(n = nn, x = 0:nn, p = dbinom(0:nn, nn, theta))
})) |>
  mutate(
    share = x / n,
    width = 0.9 / n,
    panel = factor(sprintf("n = %d", n),
                   levels = sprintf("n = %d", c(5, 20, 100)))
  )

p_grid <- ggplot(grid_n, aes(x = share, y = p, width = width)) +
  geom_col(fill = col_blue) +
  facet_wrap(~ panel, scales = "free_y", nrow = 1) +
  coord_cartesian(xlim = c(-0.10, 1.10)) +
  scale_x_continuous(breaks = c(0, 0.5, 1),
                     labels = c("0%", "50%", "100%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Share of months that are up months", y = "f(x)") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, face = "bold"))

save_fig(p_grid, "p1_binomial_n_grid", width = 5.4, height = 2.4)

# =============================================================================
# 8. From histogram to density: same data, finer and finer bins
# =============================================================================

# ggplot cannot vary binwidth across facets, so bin explicitly and stack.
hist_df <- bind_rows(lapply(c(0.05, 0.02, 0.005), function(bw) {
  brks <- seq(floor(min(monthly$ret) / bw) * bw,
              ceiling(max(monthly$ret) / bw) * bw, by = bw)
  h <- hist(monthly$ret, breaks = brks, plot = FALSE)
  tibble(bw = bw, mid = h$mids, dens = h$density, width = bw)
})) |>
  mutate(panel = factor(sprintf("bin width = %g%%", bw * 100),
                        levels = sprintf("bin width = %g%%",
                                         c(0.05, 0.02, 0.005) * 100)))

# The limiting smooth curve, shown only in the finest panel
limit_df <- tibble(
  x     = seq(-0.25, 0.25, length.out = 600),
  y     = approx(density(monthly$ret, adjust = 1.2, n = 2048),
                 xout = seq(-0.25, 0.25, length.out = 600))$y,
  panel = factor("bin width = 0.5%", levels = levels(hist_df$panel))
)

p_bins <- ggplot(hist_df, aes(x = mid, y = dens, width = width)) +
  geom_col(fill = col_sky, colour = NA) +
  geom_line(data = limit_df, aes(x = x, y = y), inherit.aes = FALSE,
            colour = col_verm, linewidth = 0.6) +
  facet_wrap(~ panel, nrow = 1) +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  scale_x_continuous(breaks = c(-0.2, 0, 0.2),
                     labels = c("-20%", "0%", "20%")) +
  labs(x = "Monthly return", y = "Density") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, face = "bold"))

save_fig(p_bins, "p1_hist_bins", width = 5.4, height = 2.4)

# =============================================================================
# 9. Probability as area: P(r < 0), the whole left tail, under the fitted density
# =============================================================================
# The slide asks "how often does the S&P have a losing month?", which is a
# frequency question, so the printed label is the empirical share. It agrees
# with the area under the shaded curve to 0.002 (0.3923 vs 0.3941, the latter
# integrated in section 10); the two are different quantities, so keep an eye on
# them if the bandwidth ever changes. Both round to 0.39.
# About 0.001 of the density's mass sits left of the -0.22 plot limit and is
# therefore clipped out of the drawn region.

dens <- density(monthly$ret, adjust = 1.2, n = 2048)
dens_df <- tibble(x = dens$x, y = dens$y)

shade <- dens_df |> filter(x <= 0)
area_emp <- mean(monthly$ret < 0)

p_area <- ggplot(dens_df, aes(x, y)) +
  geom_area(data = shade, fill = col_orange, alpha = 0.55) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  annotate("text", x = -0.088, y = 3.2,
           label = sprintf("area = %.2f", area_emp), size = 3.4) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 12)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = "Monthly return", y = "f(r)") +
  theme_custom()

save_fig(p_area, "p1_density_interval", width = 4.6, height = 2.7)

# Unshaded twin of the figure above. The slide puts this one up with the
# question and swaps to the shaded version with the answer, so the two must
# share every axis setting -- otherwise the plot jumps between overlays.
p_area_plain <- ggplot(dens_df, aes(x, y)) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 12)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = "Monthly return", y = "f(r)") +
  theme_custom()

save_fig(p_area_plain, "p1_density_plain", width = 4.6, height = 2.7)

# =============================================================================
# 10. Linked pdf and cdf, both from the empirical density, evaluated at c = 0
# =============================================================================
# Same curve as section 9, deliberately: the left panel IS the picture from
# "An Area on Real Data", so the shaded area students just saw (0.39) is the
# height they now read off the cdf. The cdf is derived from that density by
# numerical integration, not from a fitted parametric model --
#   F(x) = cumsum(f * dx)
# on density()'s own uniform grid. Dividing by the final value corrects the
# Riemann-sum error only; the raw total is already 1.0000 to four decimals.

cval <- 0

# Trapezoidal rule: a plain cumsum() is a right-endpoint sum, which biases the
# cdf upward wherever the density is rising. Total comes to 0.999999, so the
# final rescaling is a rounding correction, not a fudge.
dx_grid <- diff(dens_df$x)[1]
trap <- c(0, cumsum((head(dens_df$y, -1) + tail(dens_df$y, -1)) / 2 * dx_grid))
ecdf_df <- dens_df |>
  mutate(cdf = trap / max(trap))

# Height of the empirical density at its mode, for the dashed guide.
pdf_peak <- max(dens_df$y)
Fc <- approx(ecdf_df$x, ecdf_df$cdf, xout = cval)$y

cat(sprintf("  empirical F(0) from the KDE = %.4f\n", Fc))

p_pdf <- ggplot(dens_df, aes(x, y)) +
  geom_area(data = filter(dens_df, x <= cval), fill = col_orange, alpha = 0.55) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  annotate("text", x = -0.088, y = 3.2,
           label = sprintf("area\n= %.2f", Fc), size = 3.0,
           colour = "black", lineheight = 0.95) +
  annotate("segment", x = cval, xend = cval, y = 0, yend = pdf_peak,
           colour = col_verm, linetype = 2, linewidth = 0.4) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 12)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = NULL, y = "f(r)", title = "pdf: the area up to r = 0") +
  theme_custom()

p_cdf <- ggplot(ecdf_df, aes(x, cdf)) +
  geom_segment(x = -0.22, xend = cval, y = Fc, yend = Fc,
               colour = col_verm, linetype = 2, linewidth = 0.4) +
  geom_segment(x = cval, xend = cval, y = 0, yend = Fc,
               colour = col_verm, linetype = 2, linewidth = 0.4) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  geom_point(x = cval, y = Fc, colour = col_verm, size = 1.8) +
  annotate("text", x = -0.205, y = Fc + 0.11,
           label = sprintf("F(0) = %.2f", Fc), size = 3.0, hjust = 0) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = NULL, y = "F(r)", title = "cdf: that same area, read off") +
  theme_custom()

if (requireNamespace("patchwork", quietly = TRUE)) {
  p_pair <- patchwork::wrap_plots(p_pdf, p_cdf, nrow = 1)
} else {
  stop("patchwork is required for p1_pdf_cdf_pair")
}

save_fig(p_pair, "p1_pdf_cdf_pair", width = 5.6, height = 2.5)

# =============================================================================
# 10b. The right tail: P(r > 0) = 1 - F(0), shaded on the empirical density
# =============================================================================
# Complement of the left tail on "An Area on Real Data", read off the same
# empirical cdf, so 0.39 and 0.61 visibly account for the whole distribution.

tail_cut <- 0
F_cut <- approx(ecdf_df$x, ecdf_df$cdf, xout = tail_cut)$y
tail_p <- 1 - F_cut

cat(sprintf("  P(r > 0) = 1 - F(0) = %.4f (empirical %.4f)\n",
            tail_p, mean(monthly$ret > 0)))

p_rtail <- ggplot(dens_df, aes(x, y)) +
  geom_area(data = filter(dens_df, x >= tail_cut), fill = col_green, alpha = 0.45) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  annotate("segment", x = tail_cut, xend = tail_cut, y = 0, yend = 12,
           colour = col_verm, linetype = 2, linewidth = 0.4) +
  annotate("text", x = 0.105, y = 4.4,
           label = sprintf("area = %.2f", tail_p), size = 3.4) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 12)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = "Monthly return", y = "f(x)") +
  theme_custom()

save_fig(p_rtail, "p1_cdf_righttail", width = 4.6, height = 2.7)

# =============================================================================
# 10c. A middle slice: P(-5% < r <= 0) shaded on the same empirical density
# =============================================================================
# Reads off the same empirical cdf built above, so the two boundary heights the
# slide subtracts are the ones plotted on the previous frame.

slice_lo <- -0.05
slice_hi <- 0
F_lo <- approx(ecdf_df$x, ecdf_df$cdf, xout = slice_lo)$y
F_hi <- approx(ecdf_df$x, ecdf_df$cdf, xout = slice_hi)$y
slice_p <- F_hi - F_lo

cat(sprintf("  F(-5%%) = %.4f, F(0) = %.4f, slice = %.4f (empirical %.4f)\n",
            F_lo, F_hi, slice_p,
            mean(monthly$ret > slice_lo & monthly$ret <= slice_hi)))

p_slice <- ggplot(dens_df, aes(x, y)) +
  geom_area(data = filter(dens_df, x >= slice_lo, x <= slice_hi),
            fill = col_orange, alpha = 0.55) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  annotate("segment", x = slice_lo, xend = slice_lo, y = 0, yend = 9.2,
           colour = col_verm, linetype = 2, linewidth = 0.4) +
  annotate("segment", x = slice_hi, xend = slice_hi, y = 0, yend = 12,
           colour = col_verm, linetype = 2, linewidth = 0.4) +
  annotate("text", x = -0.115, y = 5.0,
           label = sprintf("area = %.2f", slice_p), size = 3.4) +
  annotate("segment", x = -0.083, xend = -0.038, y = 4.6, yend = 2.2,
           colour = "black", linewidth = 0.3,
           arrow = grid::arrow(length = unit(0.12, "cm"))) +
  coord_cartesian(xlim = c(-0.22, 0.22), ylim = c(0, 12)) +
  scale_x_continuous(breaks = c(-0.2, -0.1, -0.05, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "-5%", "0%", "10%", "20%")) +
  labs(x = "Monthly return", y = "f(r)") +
  theme_custom()

save_fig(p_slice, "p1_cdf_slice", width = 4.6, height = 2.7)

# =============================================================================
# 10b. The Normal pdf: the general shape (Wooldridge Figure B.7)
# =============================================================================
# Deliberately generic -- the horizontal axis is labelled in standard deviations
# away from mu, so the picture is about the SHAPE of the family, not about any
# one mean or spread. The sigma arrow is drawn at the inflection point (one sd
# out from the centre), which is where sigma is actually readable off the curve.

norm_shape <- tibble(z = seq(-3.6, 3.6, length.out = 900)) |>
  mutate(y = dnorm(z))            # density (dnorm), not the cdf

peak_h <- dnorm(0)
infl_h <- dnorm(1)                # height at the inflection point z = 1

p_norm_shape <- ggplot(norm_shape, aes(z, y)) +
  geom_area(data = filter(norm_shape, z >= -1, z <= 1),
            fill = col_sky, alpha = 0.30) +
  geom_line(colour = col_blue, linewidth = 0.7) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = peak_h,
           colour = col_verm, linetype = 2, linewidth = 0.4) +
  annotate("segment", x = 0, xend = 1, y = infl_h, yend = infl_h,
           colour = "black", linewidth = 0.35,
           arrow = grid::arrow(length = unit(0.09, "cm"), ends = "both")) +
  annotate("text", x = 0.5, y = infl_h + 0.030,
           label = "sigma", parse = TRUE, size = 3.6) +
  annotate("text", x = 0, y = peak_h + 0.028,
           label = "mu", parse = TRUE, colour = col_verm, size = 3.6) +
  scale_x_continuous(
    breaks = -3:3,
    labels = c(expression(mu - 3 * sigma), expression(mu - 2 * sigma),
               expression(mu - sigma), expression(mu),
               expression(mu + sigma), expression(mu + 2 * sigma),
               expression(mu + 3 * sigma))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  coord_cartesian(xlim = c(-3.6, 3.6)) +
  labs(x = NULL, y = "f(x)") +
  theme_custom()

save_fig(p_norm_shape, "p1_normal_pdf", width = 4.5, height = 2.6)

# =============================================================================
# 11. P(loss > 10%) under the normal model
# =============================================================================
# The fitted normal lives here and below only. Section 10 used to borrow this
# grid; it now draws the empirical density instead, so `norm_df` is defined at
# its point of use.

grid_x <- seq(-0.22, 0.22, length.out = 800)
norm_df <- tibble(x = grid_x, pdf = dnorm(x, mu_m, sd_m),
                  cdf = pnorm(x, mu_m, sd_m))

loss_cut <- -0.10
F_loss <- pnorm(loss_cut, mu_m, sd_m)

p_tail <- ggplot(norm_df, aes(x, pdf)) +
  geom_area(data = filter(norm_df, x <= loss_cut), fill = col_verm, alpha = 0.7) +
  geom_line(colour = col_blue, linewidth = 0.6) +
  annotate("segment", x = -0.175, xend = -0.118, y = 3.6, yend = 0.5,
           colour = col_verm, linewidth = 0.4,
           arrow = grid::arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = -0.175, y = 4.6,
           label = sprintf("%.2f%%", F_loss * 100), colour = col_verm,
           size = 3.6) +
  scale_x_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c("-20%", "-10%", "0%", "10%", "20%")) +
  labs(x = "Monthly return", y = "f(r)") +
  theme_custom()

save_fig(p_tail, "p1_normal_tail", width = 4.6, height = 2.7)

# =============================================================================
# 12. Fat tails: actual returns vs. the normal model that "fits"
# =============================================================================

fat_hist <- hist(monthly$ret,
                 breaks = seq(floor(min(monthly$ret) / 0.01) * 0.01,
                              ceiling(max(monthly$ret) / 0.01) * 0.01,
                              by = 0.01),
                 plot = FALSE)
fat_df <- tibble(mid = fat_hist$mids, dens = fat_hist$density)

fat_norm <- tibble(x = seq(-0.30, 0.30, length.out = 900)) |>
  mutate(pdf = dnorm(x, mu_m, sd_m))

# Panel A: the whole distribution. The normal looks like a decent fit.
p_fat_full <- ggplot() +
  geom_col(data = fat_df, aes(x = mid, y = dens), width = 0.01,
           fill = col_sky, colour = NA) +
  geom_line(data = fat_norm, aes(x = x, y = pdf),
            colour = col_verm, linewidth = 0.7) +
  annotate("rect", xmin = -0.27, xmax = -0.10, ymin = 0, ymax = 0.8,
           fill = NA, colour = "black", linewidth = 0.35, linetype = 2) +
  annotate("text", x = 0.095, y = 10.4, label = "Normal model",
           colour = col_verm, size = 3.0, hjust = 0) +
  annotate("text", x = 0.095, y = 8.8, label = "Actual returns",
           colour = col_sky, size = 3.0, hjust = 0) +
  coord_cartesian(xlim = c(-0.28, 0.28), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.2),
                     labels = paste0(seq(-20, 20, 20), "%")) +
  labs(x = "Monthly return", y = "Density", title = "The whole distribution") +
  theme_custom()

# Panel B: the boxed left tail, magnified. The fit is not decent at all.
p_fat_zoom <- ggplot() +
  geom_col(data = fat_df, aes(x = mid, y = dens), width = 0.01,
           fill = col_sky, colour = NA) +
  geom_line(data = fat_norm, aes(x = x, y = pdf),
            colour = col_verm, linewidth = 0.7) +
  coord_cartesian(xlim = c(-0.27, -0.10), ylim = c(0, 0.8)) +
  scale_x_continuous(breaks = seq(-0.25, -0.10, 0.05),
                     labels = paste0(seq(-25, -10, 5), "%")) +
  labs(x = "Monthly return", y = NULL,
       title = "The far left tail, magnified") +
  theme_custom()

p_fat <- patchwork::wrap_plots(p_fat_full, p_fat_zoom, nrow = 1,
                               widths = c(1.25, 1))

save_fig(p_fat, "p1_fattails", width = 5.6, height = 2.6)

# =============================================================================
# 12b. Empirical density overlaid with a fitted Normal
# =============================================================================
# Same kernel density used on every other continuous slide, against a Normal
# matched on mean and sd. On a linear scale the two look nearly identical, which
# is the point -- so a magnified inset of the left tail carries the disagreement
# the slide then quantifies.

norm_fit <- tibble(x = dens_df$x, y = dnorm(dens_df$x, mu_m, sd_m))

overlay_base <- function(xlim, ylim, title = NULL, legend = FALSE) {
  p <- ggplot() +
    geom_line(data = dens_df, aes(x, y), colour = col_blue, linewidth = 0.7) +
    geom_line(data = norm_fit, aes(x, y), colour = col_verm, linewidth = 0.7,
              linetype = 2) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = NULL, y = NULL, title = title) +
    theme_custom()
  if (legend) {
    p <- p +
      annotate("text", x = 0.088, y = 10.6, label = "Actual returns",
               colour = col_blue, size = 3.0, hjust = 0) +
      annotate("text", x = 0.088, y = 9.1, label = "Normal model",
               colour = col_verm, size = 3.0, hjust = 0)
  }
  p
}

p_overlay_main <- overlay_base(c(-0.28, 0.28), c(0, 12.6), legend = TRUE) +
  annotate("rect", xmin = -0.28, xmax = -0.08, ymin = 0, ymax = 1.1,
           fill = NA, colour = "black", linewidth = 0.3, linetype = 3) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1),
                     labels = paste0(seq(-20, 20, 10), "%")) +
  labs(x = "Monthly return", y = "Density")

p_overlay_inset <- overlay_base(c(-0.28, -0.08), c(0, 1.1),
                                title = "left tail, magnified") +
  scale_x_continuous(breaks = c(-0.25, -0.15)) +
  theme(
    plot.title       = element_text(size = 7, face = "plain", hjust = 0.5),
    axis.text        = element_text(size = 6),
    plot.background  = element_rect(fill = white, colour = "black",
                                    linewidth = 0.3),
    plot.margin      = margin(2, 3, 2, 2)
  )

p_overlay <- p_overlay_main +
  patchwork::inset_element(p_overlay_inset,
                           left = 0.03, bottom = 0.42, right = 0.42, top = 0.99)

save_fig(p_overlay, "p1_normal_overlay", width = 4.4, height = 2.7)

# =============================================================================
# Numbers quoted on the slides — printed so the .tex can be checked against them
# =============================================================================

cat("\nNumbers used in the deck:\n")
cat(sprintf("  P(up month)              = %.4f  (n = %s months, %s-%s)\n",
            theta, format(nrow(sb), big.mark = ","),
            substr(min(sb$eom), 1, 4), substr(max(sb$eom), 1, 4)))
cat(sprintf("  Binomial(20, %.2f): E[X] = %.1f, P(X=14) = %.3f, P(X>=14) = %.3f\n",
            theta, 20 * theta, dbinom(14, 20, theta), 1 - pbinom(13, 20, theta)))
cat(sprintf("  Monthly returns: n = %s, mean = %.2f%%, sd = %.2f%%\n",
            format(nrow(monthly), big.mark = ","), mu_m * 100, sd_m * 100))
cat(sprintf("  Kurtosis                 = %.1f (normal = 3; excess = %.1f)\n",
            mean((monthly$ret - mu_m)^4) / sd_m^4,
            mean((monthly$ret - mu_m)^4) / sd_m^4 - 3))
# Both tails of the fat-tails table, at every cutoff the deck quotes. The fitted
# normal is centred at mu = +0.65%, NOT at zero, so its two sides are unequal:
# -10% sits 2.6 sd below the mean while +10% is only 2.3 sd above it. The slide
# carries a footnote to that effect -- it is the first thing a student asks.
for (cut in c(0.10, 0.15, 0.20)) {
  loss_a <- mean(monthly$ret < -cut)
  loss_n <- pnorm(-cut, mu_m, sd_m)
  gain_a <- mean(monthly$ret > cut)
  gain_n <- pnorm(cut, mu_m, sd_m, lower.tail = FALSE)
  cat(sprintf("  loss worse than %2.0f%%:  normal = %-8.3g%% actual = %.3f%% (%2d months)  [%.0fx]\n",
              cut * 100, loss_n * 100, loss_a * 100,
              sum(monthly$ret < -cut), loss_a / loss_n))
  cat(sprintf("  gain better than %2.0f%%: normal = %-8.3g%% actual = %.3f%% (%2d months)  [%.1fx]\n",
              cut * 100, gain_n * 100, gain_a * 100,
              sum(monthly$ret > cut), gain_a / gain_n))
}
# Waiting times for the -15% month quoted under the fat-tails table: what the
# normal implies, against what the record actually delivered.
cat(sprintf("  Normal: one month below -15%% every %s months = %s years\n",
            format(round(1 / pnorm(-0.15, mu_m, sd_m)), big.mark = ","),
            format(round(1 / pnorm(-0.15, mu_m, sd_m) / 12), big.mark = ",")))
cat(sprintf("  Actual: one month below -15%% every %s months = %s years\n",
            format(round(1 / mean(monthly$ret < -0.15)), big.mark = ","),
            format(round(1 / mean(monthly$ret < -0.15) / 12), big.mark = ",")))
cat(sprintf("  P(r < 0), losing month   = %.4f  (%d of %d months)\n",
            area_emp, sum(monthly$ret < 0), nrow(monthly)))
cat(sprintf("  Binomial(20, %.2f): mode = %d, P(X = mode) = %.3f\n",
            theta, floor(21 * theta), dbinom(floor(21 * theta), 20, theta)))
cat("  Months with a loss worse than 15%: ",
    paste(monthly$date[monthly$ret < -0.15], collapse = ", "), "\n")
cat("  Months with a gain better than 15%: ",
    paste(monthly$date[monthly$ret > 0.15], collapse = ", "), "\n")
cat("  Months with a loss worse than 20%: ",
    paste(monthly$date[monthly$ret < -0.20], collapse = ", "), "\n")

cat("\nAll figures written to", FIG_DIR, "\n")
