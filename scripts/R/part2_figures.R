# =============================================================================
# Part 2: Population Parameters and Distribution Moments — figure generation
# =============================================================================
# Companion script for drafts/slides/part2_moments.tex
#
# Generates every data figure used in the Part 2 deck. Conceptual diagrams
# (balance beam for E[X], squared-deviation sketch, covariance quadrants,
# standardization number line) are drawn in TikZ inside the .tex file and are
# deliberately NOT produced here.
#
# Run from the project root:
#   Rscript scripts/R/part2_figures.R
#
# Data sources:
#   US stocks and 10-year Treasuries, monthly 1941-2023
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source("scripts/R/theme.R")

set.seed(20260816)

# ---------- Paths and shared settings ----------------------------------------

RAW_DIR <- "data/raw"
STOCKS_BONDS_CSV <- file.path(RAW_DIR, "stocks_bonds.csv")
SHILLER_XLS <- file.path(RAW_DIR, "shiller", "ie_data.xls")
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
col_purple <- OKABE_ITO[["purple"]]
col_grey <- "#BFBFBF"

MONTHS_PER_YEAR <- 12
DAYS_PER_YEAR <- 252

# =============================================================================
# Data
# =============================================================================

cat("Reading data ...\n")

sb <- read_csv(STOCKS_BONDS_CSV,
               col_types = cols(eom = col_date(), .default = col_double()),
               progress = FALSE) |>
  filter(!is.na(us_stock_market), !is.na(treasury10yr))

stk <- sb$us_stock_market
bnd <- sb$treasury10yr

# Population moments: the slides define Var as E[(X - mu)^2], so use the n
# denominator rather than R's default n-1. With n ~ 1000 the difference is
# invisible, but the script should match the formula on the slide.
pop_var <- function(x) mean((x - mean(x))^2)
pop_sd <- function(x) sqrt(pop_var(x))
pop_cov <- function(x, y) mean((x - mean(x)) * (y - mean(y)))

mu_s <- mean(stk); sd_s <- pop_sd(stk)
mu_b <- mean(bnd); sd_b <- pop_sd(bnd)
cov_sb <- pop_cov(stk, bnd)
corr_sb <- cov_sb / (sd_s * sd_b)

# Annualized (mean scales with time, SD with the square root of time)
ann_mu <- function(m) m * MONTHS_PER_YEAR
ann_sd <- function(s) s * sqrt(MONTHS_PER_YEAR)

# =============================================================================
# 2. Same mean, different spread
# =============================================================================

spread_df <- bind_rows(
  tibble(x = seq(-12, 12, length.out = 600), which = "Low spread") |>
    mutate(y = dnorm(x, 0, 2)),
  tibble(x = seq(-12, 12, length.out = 600), which = "High spread") |>
    mutate(y = dnorm(x, 0, 5))
) |>
  mutate(which = factor(which, levels = c("Low spread", "High spread")))

p_spread <- ggplot(spread_df, aes(x, y, colour = which)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 0, colour = "black", linetype = 2, linewidth = 0.4) +
  # mu alone on the dashed line: the "same mean" caption it used to straddle is
  # gone, since the slide's own text already says the two portfolios share an
  # expected return.
  annotate("text", x = -0.35, y = 0.205, label = "mu", parse = TRUE,
           size = 3.6, hjust = 1) +
  scale_colour_manual(values = c("Low spread" = col_blue,
                                 "High spread" = col_orange)) +
  scale_y_continuous(limits = c(0, 0.22), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "X", y = "f(x)") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.86, 0.82),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_spread, "p2_spread_same_mean", width = 4.4, height = 2.6)

# =============================================================================
# 3. Stocks vs bonds: same picture, very different spread
# =============================================================================

dist_df <- bind_rows(
  tibble(ret = stk, asset = "US stocks"),
  tibble(ret = bnd, asset = "10-year Treasuries")
) |>
  mutate(asset = factor(asset, levels = c("10-year Treasuries", "US stocks")))

p_dist <- ggplot(dist_df, aes(x = ret, fill = asset, colour = asset)) +
  geom_density(alpha = 0.45, linewidth = 0.6, adjust = 1.1) +
  scale_fill_manual(values = c("US stocks" = col_orange,
                               "10-year Treasuries" = col_blue)) +
  scale_colour_manual(values = c("US stocks" = col_orange,
                                 "10-year Treasuries" = col_blue)) +
  coord_cartesian(xlim = c(-0.16, 0.16)) +
  scale_x_continuous(breaks = seq(-0.15, 0.15, 0.05),
                     labels = paste0(seq(-15, 15, 5), "%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Monthly return", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.80, 0.82),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_dist, "p2_stocks_bonds_dist", width = 4.8, height = 2.7)

# =============================================================================
# 4. Property VAR.2 in action: leverage
# =============================================================================

lev_df <- bind_rows(
  tibble(x = seq(-0.30, 0.30, length.out = 700), which = "Unlevered  (X)") |>
    mutate(y = dnorm(x, mu_s, sd_s)),
  tibble(x = seq(-0.30, 0.30, length.out = 700), which = "2x levered  (2X)") |>
    mutate(y = dnorm(x, 2 * mu_s, 2 * sd_s))
) |>
  mutate(which = factor(which, levels = c("Unlevered  (X)", "2x levered  (2X)")))

p_lev <- ggplot(lev_df, aes(x, y, colour = which)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Unlevered  (X)" = col_blue,
                                 "2x levered  (2X)" = col_verm)) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1),
                     labels = paste0(seq(-20, 20, 10), "%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Monthly return", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.82, 0.82),
        legend.background = element_rect(fill = "white", colour = NA))

save_fig(p_lev, "p2_leverage", width = 4.4, height = 2.6)

# =============================================================================
# 5. Standardization: same shape, new axis
# =============================================================================

std_raw <- tibble(x = seq(-0.16, 0.18, length.out = 700)) |>
  mutate(y = dnorm(x, mu_s, sd_s))
std_z <- tibble(x = seq(-4, 4, length.out = 700)) |>
  mutate(y = dnorm(x, 0, 1))

p_raw <- ggplot(std_raw, aes(x, y)) +
  geom_line(colour = col_blue, linewidth = 0.7) +
  geom_vline(xintercept = mu_s, colour = col_verm, linetype = 2,
             linewidth = 0.45) +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.1),
                     labels = paste0(seq(-10, 10, 10), "%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = "Monthly return", y = "f(x)",
       title = sprintf("X:  mean %.2f%%, sd %.2f%%", mu_s * 100, sd_s * 100)) +
  theme_custom()

p_z <- ggplot(std_z, aes(x, y)) +
  geom_line(colour = col_green, linewidth = 0.7) +
  geom_vline(xintercept = 0, colour = col_verm, linetype = 2, linewidth = 0.45) +
  scale_x_continuous(breaks = -3:3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = "Standard deviations from the mean", y = "f(z)",
       title = "Z = (X - mean) / sd:  mean 0, sd 1") +
  theme_custom()

p_std <- patchwork::wrap_plots(p_raw, p_z, nrow = 1)
save_fig(p_std, "p2_standardize", width = 5.6, height = 2.4)

# =============================================================================
# 6. Real scatter: stocks against bonds, with the covariance quadrants
# =============================================================================

scat <- tibble(s = stk, b = bnd) |>
  mutate(quad = if_else((s - mu_s) * (b - mu_b) > 0, "positive", "negative"))

p_scat <- ggplot(scat, aes(x = b, y = s)) +
  geom_vline(xintercept = mu_b, colour = col_verm, linetype = 2,
             linewidth = 0.45) +
  geom_hline(yintercept = mu_s, colour = col_verm, linetype = 2,
             linewidth = 0.45) +
  geom_point(aes(colour = quad), size = 0.7, alpha = 0.55, show.legend = FALSE) +
  scale_colour_manual(values = c(positive = col_blue, negative = col_orange)) +
  scale_x_continuous(breaks = seq(-0.06, 0.12, 0.06),
                     labels = paste0(seq(-6, 12, 6), "%")) +
  scale_y_continuous(breaks = seq(-0.2, 0.1, 0.1),
                     labels = paste0(seq(-20, 10, 10), "%")) +
  labs(x = "10-year Treasury monthly return", y = "US stock monthly return") +
  theme_custom()

save_fig(p_scat, "p2_scatter_stocks_bonds", width = 4.0, height = 2.9)

# =============================================================================
# 7. What different correlations look like
# =============================================================================

sim_corr <- function(rho, n = 400) {
  z1 <- rnorm(n)
  z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
  tibble(x = z1, y = z2,
         panel = sprintf("corr = %+.1f", rho))
}

corr_df <- bind_rows(lapply(c(-0.8, 0, 0.8), sim_corr)) |>
  mutate(panel = factor(panel, levels = sprintf("corr = %+.1f", c(-0.8, 0, 0.8))))

p_corr <- ggplot(corr_df, aes(x, y)) +
  geom_point(colour = col_blue, size = 0.6, alpha = 0.5) +
  facet_wrap(~ panel, nrow = 1) +
  coord_cartesian(xlim = c(-3.2, 3.2), ylim = c(-3.2, 3.2)) +
  labs(x = "X", y = "Y") +
  theme_custom() +
  theme(strip.text = element_text(size = 9, face = "bold"))

save_fig(p_corr, "p2_corr_panels", width = 5.4, height = 2.3)

# =============================================================================
# 8. Zero correlation does NOT mean independent
# =============================================================================

n_u <- 500
u_x <- runif(n_u, -3, 3)
u_y <- u_x^2 + rnorm(n_u, 0, 0.7)
u_corr <- cor(u_x, u_y)

p_ushape <- ggplot(tibble(x = u_x, y = u_y), aes(x, y)) +
  geom_point(colour = col_purple, size = 0.8, alpha = 0.6) +
  annotate("text", x = 0, y = 10.2,
           label = sprintf("corr = %+.2f", u_corr), size = 3.4) +
  coord_cartesian(ylim = c(-1.5, 11.5)) +
  labs(x = "X", y = "Y") +
  theme_custom()

save_fig(p_ushape, "p2_zero_corr_dependent", width = 3.8, height = 2.7)

# =============================================================================
# 9. The payoff: the risk-return frontier
# =============================================================================

# Stored in percentage points, to match the axis labels below.
weights <- seq(0, 1, by = 0.005)
frontier <- tibble(w = weights) |>
  mutate(
    port_mu = ann_mu(w * mu_s + (1 - w) * mu_b) * 100,
    # VAR.3: Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X, Y)
    port_sd = ann_sd(sqrt(w^2 * sd_s^2 + (1 - w)^2 * sd_b^2 +
                            2 * w * (1 - w) * cov_sb)) * 100,
    # What it would look like if the two assets moved in lockstep (corr = 1)
    sd_if_corr1 = ann_sd(w * sd_s + (1 - w) * sd_b) * 100
  )

w_mv <- (sd_b^2 - cov_sb) / (sd_s^2 + sd_b^2 - 2 * cov_sb)
mv <- frontier |> slice_min(port_sd, n = 1)

marks <- frontier |>
  filter(w %in% c(0, 0.2, 0.6, 1)) |>
  mutate(
    lab = c("100% bonds", "20/80", "60/40", "100% stocks"),
    # Hand-placed so the four labels do not collide near the left-hand bend
    lab_x = port_sd + c(0.40, -0.22, 0.40, -0.55),
    lab_y = port_mu + c(-0.42, 0.70, -0.42, 0.55),
    lab_h = c(0, 0, 0, 1)
  )

# geom_path, NOT geom_line: the frontier bends back on itself in x (risk falls
# before it rises), and geom_line would sort by x and draw a self-crossing mess.
p_frontier <- ggplot(frontier, aes(x = port_sd, y = port_mu)) +
  geom_path(aes(x = sd_if_corr1), colour = col_grey, linewidth = 0.6,
            linetype = 2) +
  geom_path(colour = col_blue, linewidth = 0.8) +
  geom_point(data = marks, colour = col_verm, size = 1.8) +
  geom_text(data = marks, aes(x = lab_x, y = lab_y, label = lab, hjust = lab_h),
            size = 2.9) +
  annotate("text", x = 12.6, y = 7.0, label = "if corr were 1",
           colour = "gray45", size = 2.9, angle = 26) +
  coord_cartesian(xlim = c(6.15, 16.2), ylim = c(4.2, 12.9)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Risk (annualized standard deviation)",
       y = "Expected return (annualized)") +
  theme_custom()

save_fig(p_frontier, "p2_frontier", width = 4.9, height = 3.0)

# =============================================================================
# 10. The 68-95-99.7 rule (what a standard deviation buys you)
# =============================================================================

zz <- tibble(x = seq(-4, 4, length.out = 1200)) |> mutate(y = dnorm(x))

band <- function(lo, hi) filter(zz, x >= lo, x <= hi)

p_rule <- ggplot(zz, aes(x, y)) +
  geom_area(data = band(-3, 3), fill = col_sky, alpha = 0.35) +
  geom_area(data = band(-2, 2), fill = col_sky, alpha = 0.45) +
  geom_area(data = band(-1, 1), fill = col_blue, alpha = 0.45) +
  geom_line(colour = col_blue, linewidth = 0.7) +
  # Placed inside the band each one names, not stacked on the centre line
  annotate("text", x = 0, y = 0.17, label = "68%", size = 3.4) +
  annotate("text", x = 1.5, y = 0.055, label = "95%", size = 3.2) +
  annotate("text", x = 2.55, y = 0.017, label = "99.7%", size = 3.0) +
  scale_x_continuous(breaks = -3:3,
                     labels = c("-3sd", "-2sd", "-1sd", "mean",
                                "+1sd", "+2sd", "+3sd")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "f(z)") +
  theme_custom()

save_fig(p_rule, "p2_empirical_rule", width = 4.8, height = 2.5)

# =============================================================================
# 11. The moment anchors: one normal distribution, six times over
# =============================================================================
# Every moment taught in Part 2 gets the SAME picture with a different feature
# marked on it. Students should recognise the shape instantly by the third slide,
# so the only thing they have to read is the annotation.
#
# Deliberate choices, since they are easy to undo by accident:
#   * one running distribution, annual-equity-flavoured (mean 8%, sd 15%);
#   * one shared x-axis in sd units, so the bell never appears to move or resize;
#   * blue = the normal, vermillion = the moment on show, orange = the comparison;
#   * curves labelled in place rather than by legend -- a legend with a white
#     background sits on top of the very peak the slide is asking students to
#     look at.

M_MU <- 8      # percent per year
M_SD <- 15     # percent per year

# Grid wide enough for +/- 4 sd, which is where the tail comparison lives
m_grid <- seq(M_MU - 4.5 * M_SD, M_MU + 4.5 * M_SD, length.out = 1600)
m_norm <- tibble(x = m_grid, y = dnorm(m_grid, M_MU, M_SD))

# Shared axis for the four univariate anchors, so the shape never jumps
m_breaks <- M_MU + M_SD * (-3:3)
m_labels <- c("-3sd", "-2sd", "-1sd", "mean", "+1sd", "+2sd", "+3sd")
m_xlim <- c(M_MU - 3.6 * M_SD, M_MU + 3.6 * M_SD)

y_top <- dnorm(M_MU, M_MU, M_SD)      # height of the normal at its peak
y_sd <- dnorm(M_MU + M_SD, M_MU, M_SD)  # height one sd out

#' Common scaffolding for the univariate moment anchors.
#' Returns a ggplot with the axis, limits and theme set but no density drawn,
#' so each anchor can layer its own shading underneath the curve.
moment_base <- function(top_room = 0.12) {
  ggplot(m_norm, aes(x, y)) +
    scale_x_continuous(breaks = m_breaks, labels = m_labels) +
    scale_y_continuous(expand = expansion(mult = c(0.04, top_room))) +
    coord_cartesian(xlim = m_xlim) +
    labs(x = "Annual return", y = "f(x)") +
    theme_custom()
}

# ---- 1st moment: where the distribution sits -------------------------------

p_m_mean <- moment_base() +
  geom_area(fill = col_blue, alpha = 0.13) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  geom_segment(x = M_MU, xend = M_MU, y = 0, yend = y_top,
               colour = col_verm, linewidth = 0.9) +
  # Fulcrum, echoing the balance-beam diagram drawn in TikZ on the earlier slide
  annotate("point", x = M_MU, y = 0, shape = 24, size = 3.2,
           fill = col_verm, colour = col_verm) +
  annotate("text", x = M_MU + 0.06 * M_SD, y = y_top * 1.10,
           # `==` is non-associative in R's grammar, so the equality is written
           # with quoted "=" glue rather than the `==` operator
           label = 'E*"["*X*"]"~"="~mu', parse = TRUE,
           colour = col_verm, size = 4.0, hjust = 0) +
  annotate("text", x = M_MU - 3.4 * M_SD, y = y_top * 0.92,
           label = "The first moment.\nThe balance point of\nthe whole distribution.",
           colour = "gray35", size = 3.0, hjust = 0, vjust = 1, lineheight = 1.15)

save_fig(p_m_mean, "p2_moment_mean", width = 4.9, height = 2.7)

# ---- 2nd moment: how far it spreads ----------------------------------------
# The +/-1sd band is shaded rather than outlined so the width arrow reads on top
# of it; the arrow sits low in the band, clear of the peak annotation.

m_band <- filter(m_norm, x >= M_MU - M_SD, x <= M_MU + M_SD)
y_arrow <- y_sd * 0.45

p_m_var <- moment_base() +
  geom_area(data = m_band, fill = col_sky, alpha = 0.45) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  geom_segment(x = M_MU, xend = M_MU, y = 0, yend = y_top,
               colour = col_verm, linewidth = 0.5, linetype = 2) +
  geom_segment(x = M_MU - M_SD, xend = M_MU - M_SD, y = 0, yend = y_sd,
               colour = col_verm, linewidth = 0.5) +
  geom_segment(x = M_MU + M_SD, xend = M_MU + M_SD, y = 0, yend = y_sd,
               colour = col_verm, linewidth = 0.5) +
  annotate("segment", x = M_MU, xend = M_MU + M_SD, y = y_arrow, yend = y_arrow,
           colour = col_verm, linewidth = 0.6,
           arrow = arrow(length = unit(0.13, "cm"), ends = "both",
                         type = "closed")) +
  annotate("text", x = M_MU + 0.62 * M_SD, y = y_arrow + y_top * 0.085,
           label = 'sigma~"="~"15%"', parse = TRUE, colour = col_verm,
           size = 3.6) +
  annotate("text", x = M_MU, y = y_top * 1.10, label = "68% of the mass",
           size = 3.2, colour = "gray25") +
  annotate("text", x = M_MU - 3.4 * M_SD, y = y_top * 0.92,
           label = "The second moment.\nHow far outcomes sit\nfrom that balance point.",
           colour = "gray35", size = 3.0, hjust = 0, vjust = 1, lineheight = 1.15)

save_fig(p_m_var, "p2_moment_variance", width = 4.9, height = 2.7)

# ---- 3rd moment: which side is heavier --------------------------------------
# Skew-normal, density 2 phi(z) Phi(alpha z). Chosen over a normal mixture
# because a mixture separated enough to look skewed also looks bimodal, which
# invites exactly the wrong reading. Support stays unbounded on both sides.

ALPHA <- -5                                  # negative alpha => long LEFT tail
sn_delta <- ALPHA / sqrt(1 + ALPHA^2)
sn_mean <- sn_delta * sqrt(2 / pi)
sn_sd <- sqrt(1 - 2 * sn_delta^2 / pi)
# Third standardized moment of the skew-normal; invariant to the affine
# rescaling below, so this is the skewness of the curve actually plotted.
sn_skew <- ((4 - pi) / 2) * sn_mean^3 / sn_sd^3

#' Skew-normal density mapped onto mean M_MU and sd M_SD.
#' The affine map picks up an sn_sd / M_SD Jacobian.
d_skew <- function(x) {
  s <- sn_mean + sn_sd * (x - M_MU) / M_SD
  2 * dnorm(s) * pnorm(ALPHA * s) * sn_sd / M_SD
}

skew_df <- bind_rows(
  tibble(x = m_grid, y = dnorm(m_grid, M_MU, M_SD), which = "normal"),
  tibble(x = m_grid, y = d_skew(m_grid), which = "skewed")
)

p_m_skew <- ggplot(skew_df, aes(x, y, colour = which)) +
  geom_line(linewidth = 0.85, show.legend = FALSE) +
  geom_vline(xintercept = M_MU, colour = col_verm, linetype = 2,
             linewidth = 0.5) +
  scale_colour_manual(values = c(normal = col_blue, skewed = col_orange)) +
  # Labelled on the curves themselves; a legend box would cover the peak
  annotate("text", x = M_MU - 1.55 * M_SD, y = y_top * 0.70,
           label = "Normal\nskew = 0", colour = col_blue, size = 3.1,
           hjust = 1, lineheight = 1.15) +
  annotate("text", x = M_MU + 1.15 * M_SD, y = y_top * 1.35,
           label = "Left-skewed\nskew < 0", colour = col_orange, size = 3.1,
           hjust = 0, lineheight = 1.15) +
  annotate("segment", x = M_MU - 2.75 * M_SD, xend = M_MU - 2.30 * M_SD,
           y = y_top * 0.155, yend = y_top * 0.055, colour = "gray40",
           linewidth = 0.4, arrow = arrow(length = unit(0.11, "cm"))) +
  annotate("text", x = M_MU - 3.45 * M_SD, y = y_top * 0.20,
           label = "the long tail is\non the downside", colour = "gray35",
           size = 3.0, hjust = 0, vjust = 0, lineheight = 1.15) +
  annotate("text", x = M_MU + 0.10 * M_SD, y = y_top * 1.72, label = "same mean",
           colour = col_verm, size = 3.0, hjust = 0) +
  scale_x_continuous(breaks = m_breaks, labels = m_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.10))) +
  coord_cartesian(xlim = m_xlim) +
  labs(x = "Annual return", y = "f(x)") +
  theme_custom()

save_fig(p_m_skew, "p2_moment_skewness", width = 4.9, height = 2.7)

# ---- 4th moment: how fat the tails are --------------------------------------
# Student t with NU df, rescaled to the same mean and sd as the normal, so the
# only difference between the two curves is the fourth moment.

NU <- 5
t_scale <- sqrt((NU - 2) / NU)   # sd of t_NU is 1 / t_scale, so this normalizes

d_fat <- function(x) {
  z <- (x - M_MU) / M_SD
  dt(z / t_scale, df = NU) / (t_scale * M_SD)
}

fat_df <- bind_rows(
  tibble(x = m_grid, y = dnorm(m_grid, M_MU, M_SD), which = "normal"),
  tibble(x = m_grid, y = d_fat(m_grid), which = "fat")
)
fat_cols <- c(normal = col_blue, fat = col_orange)

p_fat_body <- ggplot(fat_df, aes(x, y, colour = which)) +
  geom_line(linewidth = 0.85, show.legend = FALSE) +
  scale_colour_manual(values = fat_cols) +
  annotate("text", x = M_MU - 3.4 * M_SD, y = y_top * 1.02,
           label = "Normal\nkurtosis = 3", colour = col_blue, size = 3.0,
           hjust = 0, vjust = 1, lineheight = 1.15) +
  annotate("text", x = M_MU + 3.4 * M_SD, y = y_top * 1.02,
           label = "Fat-tailed\nkurtosis = 9", colour = col_orange, size = 3.0,
           hjust = 1, vjust = 1, lineheight = 1.15) +
  scale_x_continuous(breaks = M_MU + M_SD * c(-2, 0, 2),
                     labels = c("-2sd", "mean", "+2sd")) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.10))) +
  coord_cartesian(xlim = m_xlim) +
  labs(x = "Annual return", y = "f(x)", title = "Same mean, same sd") +
  theme_custom() +
  theme(plot.title = element_text(size = 9, face = "bold"))

# On a linear axis the two tails are both visually zero and the slide says
# nothing. A log axis is the only honest way to show a 100-fold difference in
# probability next to a 30-fold difference in peak height.
TAIL_Z <- 4
tail_ratio <- d_fat(M_MU - TAIL_Z * M_SD) /
  dnorm(M_MU - TAIL_Z * M_SD, M_MU, M_SD)

p_fat_tail <- ggplot(filter(fat_df, x <= M_MU - 1.9 * M_SD),
                     aes(x, y, colour = which)) +
  geom_line(linewidth = 0.85, show.legend = FALSE) +
  scale_colour_manual(values = fat_cols) +
  scale_x_continuous(breaks = M_MU + M_SD * c(-4, -3, -2),
                     labels = c("-4sd", "-3sd", "-2sd")) +
  scale_y_log10(breaks = 10^(-7:-3),
                labels = c("1e-7", "1e-6", "1e-5", "1e-4", "1e-3")) +
  coord_cartesian(xlim = c(M_MU - 4.4 * M_SD, M_MU - 1.9 * M_SD),
                  ylim = c(1e-7, 3e-3)) +
  annotate("text", x = M_MU - 4.3 * M_SD, y = 4e-7,
           label = sprintf("%.0f times more likely\nat -4sd", tail_ratio),
           colour = col_orange, size = 2.9, hjust = 0, lineheight = 1.15) +
  labs(x = "Annual return", y = NULL,
       title = "The left tail, log scale") +
  theme_custom() +
  theme(plot.title = element_text(size = 9, face = "bold"))

p_m_kurt <- patchwork::wrap_plots(p_fat_body, p_fat_tail, nrow = 1,
                                  widths = c(1.25, 1))
save_fig(p_m_kurt, "p2_moment_kurtosis", width = 5.7, height = 2.5)

# ---- Bivariate: covariance as a tilted normal -------------------------------
# Contours of a bivariate normal with rho = 0.7, laid over the same +/- quadrant
# shading used in the covariance TikZ diagram, so the algebra and the picture
# carry identical colours.

RHO_COV <- 0.7
BIV_LIM <- 3.4

biv_grid <- expand_grid(x = seq(-BIV_LIM, BIV_LIM, length.out = 240),
                        y = seq(-BIV_LIM, BIV_LIM, length.out = 240))

#' Standard bivariate normal density with correlation rho.
d_biv <- function(x, y, rho) {
  q <- (x^2 - 2 * rho * x * y + y^2) / (1 - rho^2)
  exp(-q / 2) / (2 * pi * sqrt(1 - rho^2))
}

cov_df <- biv_grid |> mutate(z = d_biv(x, y, RHO_COV))

quad_fill <- tibble(
  xmin = c(0, -BIV_LIM, -BIV_LIM, 0), xmax = c(BIV_LIM, 0, 0, BIV_LIM),
  ymin = c(0, -BIV_LIM, 0, -BIV_LIM), ymax = c(BIV_LIM, 0, BIV_LIM, 0),
  sign = c("pos", "pos", "neg", "neg")
)

p_m_cov <- ggplot() +
  geom_rect(data = quad_fill,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sign),
            alpha = 0.16) +
  scale_fill_manual(values = c(pos = col_blue, neg = col_orange), guide = "none") +
  geom_contour(data = cov_df, aes(x, y, z = z), colour = col_blue,
               linewidth = 0.5, bins = 7) +
  geom_hline(yintercept = 0, colour = col_verm, linetype = 2, linewidth = 0.45) +
  geom_vline(xintercept = 0, colour = col_verm, linetype = 2, linewidth = 0.45) +
  annotate("text", x = c(2.5, -2.5), y = c(2.5, -2.5), label = "+",
           colour = col_blue, size = 5.5, fontface = "bold") +
  annotate("text", x = c(-2.5, 2.5), y = c(2.5, -2.5), label = "-",
           colour = col_orange, size = 5.5, fontface = "bold") +
  scale_x_continuous(breaks = c(-2, 0, 2),
                     labels = c("-2sd", expression(mu[X]), "+2sd")) +
  scale_y_continuous(breaks = c(-2, 0, 2),
                     labels = c("-2sd", expression(mu[Y]), "+2sd")) +
  coord_fixed(xlim = c(-BIV_LIM, BIV_LIM), ylim = c(-BIV_LIM, BIV_LIM),
              expand = FALSE) +
  labs(x = "X", y = "Y") +
  theme_custom()

save_fig(p_m_cov, "p2_moment_covariance", width = 3.3, height = 3.0)

# ---- Bivariate: correlation is covariance after standardizing ---------------
# The same joint normal twice. Left panel carries real units, so the number
# attached to the picture is 63 percent-squared and means nothing; right panel
# divides both axes by their sd and the number becomes 0.7. Identical cloud,
# interpretable number -- which is the whole argument for correlation.

RHO_CORR <- 0.7
RAW_SDX <- 6      # bond-like, percent
RAW_SDY <- 15     # stock-like, percent
RAW_MUX <- 4
RAW_MUY <- 8

raw_df <- biv_grid |>
  mutate(z = d_biv(x, y, RHO_CORR),
         x = RAW_MUX + RAW_SDX * x,
         y = RAW_MUY + RAW_SDY * y)

p_corr_raw <- ggplot(raw_df, aes(x, y, z = z)) +
  geom_contour(colour = col_blue, linewidth = 0.5, bins = 6) +
  geom_hline(yintercept = RAW_MUY, colour = col_verm, linetype = 2,
             linewidth = 0.4) +
  geom_vline(xintercept = RAW_MUX, colour = col_verm, linetype = 2,
             linewidth = 0.4) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  scale_y_continuous(labels = function(v) paste0(v, "%")) +
  labs(x = "X  (sd 6%)", y = "Y  (sd 15%)",
       title = "Raw units:  cov = 63 %-squared") +
  theme_custom() +
  theme(plot.title = element_text(size = 8.5, face = "bold", colour = col_blue))

p_corr_z <- ggplot(mutate(biv_grid, z = d_biv(x, y, RHO_CORR)), aes(x, y, z = z)) +
  geom_contour(colour = col_green, linewidth = 0.5, bins = 6) +
  geom_hline(yintercept = 0, colour = col_verm, linetype = 2, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = col_verm, linetype = 2, linewidth = 0.4) +
  scale_x_continuous(breaks = -2:2) +
  scale_y_continuous(breaks = -2:2) +
  labs(x = expression((X - mu[X]) / sigma[X]),
       y = expression((Y - mu[Y]) / sigma[Y]),
       title = "Standardized:  corr = 0.7") +
  theme_custom() +
  theme(plot.title = element_text(size = 8.5, face = "bold", colour = col_green))

p_m_corr <- patchwork::wrap_plots(p_corr_raw, p_corr_z, nrow = 1)
save_fig(p_m_corr, "p2_moment_correlation", width = 5.6, height = 2.5)

# =============================================================================
# 12. Question of the day: the two return distributions the question is about
# =============================================================================
# Stocks come from Shiller (consistent with Part 1, which reads the same
# workbook); bonds cannot -- Shiller's only bond column is "Rate GS10", a
# constant-maturity YIELD, not a return. Turning a yield into a return needs a
# duration model, which is not something to do silently on a motivation slide,
# so the 10-year Treasury RETURN series is used for the bond side.
#
# Two traps handled below:
#   * Shiller column 10 is the REAL total return index. Pairing a real stock
#     return with a nominal bond return would be meaningless, so it is converted
#     back to nominal with the CPI column (col 5).
#   * Shiller prices are monthly AVERAGES of daily closes, which smooths the
#     series: sd is ~12.1%/yr here against ~14.8%/yr for the CRSP series used
#     everywhere else in Part 2. This figure therefore shows SHAPES ONLY and
#     quotes no moments -- the numbers on later slides stay CRSP-based.

shiller_raw <- read_excel(SHILLER_XLS, sheet = "Data", skip = 7,
                          col_names = FALSE, .name_repair = "minimal")

# Date is numeric YYYY.MM, so October reads as .1 -- recover the month by
# rounding, never by slicing the string (see data/raw/shiller/README.md).
shiller <- tibble(
  date_num = suppressWarnings(as.numeric(shiller_raw[[1]])),
  cpi      = suppressWarnings(as.numeric(shiller_raw[[5]])),
  tr_real  = suppressWarnings(as.numeric(shiller_raw[[10]]))
) |>
  filter(!is.na(date_num), !is.na(cpi), !is.na(tr_real)) |>
  mutate(month = as.Date(sprintf("%d-%02d-01", floor(date_num),
                                 round((date_num - floor(date_num)) * 100)))) |>
  arrange(month) |>
  # Real index x CPI ratio = nominal return; the CPI base cancels in the ratio
  mutate(ret = (tr_real / lag(tr_real)) * (cpi / lag(cpi)) - 1) |>
  filter(!is.na(ret))

# Restrict to the months where the Treasury return series also exists, so the
# two densities describe the same period and the comparison is honest.
sb_months <- as.Date(format(sb$eom, "%Y-%m-01"))

# Labelled as the slide labels them -- "Equity fund" and "Treasuries" -- rather
# than by data source, since the question of the day is about a generic two-asset
# portfolio. Provenance is carried in the source line under the figure instead.
qotd_df <- bind_rows(
  shiller |>
    filter(month %in% sb_months) |>
    transmute(ret, asset = "Equity fund"),
  tibble(ret = bnd, asset = "10-year Treasuries")
) |>
  mutate(asset = factor(asset, levels = c("Equity fund", "10-year Treasuries")))

p_qotd <- ggplot(qotd_df, aes(x = ret, fill = asset, colour = asset)) +
  geom_density(alpha = 0.45, linewidth = 0.6, adjust = 1.1) +
  scale_fill_manual(values = c("Equity fund" = col_orange,
                               "10-year Treasuries" = col_blue)) +
  scale_colour_manual(values = c("Equity fund" = col_orange,
                                 "10-year Treasuries" = col_blue)) +
  coord_cartesian(xlim = c(-0.16, 0.16)) +
  scale_x_continuous(breaks = seq(-0.15, 0.15, 0.05),
                     labels = paste0(seq(-15, 15, 5), "%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = "Monthly return", y = "Density") +
  theme_custom() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.72, 0.86),
        legend.text = element_text(size = 8.5),
        legend.key.width = unit(0.7, "cm"),
        legend.background = element_rect(fill = "white", colour = NA))

# Sits in a half-width column, so it still wants some vertical proportion -- but
# wide enough that the two densities are not squeezed against the axis.
save_fig(p_qotd, "p2_qotd_stocks_bonds", width = 5.0, height = 2.9)

# Moments of exactly the two series drawn above, so anything the deck quotes
# alongside this figure matches it. These are NOT the same as the CRSP-based
# numbers computed at the top of the script: the Shiller stock series is a
# monthly average of daily closes, which damps its variance and changes its
# covariance with bonds.
qotd_stk <- shiller |> filter(month %in% sb_months) |> pull(ret)
qotd_cov <- pop_cov(qotd_stk, bnd)
qotd_corr <- qotd_cov / (pop_sd(qotd_stk) * pop_sd(bnd))

# =============================================================================
# 13. Law of large numbers: the coin flip
# =============================================================================
# The cleanest demonstration that Xbar -> E[X], because E[X] = 0.5 is known
# exactly and needs no estimation.
#
# ONE path, by choice. A fan of paths shows the narrowing funnel more honestly,
# but it also makes the slide about the spread across runs; a single trace keeps
# the eye on one number settling down. The gambler's-fallacy Q&A on the slide is
# what guards against reading the single path as "convergence is guaranteed
# flip by flip".

N_FLIPS <- 10000

flips <- rbinom(N_FLIPS, size = 1, prob = 0.5)
lln <- tibble(n = seq_len(N_FLIPS),
              # Running sample mean after n flips
              xbar = cumsum(flips) / seq_len(N_FLIPS))

# Log x-axis: the interesting behaviour spans four orders of magnitude, and on a
# linear axis the first 100 flips -- where all the movement is -- are invisible.
p_lln <- ggplot(lln, aes(n, xbar)) +
  geom_line(colour = col_blue, linewidth = 0.5) +
  geom_hline(yintercept = 0.5, colour = col_verm, linewidth = 0.7,
             linetype = 2) +
  annotate("text", x = 6500, y = 0.565, label = 'E*"["*X*"]"~"="~0.5',
           parse = TRUE, colour = col_verm, size = 3.4, hjust = 1) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000),
                labels = c("1", "10", "100", "1,000", "10,000")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  labs(x = "Number of flips  (log scale)",
       y = expression(bar(X)[n]~~"(share of heads)")) +
  theme_custom()

save_fig(p_lln, "p2_lln_coinflip", width = 5.0, height = 2.7)

# Milestones along the single path, for the numbers quoted on the slide
lln_at <- function(k) lln$xbar[k]

# =============================================================================
# Numbers quoted on the slides — printed so the .tex can be checked against them
# =============================================================================

port <- function(w) {
  list(mu = ann_mu(w * mu_s + (1 - w) * mu_b),
       sd = ann_sd(sqrt(w^2 * sd_s^2 + (1 - w)^2 * sd_b^2 +
                          2 * w * (1 - w) * cov_sb)))
}

cat("\nNumbers used in the deck:\n")
cat(sprintf("  Scenario example: E[R] = %.1f%%\n",
            0.20 * -15 + 0.60 * 8 + 0.20 * 25))
cat(sprintf("  Stocks: %.3f%%/mo, sd %.3f%%/mo  ->  %.2f%%/yr, sd %.2f%%/yr\n",
            mu_s * 100, sd_s * 100, ann_mu(mu_s) * 100, ann_sd(sd_s) * 100))
cat(sprintf("  Bonds:  %.3f%%/mo, sd %.3f%%/mo  ->  %.2f%%/yr, sd %.2f%%/yr\n",
            mu_b * 100, sd_b * 100, ann_mu(mu_b) * 100, ann_sd(sd_b) * 100))
cat(sprintf("  cov(stocks, bonds) = %.3g   corr = %.4f   [CRSP stocks]\n",
            cov_sb, corr_sb))
cat(sprintf("  cov(stocks, bonds) = %.3g   corr = %.4f   [Shiller stocks --",
            qotd_cov, qotd_corr))
cat(" the series plotted on 'Question of the lesson']\n")
cat(sprintf("    Shiller stocks: mean %.3f%%/mo, sd %.3f%%/mo (n = %d)\n",
            mean(qotd_stk) * 100, pop_sd(qotd_stk) * 100, length(qotd_stk)))

# Third and fourth standardized moments, population convention (n denominator),
# matching the E[(X-mu)^k]/sigma^k formulas printed on the higher-moment slides.
pop_skew <- function(x) mean((x - mean(x))^3) / pop_sd(x)^3
pop_kurt <- function(x) mean((x - mean(x))^4) / pop_sd(x)^4

cat(sprintf("  stocks [CRSP]:  skew = %+.2f, kurtosis = %.2f (excess %+.2f), n = %d\n",
            pop_skew(stk), pop_kurt(stk), pop_kurt(stk) - 3, length(stk)))
cat(sprintf("  bonds:          skew = %+.2f, kurtosis = %.2f (excess %+.2f), n = %d\n",
            pop_skew(bnd), pop_kurt(bnd), pop_kurt(bnd) - 3, length(bnd)))

# Shiller S&P, the series actually plotted on "Question of the lesson". Reported
# on three samples because the answer depends materially on which one is used:
# the pre-1941 history is far more extreme than the post-war period.
shiller_real <- shiller |>
  mutate(ret_real = (tr_real / lag(tr_real)) - 1) |>
  filter(!is.na(ret_real)) |>
  pull(ret_real)

for (nm in c("full 1871-2026", "1941-2023 (matched)", "full, REAL")) {
  x <- switch(nm,
              "full 1871-2026"      = shiller$ret,
              "1941-2023 (matched)" = qotd_stk,
              "full, REAL"          = shiller_real)
  cat(sprintf("  S&P [Shiller, %-19s]: skew = %+.2f, kurtosis = %5.2f (excess %+.2f), n = %d\n",
              nm, pop_skew(x), pop_kurt(x), pop_kurt(x) - 3, length(x)))
}
cat(sprintf("  moment anchors: skew-normal skew = %+.2f, t_%d kurtosis = %.1f,",
            sn_skew, NU, 3 + 6 / (NU - 4)))
cat(sprintf(" tail ratio at -4sd = %.0fx\n", tail_ratio))
cat("  LLN coin flip, the single path drawn:\n")
for (k in c(10, 100, 1000, N_FLIPS)) {
  cat(sprintf("    after %6s flips: Xbar = %.3f\n",
              format(k, big.mark = ","), lln_at(k)))
}
for (w in c(0, 0.2, 0.6, 1)) {
  pw <- port(w)
  cat(sprintf("  %3.0f%% stocks: return %5.2f%%, risk %5.2f%%\n",
              w * 100, pw$mu * 100, pw$sd * 100))
}
cat(sprintf("  Minimum-variance mix: %.1f%% stocks, risk %.2f%%\n",
            w_mv * 100, mv$port_sd))
cat(sprintf("  U-shape example: corr = %+.3f (but Y is a function of X)\n",
            u_corr))

cat("\n  Empirical rule against %d months of stock returns:\n" |>
      sprintf(nrow(sb)))
for (k in 1:3) {
  cat(sprintf("    within %d sd: actual %5.2f%%   normal %5.2f%%\n",
              k, mean(abs(stk - mu_s) <= k * sd_s) * 100,
              (pnorm(k) - pnorm(-k)) * 100))
}
cat(sprintf("    beyond 3 sd: actual %.2f%% (%d months)   normal %.2f%%\n",
            mean(abs(stk - mu_s) > 3 * sd_s) * 100,
            sum(abs(stk - mu_s) > 3 * sd_s),
            (1 - (pnorm(3) - pnorm(-3))) * 100))

worst_month <- sb |> mutate(z = (us_stock_market - mu_s) / sd_s) |>
  slice_min(z, n = 1)
cat(sprintf("\n  Worst month: %s, %.1f%% = %.1f sd\n",
            format(worst_month$eom), worst_month$us_stock_market * 100,
            worst_month$z))
# The daily Black Monday figures were dropped along with the "Where It Breaks"
# frame -- Part 1 now carries the fat-tail argument. The daily file, mu_d and
# sd_d have now been removed along with old section 1 (p2_bernoulli_mean and
# p2_binomial_mean, neither of which appeared in any deck). Numbering keeps the
# gap on purpose: the Beamer source cites these section numbers.

cat("\nAll figures written to", FIG_DIR, "\n")
