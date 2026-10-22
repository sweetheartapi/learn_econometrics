# Lecture 6 (RDD) course script: RD plots when the running variable is smooth, bunched, has a donut hole, heaps or has a mass point.
# Changes for this repo: defined plots_ordered (was missing); ggsave() loop commented out.
# ------------------------------------------------------------
# RD visuals: running-variable scenarios + classic RD plots
# ------------------------------------------------------------
set.seed(123)

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)

# -------------------------
# 1) Simulate running X
# -------------------------
n  <- 20000
c0 <- 0   # cutoff

# Continuous / smooth
x_cont <- rnorm(n, 0, 1)

# Bunching to the right
x_base  <- rnorm(round(n*0.7), 0, 1)
x_bunch <- runif(round(n*0.3), min = 0, max = 0.4)
x_bunching <- c(x_base, x_bunch)

# Donut hole at the cutoff
x_donut <- rnorm(n, 0, 1)
x_donut <- x_donut[abs(x_donut - c0) > 0.15]
x_donut <- c(x_donut, rnorm(n - length(x_donut), 0, 1)) # refill tails -> visible hole

# Heaping / rounding
x_heap_true <- rnorm(n, 0, 1)
x_heaped <- round(x_heap_true / 0.1) * 0.1

# Mass point at cutoff
x_mass <- rnorm(n, 0, 1)
idx <- sample.int(n, size = round(0.06 * n)) # 6% heap at c
x_mass[idx] <- c0

mk_df <- function(x, lab) tibble(scenario = lab, x = x)

dfx <- bind_rows(
  mk_df(x_cont,   "Continuous (valid)"),
  mk_df(x_bunching, "Bunching to the right"),
  mk_df(x_donut,  "Donut hole at cutoff"),
  mk_df(x_heaped, "Heaping / rounding (discrete X)"),
  mk_df(x_mass,   "Mass point at cutoff")
)

# -------------------------
# 2) Generate outcomes Y
# -------------------------
# Smooth baseline function + treatment jump at c0
g <- function(x) 0.5*x + 0.3*x^2         # smooth potential outcome
tau <- 1.0                                # true discontinuity at cutoff
eps_sd <- 1.0

df <- dfx %>%
  mutate(
    D = as.integer(x >= c0),
    y = g(x) + tau * D + rnorm(n(), 0, eps_sd)
  )

# -------------------------
# 3) Helper: triangular kernel weights + local linear fits
# -------------------------
tri_weight <- function(u) pmax(0, 1 - abs(u))  # triangular kernel K(u) = (1-|u|)+

fit_local_linear <- function(dat, c0, h, side = c("left","right")) {
  side <- match.arg(side)
  dsub <- dat %>%
    filter(abs(x - c0) <= h,
           if (side == "left") x < c0 else x >= c0) %>%
    mutate(w = tri_weight((x - c0)/h))
  if (nrow(dsub) < 10) return(NULL)
  
  # Fit weighted local linear: y ~ (x - c0)
  mdl <- lm(y ~ I(x - c0), data = dsub, weights = w)
  
  # Prediction grid on that side inside the bandwidth
  grid <- tibble(
    x = seq(if (side=="left") c0 - h else c0,
            if (side=="left") c0 else c0 + h,
            length.out = 200)
  )
  grid$yhat <- predict(mdl, newdata = grid)
  grid$side <- side
  grid
}

# -------------------------
# 4) Make RD plot builder
# -------------------------
make_rd_plot <- function(dd, c0 = 0, h = 0.6, nbins = 40) {
  
  # Binscatter (equal-width bins within a window, to avoid far points)
  window <- dd %>% filter(abs(x - c0) <= h)
  if (nrow(window) < 50) {
    # if too few points in the chosen h, widen a bit just for plotting
    window <- dd %>% filter(abs(x - c0) <= max(h, 1))
  }
  
  # equal-width bins within window
  breaks <- seq(min(window$x), max(window$x), length.out = nbins + 1)
  binned <- window %>%
    mutate(bin = cut(x, breaks = breaks, include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(x_bin = mean(x), y_bin = mean(y), .groups = "drop")
  
  # Local linear predictions (triangular weights) by side
  pred_left  <- fit_local_linear(dd, c0, h, side = "left")
  pred_right <- fit_local_linear(dd, c0, h, side = "right")
  preds <- bind_rows(pred_left, pred_right)
  
  ggplot() +
    # raw light points in the window (optional; comment out if too dense)
    geom_point(data = window, aes(x = x, y = y), alpha = 0.08, size = 0.7) +
    # binscatter means
    geom_point(data = binned, aes(x = x_bin, y = y_bin), size = 1.8) +
    # local linear fits
    { if (!is.null(preds)) geom_line(data = preds, aes(x = x, y = yhat, linetype = side), linewidth = 1) } +
    geom_vline(xintercept = c0, linetype = "dashed") +
    labs(
      subtitle = sprintf("Cutoff at x = %.2f | Bandwidth h = %.2f | Triangular kernel", c0, h),
      x = "Running variable (X)", y = "Outcome (Y)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

# -------------------------
# 5) Draw one panel per scenario
# -------------------------
# Choose a common bandwidth near the cutoff for visualization
h_vis <- 0.6

library(purrr)

lst <- split(df, df$scenario)
plots <- map(lst, ~ make_rd_plot(.x, c0 = c0, h = h_vis))

library(patchwork)
order <- c("Continuous (valid)", "Bunching to the right",
           "Donut hole at cutoff", "Heaping / rounding (discrete X)",
           "Mass point at cutoff")
wrap_plots(plots[order], ncol = 2, guides = "collect") +
  plot_annotation(title = "RD plots across scenarios")


# Make one grid (2 columns), collect legends at the bottom
plots_ordered <- plots[order]
p_all <- wrap_plots(plots_ordered, ncol = 2, guides = "collect") +
  plot_annotation(
    title = "RD plots across running-variable scenarios",
    subtitle = sprintf("Cutoff at x = %.2f | Bandwidth h = %.2f", c0, h_vis)
  )

# Show it
p_all & theme(legend.position = "bottom")

# for (nm in names(plots)) {
#   fn <- paste0("rd_", gsub("[^a-z0-9]+","_", tolower(nm)), ".pdf")
#   ggsave(fn, plots[[nm]], width = 7, height = 5, dpi = 300)
# }

# -------------------------
# (Optional) Compare different h on the continuous scenario
# -------------------------
dd_cont <- df %>% filter(scenario == "Continuous (valid)")
plot_h <- function(h) make_rd_plot(dd_cont, c0, h) +
  ggtitle(paste0("Continuous (valid): RD plot with h = ", h))

print(plot_h(0.3))
print(plot_h(0.6))
print(plot_h(1.0))
