# Lecture 6 (RDD) course script: bias, variance and MSE of the local linear RD estimate across bandwidths (Monte Carlo).
# Changes for this repo: ggsave() lines commented out (they wrote to the lecturer's own folders).
# ------------------------------------------------------------
# RD bandwidth choice: bias, variance, and MSE vs h (graphical)
# ------------------------------------------------------------
set.seed(123)

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)

# -------------------------
# 1) DGP (curvature near c)
# -------------------------
n         <- 2000           # sample size per replication
c0        <- 0              # cutoff
tau_true  <- 1.0            # true jump
f <- function(x) 0.3*x + 0.35*x^2 - 0.05*x^3 + 0.25*sin(2.5*x)  # smooth curved baseline
sigma_fun <- function(x) 0.7 + 0.1*abs(x)                       # mild heteroskedasticity

sim_once <- function(n) {
  x <- runif(n, -2.5, 2.5)
  eps <- rnorm(n, sd = sigma_fun(x))
  y <- f(x) + tau_true * (x >= c0) + eps
  list(x = x, y = y)
}

# -------------------------
# 2) Local linear RD estimator (triangular kernel)
# -------------------------
tri <- function(u) pmax(0, 1 - abs(u))

ll_rd <- function(x, y, c, h) {
  # indices within bandwidth
  win <- which(abs(x - c) <= h)
  if (length(win) < 30) return(NA_real_) # too few points
  
  # weights (triangular)
  w <- tri((x[win] - c) / h)
  
  # split sides
  left  <- which(x[win] <  c)
  right <- which(x[win] >= c)
  if (length(left) < 10 || length(right) < 10) return(NA_real_)
  
  # local linear by side
  df_L <- data.frame(y = y[win][left],  xc = x[win][left]  - c, w = w[left])
  df_R <- data.frame(y = y[win][right], xc = x[win][right] - c, w = w[right])
  
  fit_L <- try(lm(y ~ xc, data = df_L, weights = w), silent = TRUE)
  fit_R <- try(lm(y ~ xc, data = df_R, weights = w), silent = TRUE)
  if (inherits(fit_L, "try-error") || inherits(fit_R, "try-error")) return(NA_real_)
  
  # predicted limits at c (xc = 0)
  yL_c <- coef(fit_L)[1]
  yR_c <- coef(fit_R)[1]
  as.numeric(yR_c - yL_c)
}

# -------------------------
# 3) Monte Carlo across bandwidths
# -------------------------
B       <- 300                                   # replications (increase for smoother curves)
h_grid  <- seq(0.15, 1.6, length.out = 24)       # bandwidth grid

# run MC
estimates <- map_dfr(seq_len(B), function(b) {
  s <- sim_once(n)
  tibble(
    h   = h_grid,
    tau = vapply(h_grid, function(h) ll_rd(s$x, s$y, c0, h), numeric(1))
  ) %>% mutate(rep = b)
})

# drop NAs (if any extremely small h caused too few points)
estimates <- drop_na(estimates)

# summarize: bias, variance, MSE vs h
summ <- estimates %>%
  group_by(h) %>%
  summarise(
    mean_tau = mean(tau),
    bias     = mean_tau - tau_true,
    var      = var(tau),
    mse      = bias^2 + var,
    .groups = "drop"
  )

# find h that minimizes MSE
h_star <- summ$h[which.min(summ$mse)]

# -------------------------
# 4) Plots: bias, variance, MSE vs h; plus tau(h) cloud
# -------------------------
p1 <- ggplot(summ, aes(x = h, y = bias)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  geom_vline(xintercept = h_star, linetype = "dashed") +
  labs(title = "Bias vs bandwidth", x = "Bandwidth h", y = "Bias(h)")

p2 <- ggplot(summ, aes(x = h, y = var)) +
  geom_line() +
  geom_vline(xintercept = h_star, linetype = "dashed") +
  labs(title = "Variance vs bandwidth", x = "Bandwidth h", y = "Var(h)")

p3 <- ggplot(summ, aes(x = h, y = mse)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = h_star, linetype = "dashed") +
  annotate("label", x = h_star, y = min(summ$mse),
           label = paste0("h* = ", round(h_star, 3)), vjust = -0.5) +
  labs(title = "MSE vs bandwidth", x = "Bandwidth h", y = "MSE(h) = Bias(h)^2 + Var(h)")

# show the distribution of tau_hat across h (faint lines per replication)
p4 <- ggplot(estimates, aes(x = h, y = tau, group = rep)) +
  geom_line(alpha = 0.08) +
  geom_line(data = summ, aes(x = h, y = mean_tau), linewidth = 1.2, inherit.aes = FALSE) +
  geom_hline(yintercept = tau_true, linetype = "dashed") +
  geom_vline(xintercept = h_star, linetype = "dashed") +
  labs(title = expression(hat(tau)~"across replications by bandwidth"),
       subtitle = "Gray = individual reps; black = mean; dashed = true τ and h*",
       x = "Bandwidth h", y = expression(hat(tau)(h)))

(p1 | p2) / (p3 | p4)

# ggsave("RD bias.pdf", p1, width = 7, height = 5, dpi = 300)
# ggsave("RD variance.pdf", p2, width = 7, height = 5, dpi = 300)
# ggsave("RD MSE.pdf", p3, width = 7, height = 5, dpi = 300)

# -------------------------
# 5) (Optional) Visual intuition: fitted lines for a few h on one sample
# -------------------------
s <- sim_once(n)
x <- s$x; y <- s$y
xc <- x - c0

h_show <- c(min(h_grid), median(h_grid), max(h_grid))

tri <- function(u) pmax(0, 1 - abs(u))

make_pred <- function(h) {
  idx <- which(abs(x - c0) <= h)
  if (length(idx) < 30) return(NULL)
  
  w_all <- tri((x[idx] - c0) / h)
  
  # Left/right indices within the window
  iL <- idx[x[idx] <  c0]
  iR <- idx[x[idx] >= c0]
  if (length(iL) < 10 || length(iR) < 10) return(NULL)
  
  # Build clean data with explicit 'xc'
  dfL <- data.frame(y = y[iL], xc = xc[iL], w = tri((x[iL] - c0) / h))
  dfR <- data.frame(y = y[iR], xc = xc[iR], w = tri((x[iR] - c0) / h))
  
  fitL <- lm(y ~ xc, data = dfL, weights = w)
  fitR <- lm(y ~ xc, data = dfR, weights = w)
  
  # Grids that EXCLUDE the exact cutoff so the line breaks at c0
  gridL <- data.frame(x = seq(c0 - h, c0, length.out = 200)[-200])
  gridR <- data.frame(x = seq(c0, c0 + h, length.out = 200)[-1])
  
  # Predict using 'xc' that matches the model term name
  predL <- data.frame(
    x = gridL$x,
    yhat = predict(fitL, newdata = data.frame(xc = gridL$x - c0)),
    h = paste0("h = ", round(h, 3)),
    side = "left"
  )
  predR <- data.frame(
    x = gridR$x,
    yhat = predict(fitR, newdata = data.frame(xc = gridR$x - c0)),
    h = paste0("h = ", round(h, 3)),
    side = "right"
  )
  
  rbind(predL, predR)
}

preds <- purrr::map_dfr(h_show, make_pred)

# binscatter to reduce noise near the cutoff
win <- abs(x - c0) <= max(h_show)
nb  <- 60
br  <- seq(min(x[win]), max(x[win]), length.out = nb + 1)
bins <- data.frame(x = x[win], y = y[win]) |>
  dplyr::mutate(bin = cut(x, breaks = br, include.lowest = TRUE)) |>
  dplyr::group_by(bin) |>
  dplyr::summarise(x_bin = mean(x), y_bin = mean(y), .groups = "drop")

p_vis <- ggplot2::ggplot() +
  ggplot2::geom_point(data = data.frame(x = x[win], y = y[win]),
                      ggplot2::aes(x = x, y = y), alpha = 0.07, size = 0.6) +
  ggplot2::geom_point(data = bins, ggplot2::aes(x = x_bin, y = y_bin),
                      size = 1.6, color = "grey20") +
  ggplot2::geom_line(data = preds,
                     ggplot2::aes(x = x, y = yhat, color = h,
                                  group = interaction(h, side)),
                     linewidth = 1.1) +
  ggplot2::geom_vline(xintercept = c0, linetype = "dashed") +
  ggplot2::labs(
    title = "Local linear fits for different bandwidths (single sample)",
    subtitle = "Smaller h: less bias, more noise • Larger h: smoother, more bias",
    x = "X (running variable)", y = "Y", color = "Bandwidth"
  ) +
  ggplot2::theme_minimal(base_size = 12)

p_vis
