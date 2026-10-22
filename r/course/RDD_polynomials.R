# Lecture 6 (RDD) course script: global polynomials of order 1-4 vs local linear RD on one wiggly sample.
# Changes for this repo: print(polynomials) added; ggsave() commented out.
# ------------------------------------------------------------
# One RD graph with colored curves and a more complex DGP
# to reveal differences across polynomial orders
# ------------------------------------------------------------
set.seed(123)

library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)

# ----- 1) Simulate data (wiggly baseline + heteroskedastic noise) -----
n  <- 500
c0 <- 0
x  <- runif(n, -3, 3)

tau_true <- 1.0
# Baseline with curvature & oscillations (wiggles away from cutoff)
f <- function(x) 0.15*x + 0.35*x^2 - 0.02*x^3 + 0.004*x^4 - 0.00045*x^5 +
  0.6*sin(1.1*x) + 0.45*cos(2.0*x)

# Heteroskedastic noise: larger variance farther from cutoff
sigma <- 0.6 + 0.15*abs(x)
y <- f(x) + tau_true*(x >= c0) + rnorm(n, 0, sigma)

dat <- tibble(x = x, y = y, D = as.integer(x >= c0), x_c = x - c0)

# ----- 2) Global piecewise polynomials of different orders -----
Ks <- 1:4  # try higher orders to see wiggles

fit_global_poly <- function(K) {
  rhs <- paste0(
    "D + ",
    paste0("I(x_c^", 1:K, ")", collapse = " + "),
    " + ",
    paste0("D:I(x_c^", 1:K, ")", collapse = " + ")
  )
  mdl <- lm(as.formula(paste("y ~", rhs)), data = dat)
  tau_hat <- unname(coef(mdl)["D"])  # jump at cutoff since centered at c0
  list(K = K, model = mdl, tau_hat = tau_hat)
}
fits <- map(Ks, fit_global_poly)

# ----- 3) Local linear RD (reference), triangular kernel within bandwidth h -----
h <- 1.0
tri <- function(u) pmax(0, 1 - abs(u))
w  <- tri((dat$x - c0)/h)

fit_side <- function(side) {
  idx <- which(abs(dat$x - c0) <= h & if (side == "left") dat$x < c0 else dat$x >= c0)
  lm(y ~ x_c, data = dat[idx, ], weights = w[idx])
}
fit_ll_left  <- fit_side("left")
fit_ll_right <- fit_side("right")
tau_ll <- unname(predict(fit_ll_right, data.frame(x_c = 0)) -
                   predict(fit_ll_left,  data.frame(x_c = 0)))

# ----- 4) Predictions on a window — split left/right so lines break at c -----
x_min <- -2.5; x_max <- 2.5
grid_left  <- tibble(x = seq(x_min, c0, length.out = 350)[-350],  # exclude exact c0
                     D = 0L, x_c = x - c0, side = "left")
grid_right <- tibble(x = seq(c0,   x_max, length.out = 350)[-1],   # exclude exact c0
                     D = 1L, x_c = x - c0, side = "right")

# Global predictions (separate by side)
pred_global <- map_dfr(fits, function(fit) {
  bind_rows(
    tibble(
      x = grid_left$x,
      yhat = predict(fit$model, newdata = grid_left),
      method = paste0("Global poly K=", fit$K, " (estimate = ", round(fit$tau_hat, 3), ")"),
      side = "left",
      K = fit$K
    ),
    tibble(
      x = grid_right$x,
      yhat = predict(fit$model, newdata = grid_right),
      method = paste0("Global poly K=", fit$K, " (estimate = ", round(fit$tau_hat, 3), ")"),
      side = "right",
      K = fit$K
    )
  )
})

# Local linear predictions (already separate by side)
grid_ll_left  <- tibble(x = seq(c0 - h, c0, length.out = 250)[-250], x_c = seq(-h, 0, length.out = 250)[-250], side = "left")
grid_ll_right <- tibble(x = seq(c0, c0 + h, length.out = 250)[-1],   x_c = seq(0,  h, length.out = 250)[-1],   side = "right")
pred_ll <- bind_rows(
  tibble(x = grid_ll_left$x,  yhat = predict(fit_ll_left,  grid_ll_left),  method = paste0("Local linear (h=", h, ", estimate = ", round(tau_ll, 3), ")"), side = "left"),
  tibble(x = grid_ll_right$x, yhat = predict(fit_ll_right, grid_ll_right), method = paste0("Local linear (h=", h, ", estimate = ", round(tau_ll, 3), ")"), side = "right")
)

# ----- 5) Binscatter to clarify the signal near the cutoff -----
dat_win <- dat %>% filter(between(x, x_min, x_max))
nbins <- 60
breaks <- seq(x_min, x_max, length.out = nbins + 1)
bins <- dat_win %>%
  mutate(bin = cut(x, breaks = breaks, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(x_bin = mean(x), y_bin = mean(y), .groups = "drop")

# ----- 6) Plot: color = model, linetype = side; lines now break at cutoff -----
polynomials <- ggplot() +
  geom_point(data = dat %>% filter(between(x, x_min, x_max)),
             aes(x = x, y = y), alpha = 0.06, size = 0.6) +
  geom_point(data = bins, aes(x = x_bin, y = y_bin), size = 1.6, color = "grey20") +
  geom_line(
    data = pred_global,
    aes(x = x, y = yhat, color = method, group = interaction(method, side)),
    linewidth = 1.1
  ) +
  geom_line(
    data = pred_ll,
    aes(x = x, y = yhat, color = method, group = interaction(method, side)),
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c0, linetype = "dashed") +
  labs(
    x = "Running variable (X)", y = "Outcome (Y)",
    color = "Model", linetype = "Side"
  ) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right") +
  scale_color_viridis_d(option = "plasma", end = 0.9)

print(polynomials)
# ggsave("RD polynomials.pdf", polynomials, width = 9, height = 5, dpi = 300)