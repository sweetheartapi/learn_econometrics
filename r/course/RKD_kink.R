# Lecture 8 (extras) course script: sharp and fuzzy regression kink design with lm() and a triangular kernel.
# Unchanged.
# ============
# RKD via lm()
# ============
# install.packages(c("ggplot2","dplyr"), Ncpus=2)
library(ggplot2)
library(dplyr)

set.seed(123)

# 1) Running variable and cutoff
n  <- 5000
c0 <- 0
x  <- runif(n, -1, 1)
Xc <- x - c0

# 2) Policy schedule T(x): up on left, down on right -> triangle
bL <-  1.20   # slope for x < c0
bR <- -1.00   # slope for x >= c0
T_det <- ifelse(Xc < 0, bL*Xc, bR*Xc)  # continuous at 0

# 3) Untreated outcome with zero derivative at cutoff
f <- function(z) 0.30*z^2
tau_true <- 1.5

# 4) Outcomes: sharp vs fuzzy + first stage
y_sharp <- tau_true * T_det + f(x) + rnorm(n, sd = 0.35)
T_obs   <- T_det + rnorm(n, sd = 0.5)
y_fuzzy <- tau_true * T_obs + f(x) + rnorm(n, sd = 0.35)

# 5) Plot the data
# Choose which series to scatter: "sharp", "fuzzy", or "firststage"
which_series <- "sharp"

plot_df <- data.frame(
  Xc = Xc,
  y  = dplyr::case_when(
    which_series == "sharp"      ~ y_sharp,
    which_series == "fuzzy"      ~ y_fuzzy,
    which_series == "firststage" ~ T_obs,
    TRUE                         ~ y_sharp
  ),
  side = ifelse(Xc < 0, "left", "right")
)

# Unbinned scatter over the whole dataset
ggplot(plot_df, aes(x = Xc, y = y)) +
  geom_point(alpha = 0.25, size = 1) +        # all raw points
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  labs(
    title = "RKD triangle: raw scatter (no fits, no binning)",
    subtitle = paste("Series:", which_series, "— cutoff at 0"),
    x = "X - c",
    y = ifelse(which_series == "firststage", "Treatment intensity (T)", "Outcome (Y)")
  ) +
  theme_minimal(base_size = 12)

# 6) Helper: RKD local piecewise fit via lm() with triangular kernel
rkd_lm <- function(y, x, c = 0, h = 0.3) {
  Xc    <- x - c
  Right <- as.integer(Xc >= 0)
  keep  <- abs(Xc) <= h
  
  u <- abs(Xc)/h
  w <- pmax(0, 1 - u)  # triangular kernel (0 outside |Xc|>h)
  
  dat <- data.frame(y=y[keep], Xc=Xc[keep], Right=Right[keep], w=w[keep])
  fit <- lm(y ~ Xc + Right + Xc:Right, data = dat, weights = w)
  
  cf <- coef(fit)
  aL <- unname(cf["(Intercept)"])
  bLh <- unname(cf["Xc"])
  aR <- unname(aL + cf["Right"])
  bRh <- unname(bLh + cf["Xc:Right"])
  
  list(fit=fit, data_used=dat, aL=aL, bL=bLh, aR=aR, bR=bRh,
       kink=unname(cf["Xc:Right"]))
}

h <- 0.3

fitY_sharp <- rkd_lm(y_sharp, x, c0, h)
fitY_fuzzy <- rkd_lm(y_fuzzy, x, c0, h)
fitT_fuzzy <- rkd_lm(T_obs,   x, c0, h)

deltaY_sharp <- fitY_sharp$kink
deltaT_true  <- (bR - bL)
tau_hat_sharp <- deltaY_sharp / deltaT_true

deltaY_fuzzy <- fitY_fuzzy$kink
deltaT_fuzzy <- fitT_fuzzy$kink
tau_hat_fuzzy <- deltaY_fuzzy / deltaT_fuzzy

cat("\n===== RKD via lm(), triangle shape =====\n")
cat(sprintf("True tau                     : %.3f\n", tau_true))
cat(sprintf("Sharp RKD:  ΔY' = %6.3f, ΔT' (known) = %6.3f  => taû = %6.3f\n",
            deltaY_sharp, deltaT_true, tau_hat_sharp))
cat(sprintf("Fuzzy RKD:  ΔY' = %6.3f, ΔT' (est.)  = %6.3f  => taû = %6.3f\n",
            deltaY_fuzzy, deltaT_fuzzy, tau_hat_fuzzy))

# 7) ggplot helpers: binned scatter + fitted left/right lines from the lm
binned_scatter <- function(x, y, bins = 60) {
  br   <- seq(min(x), max(x), length.out = bins + 1)
  mids <- 0.5 * (br[-1] + br[-length(br)])
  grp  <- cut(x, br, include.lowest = TRUE)
  mu   <- tapply(y, grp, mean)
  data.frame(Xc = mids - 0, y = as.numeric(mu)) |>
    filter(is.finite(y))
}

fit_lines_df <- function(fit_obj) {
  d <- fit_obj$data_used
  aL <- fit_obj$aL; bL <- fit_obj$bL
  aR <- fit_obj$aR; bR <- fit_obj$bR
  xL <- seq(min(d$Xc), 0, length.out = 200)
  xR <- seq(0, max(d$Xc), length.out = 200)
  left  <- data.frame(Xc = xL, y = aL + bL*xL, side = "left")
  right <- data.frame(Xc = xR, y = aR + bR*xR, side = "right")
  rbind(left, right)
}

plot_rkd <- function(x, y, fit_obj, title, h, bins = 60) {
  scat <- binned_scatter(x, y, bins)
  lines <- fit_lines_df(fit_obj)
  ggplot() +
    geom_point(data = scat, aes(x = Xc, y = y), alpha = 0.7, size = 1.6) +
    geom_line(data = lines, aes(x = Xc, y = y, group = side), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = title,
      subtitle = sprintf("Local piecewise linear (triangular kernel, h = %.2f); cutoff at 0", h),
      x = "X - c",
      y = "Outcome"
    ) +
    theme_minimal(base_size = 12)
}

p1 <- plot_rkd(x, y_sharp, fitY_sharp, "Sharp RKD (triangle): Y vs X", h)
p2 <- plot_rkd(x, y_fuzzy, fitY_fuzzy, "Fuzzy RKD (triangle): Y vs X", h)
p3 <- plot_rkd(x, T_obs,   fitT_fuzzy, "Fuzzy RKD first stage: T vs X", h)

print(p1); print(p2); print(p3)