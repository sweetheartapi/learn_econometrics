# Lab 6: a parental-leave reform as a regression discontinuity
library(rdrobust); library(rddensity); library(ggplot2)
set.seed(6)
n <- 40000                                      # births in one year
days <- sample(-180:180, n, replace = TRUE)      # birth date relative to 1 July (cut-off = 0)
long_leave <- as.numeric(days >= 0)              # sharp: born on/after cut-off -> 2-year leave
p <- 0.32 + 0.00015 * days + 0.045 * long_leave  # true jump = 4.5 percentage points
birth36 <- rbinom(n, 1, p)                       # another child within 36 months?
d <- data.frame(days, long_leave, birth36)

rd <- rdrobust(d$birth36, d$days, c = 0)         # local linear, MSE-optimal bandwidth
summary(rd)

# Manipulation check: is there a jump in the density of birth dates?
round(rddensity(d$days, c = 0)$test$p_jk, 3)     # p-value of the density test

# Bandwidth sensitivity: same model, different windows (in days)
suppressWarnings(t(sapply(c(30, 60, 90, 150), function(h) {
  r <- rdrobust(d$birth36, d$days, c = 0, h = h)
  round(c(h = h, estimate = r$coef[1], ci_low = r$ci[3, 1], ci_high = r$ci[3, 2]), 3)})))

# Fuzzy version: eligibility only raises take-up (90 % vs 10 %)
took_leave <- rbinom(n, 1, ifelse(long_leave == 1, 0.9, 0.1))
birth36_f  <- rbinom(n, 1, 0.32 + 0.00015 * days + 0.05 * took_leave)  # true effect 0.05
fz <- suppressWarnings(rdrobust(birth36_f, days, c = 0, fuzzy = took_leave))
round(c(fuzzy_estimate = fz$coef[1], ci_low = fz$ci[3, 1], ci_high = fz$ci[3, 2]), 3)

# RD plot: monthly bins, separate lines on each side
bins <- aggregate(birth36 ~ cut(days, seq(-180, 180, 15), right = FALSE), data = d, mean)
bins$mid <- seq(-172.5, 172.5, 15)
ggplot(bins, aes(mid, birth36)) + geom_point() +
  geom_smooth(data = d, aes(days, birth36, group = long_leave), method = "lm",
              formula = y ~ x, se = FALSE, colour = "black") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Birth date (days from 1 July)", y = "Share with another child") + theme_minimal()
