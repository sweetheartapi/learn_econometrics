# Lab 8: complier profiles, a kink design and doubly robust DiD
library(rdrobust); library(ggplot2)
set.seed(8)

# (a) Profiling compliers: random offer z, take-up d, a characteristic (age)
n <- 20000
type <- sample(c("complier", "always", "never"), n, replace = TRUE, prob = c(0.2, 0.5, 0.3))
age  <- ifelse(type == "complier", 52.5, ifelse(type == "always", 55, 40)) + rnorm(n, 0, 8)
z <- rbinom(n, 1, 0.5)                                  # the offer, as good as random
d <- ifelse(type == "always", 1, ifelse(type == "never", 0, z))
pi_at <- mean(d[z == 0])                                # treated without the offer
pi_nt <- mean(d[z == 1] == 0)                           # untreated despite the offer
pi_co <- 1 - pi_at - pi_nt                              # = first stage
mu_at <- mean(age[d == 1 & z == 0]); mu_nt <- mean(age[d == 0 & z == 1])
mu_co <- (mean(age) - pi_at * mu_at - pi_nt * mu_nt) / pi_co
round(c(compliers = pi_co, always = pi_at, never = pi_nt, first_stage = mean(d[z == 1]) - mean(d[z == 0])), 3)
round(c(all = mean(age), always = mu_at, never = mu_nt, compliers = mu_co), 2)

# (b) A sharp kink: subsidy 200 below the threshold, falling 0.20 per euro above it
m <- 40000
x <- runif(m, -1000, 1000)                              # income minus the threshold (euros)
subsidy <- ifelse(x < 0, 200, 200 - 0.20 * x)
hours <- 120 + 0.004 * x - 0.05 * subsidy + rnorm(m, 0, 3)   # true effect: -0.05 hours per euro
k <- rdrobust(hours, x, c = 0, deriv = 1)               # change in slope at the threshold
round(c(slope_change = k$coef[1], ci_low = k$ci[3, 1], ci_high = k$ci[3, 2],
        effect_per_euro = k$coef[1] / -0.20), 4)
bins <- aggregate(hours ~ cut(x, seq(-1000, 1000, 50)), FUN = mean)
bins$mid <- seq(-975, 975, 50)
ggplot(bins, aes(mid, hours)) + geom_point() + geom_vline(xintercept = 0, linetype = 2) +
  geom_smooth(data = data.frame(x, hours, side = x >= 0), aes(x, hours, group = side),
              method = "lm", formula = y ~ x, se = FALSE, colour = "black") +
  labs(x = "Income minus threshold (EUR)", y = "Hours worked per month") + theme_minimal()

# (c) Doubly robust DiD by hand (two periods, change in outcome dy, true ATT = 2)
k2 <- 3000
xx <- rnorm(k2)
g  <- rbinom(k2, 1, plogis(-1 + 0.7 * xx + 0.6 * xx^2))     # who gets treated depends on x
dy <- 1 + 0.5 * xx + 0.8 * xx^2 + 2 * g + rnorm(k2)         # untreated trend also depends on x
dat <- data.frame(dy, xx, g)
dr_did <- function(trend, ps) {
  m0 <- lm(trend, data = dat[dat$g == 0, ])                  # trend model on controls only
  r  <- dat$dy - predict(m0, newdata = dat)                  # what the trend model can't explain
  e  <- fitted(glm(ps, family = binomial, data = dat))       # propensity score
  w  <- (e / (1 - e))[dat$g == 0]
  mean(r[dat$g == 1]) - sum(w * r[dat$g == 0]) / sum(w)
}
right_t <- dy ~ xx + I(xx^2); wrong_t <- dy ~ xx
right_p <- g ~ xx + I(xx^2);  wrong_p <- g ~ xx
round(c(naive = mean(dy[g == 1]) - mean(dy[g == 0]),
        both_right = dr_did(right_t, right_p), trend_wrong = dr_did(wrong_t, right_p),
        weights_wrong = dr_did(right_t, wrong_p), both_wrong = dr_did(wrong_t, wrong_p)), 3)
