# Lab 5: a charter-school lottery as an instrument
library(AER); library(ggplot2)
set.seed(5)
n <- 2000
motivation <- rnorm(n)                         # unobserved: affects attending AND scores
win <- rbinom(n, 1, 0.5)                       # Z: lottery offer, random
# D: winners attend unless very unmotivated; losers attend only if very motivated
attend <- ifelse(win == 1, motivation > -1, motivation > 1.5) * 1
score <- 0.5 * attend + 0.6 * motivation + rnorm(n)   # true effect = 0.5 SD
d <- data.frame(score, attend, win)

coef(lm(score ~ attend, data = d))["attend"]  # OLS: mixes effect with motivation

fs <- mean(d$attend[d$win == 1]) - mean(d$attend[d$win == 0])  # first stage
rf <- mean(d$score[d$win == 1])  - mean(d$score[d$win == 0])   # reduced form
round(c(first_stage = fs, reduced_form = rf, wald = rf / fs), 3)

iv <- ivreg(score ~ attend | win, data = d)    # 2SLS with correct SEs
summary(iv, diagnostics = TRUE)

# Weak vs strong instrument: 1,000 simulated studies each
sim <- function(strength) t(replicate(1000, {
  u <- rnorm(200); z <- rnorm(200)
  x <- strength * z + u + rnorm(200)          # u makes x endogenous
  y <- 0.5 * x + u + rnorm(200)
  c(est = cov(z, y) / cov(z, x),
    F = summary(lm(x ~ z))$fstatistic[[1]])}))
strong <- sim(1); weak <- sim(0.1)
round(rbind(strong = c(median_F = median(strong[, "F"]), median_est = median(strong[, "est"])),
            weak   = c(median_F = median(weak[, "F"]),   median_est = median(weak[, "est"]))), 2)
sims <- data.frame(est = c(strong[, "est"], weak[, "est"]),
                   type = rep(c("Strong instrument", "Weak instrument"), each = 1000))
ggplot(subset(sims, abs(est) < 4), aes(est)) +
  geom_histogram(bins = 60, fill = "grey60", colour = "white") +
  geom_vline(xintercept = 0.5, linetype = 2) + facet_wrap(~type) +
  labs(x = "IV estimate (true effect = 0.5)", y = "Count") + theme_minimal()
