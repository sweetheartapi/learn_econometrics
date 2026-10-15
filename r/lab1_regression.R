# Lab 1: wages, education and the omitted "ability"
library(sandwich); library(lmtest); library(ggplot2)
set.seed(1)
n <- 500
ability <- rnorm(n)                                   # unobserved in real life
educ    <- round(12 + 1.5 * ability + rnorm(n, 0, 2)) # able people study longer
exper   <- round(runif(n, 0, 30))
e       <- rnorm(n, 0, 0.15 + 0.01 * exper)           # noise grows with experience
logwage <- 1 + 0.08 * educ + 0.02 * exper + 0.10 * ability + e
d <- data.frame(logwage, educ, exper, ability)

long  <- lm(logwage ~ educ + exper + ability, data = d)  # if ability were measured
short <- lm(logwage ~ educ + exper, data = d)            # what we can actually run
round(rbind(long = coef(long)[2:3], short = coef(short)[2:3]), 3)

# Omitted variable bias formula: beta3 * gamma1
gamma1 <- coef(lm(ability ~ educ + exper, data = d))["educ"]
round(c(gap = unname(coef(short)["educ"] - coef(long)["educ"]),
        formula = unname(coef(long)["ability"] * gamma1)), 4)

# Robust (heteroskedasticity-consistent) standard errors
coeftest(short, vcov = vcovHC(short, type = "HC1"))

# Frisch-Waugh-Lovell: remove exper from educ, then a simple regression
educ_resid <- resid(lm(educ ~ exper, data = d))
round(coef(lm(logwage ~ educ_resid, data = d))[2], 4)

# Residual plot: look for curves or a fan shape
ggplot(data.frame(fitted = fitted(short), resid = resid(short), exper = d$exper),
       aes(exper, resid)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Experience (years)", y = "Residual") + theme_minimal()
