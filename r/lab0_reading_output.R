# Reading a regression table (same simulated wage data as Lab 1)
set.seed(1)
n <- 500
ability <- rnorm(n)
educ    <- round(12 + 1.5 * ability + rnorm(n, 0, 2))
exper   <- round(runif(n, 0, 30))
logwage <- 1 + 0.08 * educ + 0.02 * exper + 0.10 * ability +
           rnorm(n, 0, 0.15 + 0.01 * exper)
m <- lm(logwage ~ educ + exper)
summary(m)
confint(m)
