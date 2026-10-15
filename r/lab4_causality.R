# Lab 4: potential outcomes, randomisation and selection bias
set.seed(4)
n  <- 1000
y0 <- rnorm(n, 50, 10)                 # outcome without treatment
effect <- rnorm(n, 5, 4)               # individual effects differ
y1 <- y0 + effect
c(ATE = mean(y1 - y0))

# (a) Random assignment: compare means
d <- sample(rep(0:1, n / 2))
y <- ifelse(d == 1, y1, y0)            # we only see one outcome per person
c(estimate = mean(y[d == 1]) - mean(y[d == 0]))

# Repeat the random assignment 1,000 times
est <- replicate(1000, {d <- sample(rep(0:1, n / 2))
  mean(y1[d == 1]) - mean(y0[d == 0])})
round(c(mean_of_estimates = mean(est), sd = sd(est),
        min = min(est), max = max(est)), 2)

# (b) Self-selection: people with high y0 choose treatment more often
d_self <- rbinom(n, 1, plogis((y0 - 50) / 5))
y_self <- ifelse(d_self == 1, y1, y0)
c(naive = mean(y_self[d_self == 1]) - mean(y_self[d_self == 0]),
  ATT   = mean((y1 - y0)[d_self == 1]),
  selection_bias = mean(y0[d_self == 1]) - mean(y0[d_self == 0]))
