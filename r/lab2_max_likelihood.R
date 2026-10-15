# Lab 2: maximum likelihood by hand, and with glm()
set.seed(2)
wait <- rexp(100, rate = 1/5)          # 100 waiting times, true mean 5 minutes

# Negative log-likelihood of the exponential with mean lambda
nll <- function(lambda) -sum(dexp(wait, rate = 1/lambda, log = TRUE))
fit <- optim(4, nll, method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
c(mle = fit$par, sample_mean = mean(wait))

# Standard error from the curvature (Hessian of the negative log-likelihood)
c(se_hessian = sqrt(1 / fit$hessian[1, 1]), se_formula = fit$par / sqrt(100))

# Logistic regression is also fitted by maximum likelihood
launch <- data.frame(temp = round(runif(23, 12, 27), 1))
p <- plogis(5 - 0.35 * launch$temp)
launch$fail <- rbinom(23, size = 6, prob = p)        # failed O-rings out of 6
m <- glm(cbind(fail, 6 - fail) ~ temp, family = binomial, data = launch)
round(summary(m)$coefficients, 3)
round(exp(coef(m)["temp"]), 3)        # odds multiplier per +1 degree
round(predict(m, data.frame(temp = c(10, 20)), type = "response"), 3)
logLik(m)
