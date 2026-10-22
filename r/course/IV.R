# Lecture 5 (IV) course script: IV by hand vs 2SLS, controls via FWL, and Acemoglu et al. (2001).
# Changes for this repo: data path, install.packages commented out, print(p) added.
# Run from the repository folder (setwd() to the downloaded learn_econometrics folder), so data/course/ is found.
# ------------------------------------------------------
# One instrument, one endogenous regressor (NO controls)
# De-meaned Wald/MoM = 2SLS (through origin)
# ------------------------------------------------------
# install.packages("AER") # For IVreg (run once)

set.seed(42)
n <- 2000
beta_true <- 1.5

# Instrument (exogenous)
Z <- rnorm(n)

# Build X with correlation to Z (relevance)
rho <- 0.5
X  <- rho*Z + sqrt(1 - rho^2) * rnorm(n)

# Create endogeneity by correlating u with X
eps <- rnorm(n)
u   <- 0.6*X + eps   # so E[X*u] != 0
Y   <- beta_true*X + u

# -------- De-mean (absorb intercept) --------
Zc <- Z - mean(Z)
Xc <- X - mean(X)
Yc <- Y - mean(Y)

# -------- Method of Moments / Wald --------
beta_iv_wald <- sum(Zc * Yc) / sum(Zc * Xc)

# -------- 2SLS on de-meaned data (through origin) --------
# First stage: Xc on Zc (no intercept)
fs <- lm(Xc ~ Zc + 0)
Xhat <- fitted(fs)

# Second stage: Yc on Xhat (no intercept)
ss <- lm(Yc ~ Xhat + 0)
beta_iv_2sls <- coef(ss)[1]

# -------- OLS for comparison (with intercept) --------
beta_ols <- coef(lm(Y ~ X))[2]

# -------- IVreg -------- 
beta_ivreg <- NA
if (requireNamespace("AER", quietly = TRUE)) {
  ivfit <- AER::ivreg(Y ~ X| Z)
  beta_ivreg <- coef(ivfit)["X"]
}

# -------- Print results --------
cat("\n=== One Z, one X — de-meaned example (no controls) ===\n")
cat(sprintf("True beta           : %7.3f\n", beta_true))
cat(sprintf("IV Wald (demeaned)  : %7.3f\n", beta_iv_wald))
cat(sprintf("2SLS (demeaned)     : %7.3f\n", beta_iv_2sls))
# cat(sprintf("OLS (biased)        : %7.3f\n", beta_ols))
cat(sprintf("IVreg 2SLS (no de-meaning)      : %7.3f\n", beta_ivreg))

cat("\nEquality checks:\n")
cat("all.equal(Wald, 2SLS): ",
    tryCatch(all.equal(beta_iv_wald, beta_iv_2sls, tolerance = 1e-12),
             error = function(e) e$message),
    "\n")

# -------- First-stage strength (on de-meaned data; same as with intercept) --------
fs_int <- lm(X ~ Z)  # just to show F-stat in the usual parametrization with intercept
F1 <- summary(fs_int)$fstatistic[1]
R2 <- summary(fs_int)$r.squared
cat(sprintf("\nFirst-stage diagnostics: F = %6.2f,  R^2 = %.3f\n", F1, R2))

Xhat_int <- fitted(fs_int)
ss_int <- lm(Y ~ Xhat_int + 0)
beta_iv_2sls_int <- coef(ss_int)[1]

# ------------------------------------------------------------
# IV with controls W: Cov(Z_perp, Y_perp) / Cov(Z_perp, X_perp)
# equals 2SLS with W in both stages (Frisch–Waugh–Lovell).
# ------------------------------------------------------------

set.seed(42)
n <- 2000

# --- Generate controls W (include an intercept via lm()) ---
W1 <- rnorm(n)
W2 <- rbinom(n, 1, 0.4)

# --- Generate a valid instrument Z (relevant, exogenous) ---
# Z depends on W but is independent of the structural error u.
vz <- rnorm(n)
Z  <- 0.8*W1 - 0.5*W2 + vz

# --- Create endogeneity in X via correlation with u ---
ex  <- rnorm(n)              # shock to X
eps <- rnorm(n)              # shock to u
u   <- 0.6*ex + eps          # structural error u correlates with ex -> endogeneity
beta_true <- 1.5

# First stage relationship (relevant instrument):
X <- 0.9*Z + 0.7*W1 - 0.4*W2 + ex

# Outcome:
Y <- beta_true*X + 1.0*W1 - 0.5*W2 + u

# Convenience data frame
dat <- data.frame(Y, X, Z, W1, W2)

# ------------------------------------------------------------
# 1) Residualize (partial out) W using FWL: A_perp = residuals(lm(A ~ W))
# ------------------------------------------------------------
Y_perp <- resid(lm(Y ~ W1 + W2, data = dat))
X_perp <- resid(lm(X ~ W1 + W2, data = dat))
Z_perp <- resid(lm(Z ~ W1 + W2, data = dat))

# Covariance-ratio IV with controls:
beta_cov <- cov(Z_perp, Y_perp) / cov(Z_perp, X_perp)

# ------------------------------------------------------------
# 2) Show it equals 2SLS on residuals (stage 1 on Z_perp, stage 2 on Y_perp ~ Xhat_perp)
# ------------------------------------------------------------
# First stage (through origin after residualizing):
fs_res <- lm(X_perp ~ Z_perp + 0)
Xhat_perp <- fitted(fs_res)

# Second stage (through origin):
ss_res <- lm(Y_perp ~ Xhat_perp + 0)
beta_2sls_resid <- coef(ss_res)[1]

# ------------------------------------------------------------
# 3) Show it equals standard 2SLS with W in both stages (no manual residuals)
# ------------------------------------------------------------
# First stage: X on Z and W
fs_full <- lm(X ~ Z + W1 + W2, data = dat)
Xhat_full <- fitted(fs_full)

# Second stage: Y on Xhat and W
ss_full <- lm(Y ~ Xhat_full + W1 + W2, data = dat)
beta_2sls_full <- coef(ss_full)["Xhat_full"]

# ------------------------------------------------------------
# 4) OLS for comparison (should be biased due to endogeneity)
# ------------------------------------------------------------
ols <- lm(Y ~ X + W1 + W2, data = dat)
beta_ols <- coef(ols)["X"]

# ------------------------------------------------------------
# 5) (Optional) Compare to ivreg from AER if available
# ------------------------------------------------------------
beta_ivreg <- NA
if (requireNamespace("AER", quietly = TRUE)) {
  ivfit <- AER::ivreg(Y ~ X + W1 + W2 | Z + W1 + W2, data = dat)
  beta_ivreg <- coef(ivfit)["X"]
}

# ------------------------------------------------------------
# Print results
# ------------------------------------------------------------
cat("\n=== IV with controls: covariance-ratio vs 2SLS ===\n")
cat(sprintf("True beta                     : %6.3f\n", beta_true))
cat(sprintf("Cov-ratio (residualized)      : %6.3f\n", beta_cov))
cat(sprintf("2SLS on residuals (FWL)       : %6.3f\n", beta_2sls_resid))
cat(sprintf("2SLS (W in both stages)       : %6.3f\n", beta_2sls_full))
# cat(sprintf("OLS (biased)                  : %6.3f\n", beta_ols))
if (!is.na(beta_ivreg)) cat(sprintf("AER::ivreg (if installed)     : %6.3f\n", beta_ivreg))

cat("\nall.equal(Cov-ratio, 2SLS FWL): ",
    tryCatch(all.equal(beta_cov, beta_2sls_resid, tolerance = 1e-10), error=function(e) e$message), "\n")
cat("all.equal(Cov-ratio, 2SLS full): ",
    tryCatch(all.equal(beta_cov, beta_2sls_full, tolerance = 1e-10), error=function(e) e$message), "\n")

# ------------------------------------------------------------
# 6) Binary instrument variant (same logic with controls)
# ------------------------------------------------------------
# Make Z binary but keep exogeneity (monotone transform + noise, then threshold)
Zb <- as.numeric(scale(Z + rnorm(n, sd=0.5)) > 0)
dat$Zb <- Zb

# Residualize on controls:
Zb_perp <- resid(lm(Zb ~ W1 + W2, data = dat))   # becomes non-binary after residualization
# Cov-ratio with binary instrument (controls partialled out):
beta_cov_bin <- cov(Zb_perp, Y_perp) / cov(Zb_perp, X_perp)

# 2SLS with binary instrument (standard two-stage with controls):
fs_bin <- lm(X ~ Zb + W1 + W2, data = dat)
Xhat_bin <- fitted(fs_bin)
ss_bin <- lm(Y ~ Xhat_bin + W1 + W2, data = dat)
beta_2sls_bin <- coef(ss_bin)["Xhat_bin"]

cat("\n=== Binary instrument (with controls) ===\n")
cat(sprintf("Cov-ratio (residualized)      : %6.3f\n", beta_cov_bin))
cat(sprintf("2SLS (binary Z, with controls): %6.3f\n", beta_2sls_bin))
cat("all.equal(Binary cov-ratio, 2SLS): ",
    tryCatch(all.equal(beta_cov_bin, beta_2sls_bin, tolerance = 1e-10), error=function(e) e$message), "\n")

# In this practical example we will replicate Acemoglu et. al (2001)
# The Colonial Origins of Comparative Development: An Empirical Investigation.
# The American Economic Review 91 (5), pp. 1369-1401

# --- Packages ---
# install.packages(c("haven","dplyr","ggplot2","sandwich","lmtest","AER","broom","ggrepel"))
library(haven)
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(AER)
library(broom)
library(ggrepel)

# --- Load data ---
dat  <- read_dta("data/course/Colonial_origins.dta") %>% as_factor()

# --- Quick summary stats
dat %>% summary()

# If you prefer a tidy table of means/sd/n (numeric vars only):
dat %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(),
                   list(mean = ~mean(. , na.rm = TRUE),
                        sd   = ~sd(  . , na.rm = TRUE),
                        n    = ~sum(!is.na(.)) ),
                   .names = "{.col}_{.fn}"))

# --- Histograms: loggdp and risk ---
ggplot(dat, aes(x = loggdp)) +
  geom_histogram(bins = 15) +
  labs(title = "Distribution of log GDP",
       x = "log(GDP per capita)", y = "Frequency") +
  theme_bw()

ggplot(dat, aes(x = risk)) +
  geom_histogram(bins = 15) +
  labs(title = "Protection against expropriation risk",
       x = "Risk index", y = "Frequency") +
  theme_bw()

# --- Scatter: institutions (risk) vs performance (loggdp) ---
p <- ggplot(dat, aes(x = loggdp, y = risk)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Institutions vs. Economic Performance",
       x = "log(GDP per capita)",
       y = "Protection against expropriation risk") +
  theme_bw()
print(p)

# --- OLS: loggdp on risk with robust (HC1) SEs ---
ols <- lm(loggdp ~ risk, data = dat)
coeftest(ols, vcov. = vcovHC(ols, type = "HC1"))

# Interpret coefficient: % effect for a 1-point increase in risk
beta_risk <- coef(ols)["risk"]
( exp(beta_risk) - 1 ) * 100   # ≈ percent change in GDP

# --- Instrument relevance: risk on logmort0 (first stage) ---
fs <- lm(risk ~ logmort0, data = dat)
coeftest(fs, vcov. = vcovHC(fs, type = "HC1"))

# First-stage scatter with fitted line
ggplot(dat, aes(x = logmort0, y = risk)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "First Stage: Settler Mortality → Institutions",
       x = "log(settler mortality)", y = "Protection against expropriation risk") +
  theme_bw()

# --- Manual 2-step (predicted instrumented values) ---
dat <- dat %>% mutate(z_hat = predict(fs))  # z^ = X*delta^ (here X is logmort0)
ss <- lm(loggdp ~ z_hat, data = dat)
coeftest(ss, vcov. = vcovHC(ss, type = "HC1"))

# NOTE: The above gets point estimates but SEs are not 2SLS-correct.
# Proper 2SLS with correct SEs below.

# --- Proper IV (2SLS) with robust SEs ---
iv1 <- ivreg(loggdp ~ risk | logmort0, data = dat)
# "first" in Stata prints first-stage stats; in R:
summary(iv1, diagnostics = TRUE)        # shows first-stage F and diagnostics
coeftest(iv1, vcov. = vcovHC(iv1, type = "HC1"))

# --- IV with additional covariates (latitude, africa, asia, other) ---
# (Assumes those columns exist in the dataset)
iv2 <- ivreg(loggdp ~ risk + latitude + africa + asia + other |
               logmort0 + latitude + africa + asia + other,
             data = dat)
summary(iv2, diagnostics = TRUE)
coeftest(iv2, vcov. = vcovHC(iv2, type = "HC1"))