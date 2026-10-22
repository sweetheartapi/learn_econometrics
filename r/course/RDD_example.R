# Lecture 6 (RDD) course script: the 300-euro vaccination bonus at age 60 (Cerveny, Hellebrandt & Kravec 2025).
# Changes for this repo: data path; install.packages commented out; dat_subset defined (was missing);
# h_grid typo fixed (c(0.1, 0.5, 0,8) -> c(0.1, 0.5, 0.8)).
# Run from the repository folder (setwd() to the downloaded learn_econometrics folder), so data/course/ is found.
# =============================================================
# Conditional cash transfers and vaccination
# The dataset is from:
# Červený, J., Hellebrandt, T., & Kravec, P. (2025). 
# Financial incentives and COVID-19 vaccinations: 
# Evidence from a conditional cash transfer program. 
# Journal of health economics, 104, 103063
# A cash bonus of 300 euro was offered to individuals reaching
# age 60 or older by 31.12.2021, conditional on taking any
# available Covid-19 vaccine at the time. This creates a
# sharp discontinuity in the CCT eligibility. The policy allows
# to analyze the effects on vaccine uptake and health outcomes.
# =============================================================
# install.packages(c("readr","dplyr","ggplot2","rdrobust","rddensity","patchwork"))  # run once

library(readr)
library(dplyr)
library(ggplot2)
library(rdrobust)
library(rddensity)
library(patchwork)

# ------------------------------------------------------------
# 0) Load & basic checks
# ------------------------------------------------------------
fn <- "data/course/Vaccination.csv"  # semicolon-delimited
dat_raw <- read_delim(fn, delim = ";", show_col_types = FALSE)

# Expecting columns (case-insensitive friendly access below):
# birthdate, treat, cutoff_dist, vacc_during_bonus, died, log_costs, chronic, income
names(dat_raw)

# Harmonize names (lowercase)
dat_raw <- dat_raw %>% rename_with(tolower)

required <- c("birthdate","treat","cutoff_dist","vacc_during_bonus","died","log_costs","chronic","income")
stopifnot(all(required %in% names(dat_raw)))

# After reading the CSV and lowercasing column names
# Convert strings like "12feb1963" -> Date
if (!inherits(dat_raw$birthdate, "Date")) {
  # Ensure month abbreviations are parsed in a C/English locale
  old_loc <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_loc), silent = TRUE), add = TRUE)
  try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)

  dat_raw <- dat_raw %>%
    mutate(
      birthdate = as.Date(birthdate, format = "%d%b%Y")
    )
}

# Keep necessary vars, drop obvious missing
dat <- dat_raw %>%
  transmute(
    birthdate,
    treat = as.integer(treat),
    xc_days = as.numeric(cutoff_dist),   # running variable (distance to 60y cutoff, in days; 0 at cutoff)
    vacc = as.integer(vacc_during_bonus),
    died2022 = as.integer(died),
    log_costs_2022 = as.numeric(log_costs),
    chronic = as.integer(chronic),
    income = as.numeric(income),
    costs_2021 = as.numeric(costs_2021)
  ) %>%
  filter(is.finite(xc_days), !is.na(treat), !is.na(vacc))

# Centered running variable in *years* for nicer axes (keep days for estimation if you prefer)
dat$xc_years <- dat$xc_days / 365.25

summary(dat$xc_days)

# For all rdrobust calls, we’ll use X = xc_years with cutoff c = 0 (equivalent to days rescaled).

# ------------------------------------------------------------
# 1) RD plot: vaccination during the program (sharp RD)
# ------------------------------------------------------------
rdp <- rdplot(
  y = dat$vacc, x = dat$xc_years, c = 0,
  x.label = "Distance to 60y cutoff (years; 0 = eligible)",
  y.label = "Vaccinated during program",
  title   = "CCT program (200–300€): RD plot for vaccination"
)
print(rdp$rdplot)

# ------------------------------------------------------------
# 2) Density test: manipulation around the cutoff?
# ------------------------------------------------------------
rdd_out <- rddensity(X = dat$xc_years, c = 0)
rdplotdensity(rdd_out, dat$xc_years,
              title = "Density of running variable around cutoff",
              xlabel = "Distance to cutoff (years)")

# ------------------------------------------------------------
# 3) SHARP RD: effect of eligibility on vaccination (local linear, RBC CIs)
# ------------------------------------------------------------
fit_sharp <- rdrobust(y = dat$vacc, x = dat$xc_years, c = 0, p = 2, kernel = "triangular")
print(summary(fit_sharp))

# Same idea by hand: OLS with separate slopes, inside a 1-year window around the cutoff
dat_subset <- dat %>% filter(abs(xc_years) <= 1)
fit_lm <- lm(vacc ~ treat + xc_years + treat * xc_years, data = dat_subset)
print(summary(fit_lm)$coefficients)

h_grid <- c(0.1, 0.5, 0.8)
# ------------------------------------------------------------
# 4) Visualize bandwidth choice for the sharp RD (vaccination)
# ------------------------------------------------------------
tri <- function(u) pmax(0, 1 - abs(u))
make_local_line <- function(h, side = c("left","right")) {
  side <- match.arg(side)
  dsub <- dat %>% filter(abs(xc_years) <= h, if (side=="left") xc_years < 0 else xc_years >= 0)
  if (nrow(dsub) < 50) return(NULL)
  w <- tri(dsub$xc_years / h)
  fit <- lm(vacc ~ xc_years, data = dsub, weights = w)
  grid <- if (side=="left") {
    data.frame(xc_years = seq(-h, 0, length.out = 400)[-400])
  } else {
    data.frame(xc_years = seq(0,  h, length.out = 400)[-1])
  }
  grid$yhat  <- predict(fit, newdata = grid)
  grid$h_lab <- paste0("h = ", round(h,3))
  grid$side  <- side
  grid
}
preds <- dplyr::bind_rows(
  make_local_line(h_grid[1], "left"),  make_local_line(h_grid[1], "right"),
  make_local_line(h_grid[2], "left"),  make_local_line(h_grid[2], "right"),
  make_local_line(h_grid[3], "left"),  make_local_line(h_grid[3], "right")
)

# Bins inside widest window
win   <- max(h_grid)
nbins <- 60
brks  <- seq(-win, win, length.out = nbins + 1)
bins <- dat %>%
  filter(dplyr::between(xc_years, -win, win)) %>%
  mutate(bin = cut(xc_years, breaks = brks, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(x_bin = mean(xc_years), y_bin = mean(vacc), .groups = "drop")

p_bw <- ggplot() +
  geom_point(data = dat %>% filter(dplyr::between(xc_years, -win, win)),
             aes(x = xc_years, y = vacc), alpha = 0.08, size = 0.7) +
  geom_point(data = bins, aes(x = x_bin, y = y_bin), size = 1.8, color = "grey30") +
  geom_line(data = preds,
            aes(x = xc_years, y = yhat, color = h_lab, group = interaction(h_lab, side)),
            linewidth = 1.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Vaccination during program: local linear fits at different bandwidths",
       x = "Distance to cutoff (years)", y = "Pr(Vaccinated during program)",
       color = "Bandwidth") +
  theme_minimal(base_size = 12)
print(p_bw)

# -------------------------------------------------------------
# 5) Baseline covariate continuity (placebo): health costs 2021
# -------------------------------------------------------------
# This should be smooth at the cutoff (no program effect pre-period).
fit_placebo <- rdrobust(y = dat$costs_2021, x = dat$xc_years, c = 0, p = 1)
print(summary(fit_placebo))
rdplot(dat$costs_2021, dat$xc_years, c = 0,
       y.label = "Log costs (2021, baseline)",
       x.label = "Distance to cutoff (years)",
       title   = "Covariate balance: 2021 log costs (placebo)")

# --------------------------------------------------------------------
# 6) Downstream outcomes: ITT (reduced-form) and Fuzzy RD (local Wald)
# --------------------------------------------------------------------
# (a) Reduced-form "program effect" (effect of eligibility on outcomes)
#     Example: death in 2022
rf_died <- rdrobust(y = dat$died2022, x = dat$xc_years, c = 0, p = 1, kernel = "triangular")
cat("\nReduced-form (eligibility -> died in 2022):\n"); print(summary(rf_died))

# Health care utilization in 2022
rf_costs22 <- rdrobust(y = dat$log_costs_2022, x = dat$xc_years, c = 0, p = 1)
cat("\nReduced-form (eligibility -> costs in 2022):\n"); print(summary(rf_costs22))

# (b) First stage: jump in vaccination at cutoff (already estimated as 'fit_sharp')
cat("\nFirst stage (eligibility -> vaccination):\n"); print(summary(fit_sharp))

# (c) Fuzzy RD (local Wald): LATE of vaccination induced by the program on outcomes
#     Here we estimate the effect of *getting vaccinated during program* (complier LATE)
#     on, e.g., death in 2022, using eligibility as the instrument.
frd_died <- rdrobust(
  y = dat$died2022,
  x = dat$xc_years, c = 0, p = 1, kernel = "triangular",
  fuzzy = dat$vacc
)
cat("\nFuzzy RD (LATE of vaccination on death in 2022):\n"); print(summary(frd_died))

# If you have a post-program costs outcome, do the same:
frd_costs22 <- rdrobust(y = dat$log_costs_2022, x = dat$xc_years, c = 0, p = 1, kernel = "triangular",
                         fuzzy = dat$vacc)
cat("\nFuzzy RD (LATE of vaccination on health care utilization in 2022):\n"); print(summary(frd_costs22))

# ------------------------------------------------------------
# 7) Optional extras
# ------------------------------------------------------------
# (i) Donut RD (drop a narrow neighborhood if you fear manipulation exactly at 0)
# donut <- 14/365.25  # e.g., drop ±14 days
# fit_sharp_donut <- rdrobust(y = dat$vacc[abs(dat$xc_years) > donut],
#                             x = dat$xc_years[abs(dat$xc_years) > donut], c = 0)
# summary(fit_sharp_donut)

# (ii) Heterogeneity by chronic
# fit_sharp_chronic0 <- rdrobust(y = dat$vacc[dat$chronic==0], x = dat$xc_years[dat$chronic==0], c = 0)
# fit_sharp_chronic1 <- rdrobust(y = dat$vacc[dat$chronic==1], x = dat$xc_years[dat$chronic==1], c = 0)