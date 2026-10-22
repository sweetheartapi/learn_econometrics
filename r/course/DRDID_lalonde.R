# Lecture 8 (extras) course script: doubly robust DiD by hand vs DRDID::drdid on the Lalonde/NSW data with CPS controls.
# Unchanged. Needs the DRDID package (the data nsw_long ship with it).
# =========================================================
# DR-DiD on DRDID::nsw_long — manual vs drdid()
# =========================================================
# install.packages(c("dplyr","tidyr","DRDID"), Ncpus = 2)
library(dplyr)
library(tidyr)
library(DRDID)

# 1) Load data and form Lalonde sample with CPS comparison group
data(nsw_long)

# Lalonde eval sample: experimental (NSW) + CPS controls
eval_lalonde_cps <- subset(nsw_long, treated == 0 | sample == 2)

# Keep only variables we need
eval_lalonde_cps <- eval_lalonde_cps %>%
  select(id, year, re, experimental, age, educ, black, married, nodegree, hisp, re74)

# 2) Manual DR-DiD (panel)
#    Convert to wide by year so we can form ΔY = Y(post) - Y(pre)
#    (Assumes two periods in this sample; drop ids missing a period.)
df_wide <- eval_lalonde_cps %>%
  mutate(year = as.integer(year)) %>%
  select(id, year, re, experimental, age, educ, black, married, nodegree, hisp, re74) %>%
  pivot_wider(names_from = year, values_from = re, names_prefix = "Y") %>%
  # keep those observed in both periods
  filter(!is.na(Y1975), !is.na(Y1978)) %>%
  # de-duplicate (one row per id)
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    D   = as.integer(experimental),     # treatment indicator (NSW experimental group)
    dY  = Y1978 - Y1975                       # outcome change
  )

# Propensity score (logit) on your covariates
ps_fit <- glm(D ~ age + educ + black + married + nodegree + hisp + re74,
              data = df_wide, family = binomial())
ehat <- pmin(pmax(predict(ps_fit, type = "response"), 1e-6), 1 - 1e-6)

# Stabilized, sample-normalized weights (panel flavor)
w1 <- df_wide$D / mean(df_wide$D)                                   # treated weight
num0 <- (1 - df_wide$D) * (ehat / (1 - ehat))                       # control numerator
w0 <- num0 / mean(num0[num0 > 0])                                   # normalize to mean 1 in controls

# Outcome regression for controls: ΔY | X using only D=0
or_fit <- lm(dY ~ age + educ + black + married + nodegree + hisp + re74,
             data = df_wide %>% filter(D == 0))
mhat0 <- predict(or_fit, newdata = df_wide)

# DR score: ATT = mean_treated(resid) - weighted_mean_controls(resid)
resid <- df_wide$dY - mhat0
ATT_manual <- mean(resid[df_wide$D == 1]) -
  sum(w0[df_wide$D == 0] * resid[df_wide$D == 0]) / sum(w0[df_wide$D == 0])

cat(sprintf("\nManual DR-DiD (panel) ATT: %.4f\n", ATT_manual))

# 3) DRDID package call on the same (long) data
out <- drdid(
  yname   = "re",
  tname   = "year",
  idname  = "id",
  dname   = "experimental",
  xformla = ~ age + educ + black + married + nodegree + hisp + re74,
  data    = eval_lalonde_cps,
  panel   = TRUE,
  estMethod = "trad"   # improved doubly robust (panel)
)

ATT_drdid <- if (!is.null(out$ATT)) out$ATT else out$att
cat(sprintf("drdid (imp, panel) ATT    : %.4f\n", ATT_drdid))

# 4) Side-by-side comparison
print(data.frame(
  estimator = c("Manual DR-DiD (panel)", "drdid::drdid (imp, panel)"),
  ATT = c(ATT_manual, ATT_drdid)
))