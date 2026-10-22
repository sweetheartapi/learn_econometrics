# Lecture 7 (DiD) course script (originally Castle.r): staggered DiD and an event study with Stevenson & Wolfers (2006) divorce-law data.
# Changes for this repo: data path; one typo in a comment.
# Run from the repository folder (setwd() to the downloaded learn_econometrics folder), so data/course/ is found.
# ================================================================================
# In this practical example we will explore data from Stevenson & Wolfers (2006)
# Bargaining in the Shadow of the Law: Divorce Laws and Family Distress.
# The Quarterly Journal of Economics 121.1, pp. 267–288.

# The dataset contains a panel of US states, with data about suicide mortality and
# introduction of no-fault divorce laws. The data can be exploited in a staggered
# DiD framework to analyze the effects on suicide mortality and homicide. 
# ================================================================================

# install.packages(c("haven","dplyr","fixest","ggplot2","broom","readr"), Ncpus = 2)
library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
library(broom)

# 0) Load data
# Your do-file does:  use Divorce.dta, clear
df <- read_dta("data/course/Divorce.dta")

# 1) Basic variables
# Panel ids (stfips, year) and outcomes:
#  - asmrs  : suicide mortality rate (women) used in the do-file regressions
#  - _nfd   : first year of no-fault/unilateral divorce adoption (cohort timing)
#  - post   : post-policy dummy
df <- df %>%
  mutate(
    stfips = as.integer(stfips),
    year   = as.integer(year),
    treat  = !is.na(`_nfd`),                         # g in C&S
    post   = if (!"post" %in% names(df)) {
      as.integer(treat & year >= `_nfd`)  
    } else as.integer(post)
  )

# 2) TWFE DiD
twfe <- feols(
  asmrs ~ post | stfips + year,                      # unit & time FE
  cluster = ~ stfips,
  data = df
)
cat("\n=== TWFE (unit & year FE, cluster by state) ===\n")
print(summary(twfe), n=5)

# 3) Event-time (reltime), cap leads/lags, choose base (ref = -1) 
# Build event time for treated; park controls at a neutral bucket 
df <- df %>%
  mutate(
    treat        = is.finite(`_nfd`),
    reltime_raw  = ifelse(treat, year - `_nfd`, NA_integer_),
    reltime_cap  = case_when(
      is.na(reltime_raw) ~ NA_integer_,
      reltime_raw < -15L ~ -15L,
      reltime_raw >  14L ~  14L,
      TRUE               ~  reltime_raw
    ),
    # Controls (and treated before we compute reltime) get a placeholder
    reltime_all = ifelse(is.na(reltime_cap), -999L, reltime_cap)
  )

# Event study with FE; keep only the real event-time dummies (-15:14)
# Controls have reltime_all = -999 and treat = FALSE, so they are KEPT but don't get dummies.
es <- feols(
  asmrs ~ i(reltime_all, treat, ref = -1, keep = -15:14) | stfips + year,
  cluster = ~ stfips,
  data = df
)

cat("\n=== Event-study with FE (ref = -1) ===\n")
print(etable(es), n=20)

# 2) Select ES coefficients regardless of naming variant
all_coefs <- names(coef(es))
sel <- grep("^reltime_all::(treat::|-?\\d+:treat$)", all_coefs, value = TRUE)

# 3) Extract numeric event-time k from either pattern
extract_k <- function(x) {
  if (grepl("^reltime_all::treat::", x)) {
    as.integer(sub("^reltime_all::treat::(-?\\d+)$", "\\1", x))
  } else if (grepl("^reltime_all::(-?\\d+):treat$", x)) {
    as.integer(sub("^reltime_all::(-?\\d+):treat$", "\\1", x))
  } else {
    NA_integer_
  }
}
k_vals <- vapply(sel, extract_k, integer(1))

# 4) Order by event time and make pretty x labels
ord_idx <- order(k_vals)
sel_ord <- sel[ord_idx]
k_ord   <- k_vals[ord_idx]
dict_vec <- setNames(as.character(k_ord), sel_ord)

# 5) Plot with fixest::coefplot
fixest::coefplot(
  es,
  dict     = dict_vec,          # map coef names -> numeric k on x-axis
  order    = sel_ord,           # ensure numeric ordering of k
  ci_level = 0.95,
  pt.pch   = 16
)