# Lecture 7 (DiD) course script: TWFE, Goodman-Bacon decomposition and Callaway & Sant'Anna ATT(g,t) by hand vs did::att_gt.
# Changes for this repo: data path; install line lists all packages used.
# Run from the repository folder (setwd() to the downloaded learn_econometrics folder), so data/course/ is found.
# ===============================================================
# Manual Callaway–Sant'Anna ATT(g,t), verbose:
# - VARYING base (last pre s = g-1)
# - FIXED controls = not-yet-treated at t (carried back to s)
# - Repeated cross-sections (RC)
# Prints all cells and numbers used, then compares to did::att_gt
# and overall ATT to did::aggte(type="simple").
# Requires: id, Y, G, time  (D not needed)
# ===============================================================

# install.packages(c("readr","data.table","dplyr","did","fixest","bacondecomp"))  # run once
library(readr)
library(data.table)
library(dplyr)
library(did)
library(fixest)
library(bacondecomp)

# 0) Load
df <- fread("data/course/DiD.csv", sep = ";") %>%
  mutate(
    time = as.numeric(t),
    G    = ifelse(is.na(G) | G == 0, Inf, as.numeric(G))
  )

stopifnot(all(c("id","Y","time","G") %in% names(df)))

# (A) TWFE estimate
mod_twfe <- feols(Y ~ D | id + time, data = df, cluster = ~ id)
twfe_beta <- coef(mod_twfe)["D"]
cat("\n=== TWFE (feols) ===\n")
print(summary(mod_twfe, n=5))

# (B) Goodman–Bacon decomposition
# bacondecomp expects finite cohort times for adopters, and a separate "never-treated" indicator.
# We'll pass original variables: treat timing G*, with never-treated as NA for the function.

df_bacon <- df %>%
  mutate(G_bacon = ifelse(is.infinite(G), NA, G))

bd <- bacon(Y ~ D,
            data = df_bacon,
            id_var = "id",
            time_var = "time",
            # Default uses timing cohorts from data; never-treated are those with all D=0
            quietly = TRUE)

cat("\n=== Goodman–Bacon decomposition (first rows) ===\n")
print(head(bd, 10))

# Check that TWFE equals sum(weights * estimates)
bacon_weighted <- with(bd, sum(weight * estimate))
cat(sprintf("\nCheck: TWFE = %.6f   vs  Bacon weighted avg = %.6f\n",
            twfe_beta, bacon_weighted))

# Show contribution by comparison type
bacon_by_type <- bd %>%
  group_by(type) %>%
  summarise(
    n_comparisons = n(),
    weight_sum = sum(weight),
    # the weighted component of TWFE from this type
    weighted_contrib = sum(weight * estimate),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(weighted_contrib)))

# 1) Manual ATT(g,t) with prints
cohorts <- sort(unique(df$G[is.finite(df$G)]))
manual_rows <- list()

cat("\n==== Manual C&S (varying base, fixed NY@t) — VERBOSE OUTPUT ====\n")

for (g in cohorts) {
  # Last pre period s for cohort g
  s <- max(df$time[df$time < g], na.rm = TRUE)
  if (!is.finite(s)) next
  tvals <- sort(unique(df$time[df$time >= g]))
  
  cat(sprintf("\n---------------- Cohort g=%s | last pre s=%s ----------------\n", g, s))
  
  for (tt in tvals) {
    # Treated cells
    Treated_t <- df %>% filter(G == g, time == tt) %>% select(id, time, G, Y) %>% arrange(id)
    Treated_s <- df %>% filter(G == g, time == s ) %>% select(id, time, G, Y) %>% arrange(id)
    
    # FIXED control IDs = NY at t (G > t or never), carried back to s
    control_ids <- df %>% filter((G > tt | is.infinite(G)), time == tt) %>% pull(id) %>% unique()
    # Ensure cohort g IDs are not in controls
    control_ids <- setdiff(control_ids, df$id[df$G == g])
    
    NY_t <- df %>% filter(id %in% control_ids, time == tt) %>% select(id, time, G, Y) %>% arrange(id)
    NY_s <- df %>% filter(id %in% control_ids, time == s ) %>% select(id, time, G, Y) %>% arrange(id)
    
    # Four means (computed ONLY from these four tables)
    M_g_t  <- if (nrow(Treated_t)) mean(Treated_t$Y) else NA_real_
    M_g_s  <- if (nrow(Treated_s)) mean(Treated_s$Y) else NA_real_
    M_ny_t <- if (nrow(NY_t))      mean(NY_t$Y)      else NA_real_
    M_ny_s <- if (nrow(NY_s))      mean(NY_s$Y)      else NA_real_
    
    # ----- Print the exact content of the four cells -----
    cat(sprintf("\n[g=%s, t=%s] Treated@t rows:\n", g, tt)); print(Treated_t)
    cat(sprintf("\n[g=%s, s=%s] Treated@s rows:\n", g, s));  print(Treated_s)
    
    cat(sprintf("\n[g=%s, t=%s] NY@t (fixed NY at t) rows:\n", g, tt)); print(NY_t)
    cat(sprintf("\n[g=%s, s=%s] NY@s (same NY@t ids, at s) rows:\n", g, s)); print(NY_s)
    
    # ----- Print the four numbers and the ATT arithmetic -----
    cat("\nAverages USED (varying base — single last pre):\n")
    cat(sprintf("  E[Y | G=g, t=%s] = %.6f\n", tt, M_g_t))
    cat(sprintf("  E[Y | G=g, s=%s] = %.6f\n", s,  M_g_s))
    cat(sprintf("  E[Y | NY,  t=%s] = %.6f\n", tt, M_ny_t))
    cat(sprintf("  E[Y | NY,  s=%s] = %.6f\n", s,  M_ny_s))
    
    ATT <- (M_g_t - M_g_s) - (M_ny_t - M_ny_s)
    cat(sprintf("  ATT(g=%s, t=%s) = (%.6f - %.6f) - (%.6f - %.6f) = %.6f\n",
                g, tt, M_g_t, M_g_s, M_ny_t, M_ny_s, ATT))
    
    manual_rows[[length(manual_rows)+1]] <- tibble(
      g = g, t = tt, s_last_pre = s,
      n_treated_cell = nrow(Treated_t),
      EY_g_t  = M_g_t, EY_g_s  = M_g_s,
      EY_NY_t = M_ny_t, EY_NY_s = M_ny_s,
      ATT_manual_lastpre_fixedNY = ATT
    )
  }
}

att_manual <- bind_rows(manual_rows) %>% arrange(g, t)

cat("\n==== SUMMARY: Manual ATT(g,t) (varying base, fixed NY@t) ====\n")
print(att_manual, n = 200)

# 2) did::att_gt
att_cs <- att_gt(
  yname  = "Y",
  tname  = "time",
  idname = "id",
  gname  = "G",
  control_group = "notyettreated",
  panel  = TRUE,
  est_method = "reg",
  base_period = "varying",     # last pre (matches manual)
  allow_unbalanced_panel = TRUE,
  print_details = TRUE,
  data = df
)

att_cs_df <- tibble(g = att_cs$group, t = att_cs$t, ATT_did = att_cs$att) %>% arrange(g, t)

cat("\n==== did::att_gt ATT(g,t) ====\n")
print(att_cs_df, n = 200)

# 3) Comparison by (g,t)
comparison <- att_manual %>%
  left_join(att_cs_df, by = c("g","t")) %>%
  mutate(diff = ATT_manual_lastpre_fixedNY - ATT_did) %>%
  arrange(g, t)

cat("\n==== Manual vs. did by (g,t) ====\n")
print(comparison, n = 200)
cat(sprintf("\nMax |manual - did| = %g\n", max(abs(comparison$diff), na.rm = TRUE)))

# 4) Manual overall ATT (average over treatment periods)
# (A) Simple unweighted average over all (g,t) cells:
overall_manual_simple <- mean(att_manual$ATT_manual_lastpre_fixedNY, na.rm = TRUE)

# (B) Treated-cell–weighted average: weights = number of treated obs in each (g,t)
overall_manual_weighted <- with(att_manual,
                                sum(ATT_manual_lastpre_fixedNY * n_treated_cell, na.rm = TRUE) /
                                  sum(n_treated_cell, na.rm = TRUE)
)

cat("\n==== Manual overall ATT (across treatment periods) ====\n")
cat(sprintf("  Simple unweighted average : %.6f\n", overall_manual_simple))
cat(sprintf("  Treated-cell weighted avg : %.6f\n", overall_manual_weighted))

# 5) did::aggte overall comparison
overall_cs <- aggte(att_cs, type = "simple")  # simple average over (g,t)
overall_did <- if (!is.null(overall_cs$overall.att)) overall_cs$overall.att else {
  if (!is.null(overall_cs$att.egt)) mean(overall_cs$att.egt) else NA_real_
}

overall_comparison <- tibble(
  overall_manual_simple   = overall_manual_simple,
  overall_manual_weighted = overall_manual_weighted,
  overall_did_simple      = overall_did,
  diff_simple_vs_did      = overall_manual_simple - overall_did,
  diff_weighted_vs_did    = overall_manual_weighted - overall_did
)

cat("\n==== Overall ATT comparison ====\n")
print(overall_comparison)

# Optional: focus on a given cohort (e.g., g = 8)
if (8 %in% comparison$g) {
  cat("\nSubset for g = 8:\n")
  print(comparison %>% filter(g == 8), n = 200)
}