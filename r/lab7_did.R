# Lab 7: difference-in-differences, event study and staggered adoption
library(sandwich); library(lmtest); library(dplyr); library(ggplot2)
set.seed(7)

# (a) 2x2: sugar tax in city A, not in city B; 100 shops each, before and after
shops <- expand.grid(shop = 1:200, post = 0:1)
shops$cityA <- as.numeric(shops$shop <= 100)
shop_fe <- rnorm(200, 0, 3)
shops$sales <- 22 - 2 * shops$cityA - 1 * shops$post - 4 * shops$cityA * shops$post +
               shop_fe[shops$shop] + rnorm(400)
m <- lm(sales ~ cityA * post, data = shops)
coeftest(m, vcov = vcovCL(m, cluster = ~shop))        # clustered by shop

# (b) Event study: 200 regions, half treated from period 0, effect grows over time
es <- expand.grid(unit = 1:200, t = -5:5)
es$treated <- as.numeric(es$unit <= 100)
es$y <- rnorm(200)[es$unit] + 0.3 * es$t + es$treated * pmax(es$t + 1, 0) + rnorm(nrow(es), 0, 0.5)
for (k in setdiff(-5:5, -1))                          # one dummy per event time, k = -1 left out
  es[[paste0("k", ifelse(k < 0, paste0("m", -k), k))]] <- as.numeric(es$treated == 1 & es$t == k)
ev <- lm(as.formula(paste("y ~ factor(unit) + factor(t) +",
                          paste(grep("^k", names(es), value = TRUE), collapse = " + "))), data = es)
ct <- coeftest(ev, vcov = vcovCL(ev, cluster = ~unit))[grep("^k", rownames(coef(summary(ev)))), 1:2]
plot_df <- data.frame(k = setdiff(-5:5, -1), est = ct[, 1], se = ct[, 2])
round(t(plot_df), 2)
ggplot(rbind(plot_df, data.frame(k = -1, est = 0, se = 0)), aes(k, est)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = -0.5, linetype = 2) +
  geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se)) +
  labs(x = "Periods relative to treatment (k)", y = "Estimated effect") + theme_minimal()

# (c) Staggered adoption: cohort A treated from t = 3, B from t = 6, N never; effects grow
st <- expand.grid(unit = 1:150, t = 1:8)
st$g <- c(3, 6, Inf)[ceiling(st$unit / 50)]
st$D <- as.numeric(st$t >= st$g)
st$effect <- ifelse(st$D == 1, 2 * (st$t - st$g + 1), 0)   # 2, 4, 6, ... after adoption
st$y <- rnorm(150)[st$unit] + 0.5 * st$t + st$effect + rnorm(nrow(st), 0, 0.5)
twfe <- coef(lm(y ~ factor(unit) + factor(t) + D, data = st))["D"]

att_gt <- function(gg, tt) {                          # clean 2x2: cohort gg vs not-yet/never treated at tt
  base <- gg - 1
  ctrl <- st$g > tt
  chg <- function(sel) mean(st$y[sel & st$t == tt]) - mean(st$y[sel & st$t == base])
  chg(st$g == gg) - chg(ctrl)
}
cells <- do.call(rbind, lapply(c(3, 6), function(gg)
  data.frame(g = gg, t = gg:8, att = sapply(gg:8, function(tt) att_gt(gg, tt)))))
true_avg <- mean(st$effect[st$D == 1])
round(cells, 2)
round(c(TWFE = unname(twfe), clean_average = mean(cells$att), true_average = true_avg), 2)
