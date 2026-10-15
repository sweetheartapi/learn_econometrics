# Lab 3: bootstrapping a median with the boot package
library(boot); library(ggplot2)
set.seed(3)
basket <- round(rlnorm(20, meanlog = 3.5, sdlog = 0.8), 1)  # 20 skewed basket values (EUR)
c(mean = mean(basket), median = median(basket))

med_fun <- function(data, idx) median(data[idx])  # statistic on a resample
b <- boot(basket, med_fun, R = 2000)
b                                                 # original, bias, std. error
boot.ci(b, type = c("norm", "perc", "bca"))

# Histogram of the 2,000 bootstrap medians
ggplot(data.frame(med = b$t[, 1]), aes(med)) +
  geom_histogram(bins = 30, fill = "grey60", colour = "white") +
  geom_vline(xintercept = b$t0, linetype = 2) +
  labs(x = "Bootstrap median (EUR)", y = "Count") + theme_minimal()

# Where the bootstrap fails: the sample maximum
x <- runif(50, 0, 10)
bmax <- boot(x, function(d, i) max(d[i]), R = 2000)
mean(bmax$t[, 1] == max(x))       # share of resamples that repeat the sample max
