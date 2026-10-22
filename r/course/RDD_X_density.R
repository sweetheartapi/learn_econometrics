# Lecture 6 (RDD) course script: what the density of the running variable looks like with and without manipulation.
# Changes for this repo: scenario label made consistent ("Heaping / rounding") so the second half finds it; ggsave() commented out.
# ------------------------------------------------------------
# Visualizing running-variable continuity vs. manipulation
# ------------------------------------------------------------
set.seed(123)

library(ggplot2)
library(dplyr)

n <- 20000
c0 <- 0  # cutoff

# --- 1) Continuous / smooth density ---
x_cont <- rnorm(n, mean = 0, sd = 1)

# --- 2) Bunching to the right of the cutoff (sorting) ---
# Mix of a baseline normal + extra units clustered in [0, 0.4]
x_base <- rnorm(round(n*0.7), 0, 1)
x_bunch <- runif(round(n*0.3), min = 0, max = 0.4)
x_bunching <- c(x_base, x_bunch)

# --- 3) "Donut" hole: units near the cutoff strategically avoid it ---
x_donut <- rnorm(n, 0, 1)
x_donut <- x_donut[abs(x_donut - c0) > 0.15]           # drop those extremely near c
x_donut <- c(x_donut, rnorm(n - length(x_donut), 0, 1))# refill from tails (keeps a hole)

# --- 4) Heaping / rounding (discrete X) ---
x_heap_true <- rnorm(n, 0, 1)
x_heaped <- round(x_heap_true / 0.1) * 0.1             # round to nearest 0.1 (visible spikes)

# --- 5) Mass point at the cutoff ---
x_mass <- rnorm(n, 0, 1)
idx <- sample.int(n, size = round(0.06 * n))           # 6% pile up exactly at c
x_mass[idx] <- c0

# Combine
df <- bind_rows(
  tibble(scenario = "Continuous (valid)",               x = x_cont),
  tibble(scenario = "Bunching to the right",            x = x_bunching),
  tibble(scenario = "Donut hole at cutoff",             x = x_donut),
  tibble(scenario = "Heaping / rounding (discrete X)", x = x_heaped),
  tibble(scenario = "Mass point at cutoff",             x = x_mass)
)

# Common theme
vline <- geom_vline(xintercept = c0, linewidth = 0.6, linetype = "dashed", alpha = 0.8)

# ---- Plot A: Histograms (good for mass points and heaping) ----
p_hist <- ggplot(df, aes(x = x)) +
  geom_histogram(binwidth = 0.05, boundary = c0, closed = "left", color = "white") +
  vline +
  labs(
    title = "Running variable around the cutoff",
    subtitle = "Histograms by scenario (vertical dashed line = cutoff)",
    x = "Running variable (X)", y = "Count"
  ) +
  facet_wrap(~scenario, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 12)

# ---- Plot B: Zoomed-in window near cutoff (densities) ----
# (Density is great for smoothness/bunching; histogram is better for mass points)
p_dens <- ggplot(df %>% filter(between(x, -1, 1)),
                 aes(x = x)) +
  geom_density(adjust = 1.2) +
  vline +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(
    x = "Running variable (X)", y = "Density"
  ) +
  facet_wrap(~scenario, ncol = 2) +
  theme_minimal(base_size = 12)

# Draw
print(p_hist)
print(p_dens)

# ggsave("RD density.pdf", p_dens, width = 9, height = 5, dpi = 300)

library(ggplot2)
library(dplyr)
library(forcats)

# Focus on continuous vs. heaped scenarios
d2 <- df %>%
  filter(scenario %in% c("Continuous (valid)", "Heaping / rounding (discrete X)")) %>%
  mutate(scenario = fct_relevel(scenario, "Continuous (valid)", "Heaping / rounding (discrete X)"))

vline <- geom_vline(xintercept = c0, linetype = "dashed")

# 1) Side-by-side KDE (to show why they can look similar)
p_kde <- ggplot(d2 %>% filter(between(x, -1.5, 1.5)),
                aes(x = x, color = scenario)) +
  geom_density(adjust = 1.2, linewidth = 1) +
  vline +
  labs(title = "KDE makes heaping look smooth",
       subtitle = "Kernel densities within ±1.5 around the cutoff",
       x = "Running variable (X)", y = "Density") +
  theme_minimal(base_size = 12)

# 2) Histograms with binwidth aligned to the heaping grid (0.1 here)
#    This reveals the spikes in the discrete case.
p_hist <- ggplot(d2 %>% filter(between(x, -1.5, 1.5)),
                 aes(x = x)) +
  geom_histogram(binwidth = 0.1, boundary = c0, closed = "left",
                 color = "white") +
  vline +
  labs(title = "Histograms aligned to the rounding grid",
       subtitle = "Binwidth = 0.1 (matches the heaping grid)",
       x = "X", y = "Count") +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 12)

# 3) Exact-mass “spike plot” for the heaped case (lollipop at each unique X)
heaped_counts <- d2 %>%
  filter(scenario == "Heaping / rounding (discrete X)") %>%
  count(x)  # counts at exact rounded values (e.g., ..., -0.2, -0.1, 0.0, 0.1, ...)

p_spikes <- ggplot(heaped_counts %>% filter(between(x, -1.5, 1.5)),
                   aes(x = x, y = n)) +
  geom_segment(aes(xend = x, y = 0, yend = n)) +
  geom_point() +
  vline +
  labs(title = "Heaped X: exact-mass spikes",
       subtitle = "Counts at each rounded grid point (width ≈ 0.1)",
       x = "X", y = "Count") +
  theme_minimal(base_size = 12)

# 4) ECDFs: discrete shows steps, continuous is smooth
p_ecdf <- ggplot(d2 %>% filter(between(x, -1.5, 1.5)),
                 aes(x = x, color = scenario)) +
  stat_ecdf(geom = "step", linewidth = 1) +
  vline +
  labs(title = "ECDF comparison",
       subtitle = "Discrete/heaped X has visible steps",
       x = "X", y = "F(X)") +
  theme_minimal(base_size = 12)

# Show all
print(p_kde)
print(p_hist)
print(p_spikes)
print(p_ecdf)