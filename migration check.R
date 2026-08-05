# migration check.R
# Age profile of recent (within-12-month) migration among women.
# Share of women who migrated in the last 12 months, by 5-year age bin,
# pooled over 2011-2017 (migrant is NA in 2010 and for under-15s).
# Unweighted (matches the lfs_sum_dist pipeline); survey weight `wt` is in lfs_all if needed.

library(tidyverse)

load("Clean data/lfs_all.Rda")

mig_by_age <- lfs_all %>%
  filter(female == 1, year >= 2011, !is.na(migrant), age >= 15) %>%
  mutate(age_bin = cut(age,
                       breaks = c(seq(15, 65, by = 5), Inf),
                       right  = FALSE,
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65+"))) %>%
  group_by(age_bin) %>%
  summarise(n     = n(),
            share = mean(migrant),
            se    = sqrt(share * (1 - share) / n),
            lo    = pmax(0, share - 1.96 * se),
            hi    = share + 1.96 * se,
            .groups = "drop")

print(mig_by_age)

p <- ggplot(mig_by_age, aes(x = age_bin, y = share, group = 1)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, colour = "#1B9E77") +
  geom_line(colour = "#1B9E77", linewidth = 0.8) +
  geom_point(colour = "#1B9E77", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Age group",
       y = "Share migrated in last 12 months",
       title = "Women: share who migrated in the last 12 months, by age (2011-2017)") +
  theme_minimal(base_size = 12)

print(p)
ggsave("migration_check_age.png", p, width = 8, height = 5, dpi = 150)


# --- Time-series version: one line per 5-year age bin, year on the x-axis ---
mig_by_age_year <- lfs_all %>%
  filter(female == 1, year >= 2011, !is.na(migrant), age >= 15) %>%
  mutate(age_bin = cut(age,
                       breaks = c(seq(15, 65, by = 5), Inf),
                       right  = FALSE,
                       labels = c("15-19", "20-24", "25-29", "30-34", "35-39",
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65+"))) %>%
  group_by(year, age_bin) %>%
  summarise(n = n(), share = mean(migrant), .groups = "drop")

mig12_by_age_year <- ggplot(mig_by_age_year,
                            aes(x = year, y = share, colour = age_bin, group = age_bin)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = 2011:2017) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.08, 0.01), limits = c(0, 0.08)) +
  scale_colour_viridis_d(option = "D", end = 0.95) +
  labs(x = "Year", y = "Share migrated in last 12 months", colour = "Age group",
       title = "Women: share who migrated in the last 12 months, by year and age group") +
  theme_minimal(base_size = 12)

print(mig12_by_age_year)
ggsave("mig12_by_age_year.png", mig12_by_age_year, width = 9, height = 5.5, dpi = 150)
