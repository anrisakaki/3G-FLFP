library(triplediff)
library(tidyverse)
library(fixest)
library(broom)
library(ggplot2)

load("Clean data/lfs_sum_dist_ddd.Rda")
load("Clean data/lfs_sum_dist_f_ddd.Rda")
load("Clean data/lfs_sum_dist_m_ddd.Rda")

fig_dir <- "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results"

# ── TWFE DDD event study ──────────────────────────────────────────────────────

colours <- c("#4D4D4D", "#1B9E77", "#D95F02")

plot_combined_event_study <- function(df, outcome, out_file) {

  df$treated1 <- case_when(
    df$mean_3G_OCI == 1 & df$young == 1 ~ 1,
    df$young == 1 ~ 0,
    .default = NA_integer_
  )
  
  df$treated_placebo <- case_when(
    df$mean_3G_OCI == 1 & df$young == 0 ~ 1,
    df$young == 0 ~ 0,
    .default = NA_integer_
  )
  
  df_filtered <- df %>%
    filter(year_mean_OCI != 2010) %>%
    mutate(ytt_mean_OCI = if_else(ytt_mean_OCI == -1000, 0, ytt_mean_OCI),
           treated1_x_ytt = if_else(treated1 == 1, ytt_mean_OCI, -1L),
           placebo_x_ytt = if_else(treated_placebo == 1, ytt_mean_OCI, -1L)) 

  # 1. DiD for young: event study
  es_young <- feols(
    as.formula(paste0(outcome, " ~ i(treated1_x_ytt, ref = -1) +
                      lnexport_all + i(year, sh_manu_exposed, ref = 2010) | year + ID_2")),
    df_filtered,
    vcov = ~ID_2
  )

  # 2. DiD for old: event study
  es_old <- feols(
    as.formula(paste0(outcome, " ~ i(placebo_x_ytt, ref = -1) +
                      lnexport_all + i(year, sh_manu_exposed, ref = 2010) | year + ID_2")),
    df_filtered,
    vcov = ~ID_2
  )

  # Extract coefficients
  pts_young <- es_young %>%
    broom::tidy() %>%
    filter(str_detect(term, "treated1_x_ytt::")) %>%
    mutate(
      estimator = "DiD: Aged 20-49",
      ytt = as.numeric(str_extract(term, "treated1_x_ytt::(-?\\d+)", group = 1))
    )

  pts_old <- es_old %>%
    broom::tidy() %>%
    filter(str_detect(term, "placebo_x_ytt::")) %>%
    mutate(
      estimator = "DiD: Aged 50-64",
      ytt = as.numeric(str_extract(term, "placebo_x_ytt::(-?\\d+)", group = 1))
    )

  # Combine all estimates
  pts <- bind_rows(pts_young, pts_old)

  # Plot
  p <- ggplot(pts) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    geom_point(
      aes(x = ytt, y = estimate, color = estimator),
      size = 2,
      position = position_dodge(width = 0.3)
    ) +
    geom_errorbar(
      aes(
        x = ytt, color = estimator,
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error
      ),
      width = 0.2,
      position = position_dodge(width = 0.3)
    ) +
    scale_color_manual(values = colours) +
    labs(
      x = "Years to treatment",
      y = "Estimate and 95% CI",
      color = NULL,
      title = ""
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(16, "pt"),
      legend.box.background = element_rect()
    )

  ggsave(out_file, p, width = 10, height = 7, dpi = 300)

  return(list(plot = p, results_DDD = pts))
}

results_old_young <- bind_rows(
  plot_combined_event_study(lfs_sum_dist_ddd,   "work",  file.path(fig_dir, "work_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "work", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "work",  file.path(fig_dir, "work_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "work", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "work",  file.path(fig_dir, "work_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "work", sample = "male"),

  plot_combined_event_study(lfs_sum_dist_ddd,   "agri",  file.path(fig_dir, "agri_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "agri", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "agri",  file.path(fig_dir, "agri_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "agri", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "agri",  file.path(fig_dir, "agri_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "agri", sample = "male"),

  plot_combined_event_study(lfs_sum_dist_ddd,   "manu",  file.path(fig_dir, "manu_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "manu", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "manu",  file.path(fig_dir, "manu_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "manu", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "manu",  file.path(fig_dir, "manu_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "manu", sample = "male"),
  
  plot_combined_event_study(lfs_sum_dist_ddd,   "service",  file.path(fig_dir, "service_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "service", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "service",  file.path(fig_dir, "service_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "service", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "service",  file.path(fig_dir, "service_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "service", sample = "male"),
  
  plot_combined_event_study(lfs_sum_dist_ddd,   "construction",  file.path(fig_dir, "construction_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "construction", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "construction",  file.path(fig_dir, "construction_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "construction", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "construction",  file.path(fig_dir, "construction_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "construction", sample = "male"),

  plot_combined_event_study(lfs_sum_dist_ddd,   "hhbus", file.path(fig_dir, "hhbus_combined_all.jpeg"))$results_DDD %>% mutate(outcome = "hhbus", sample = "all"),
  plot_combined_event_study(lfs_sum_dist_f_ddd, "hhbus", file.path(fig_dir, "hhbus_combined_f.jpeg"))$results_DDD %>% mutate(outcome = "hhbus", sample = "female"),
  plot_combined_event_study(lfs_sum_dist_m_ddd, "hhbus", file.path(fig_dir, "hhbus_combined_m.jpeg"))$results_DDD %>% mutate(outcome = "hhbus", sample = "male")
)

# Export results to CSV
write_csv(results_DDD, "Clean data/results_DDD_event_study.csv")
