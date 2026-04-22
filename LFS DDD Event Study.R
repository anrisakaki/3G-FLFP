library(triplediff)

load("Clean data/lfs_sum_dist_ddd.Rda")
load("Clean data/lfs_sum_dist_f_ddd.Rda")
load("Clean data/lfs_sum_dist_m_ddd.Rda")

fig_dir <- "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results"

# ── Triplediff event study ────────────────────────────────────────────────────

prep_triplediff <- function(df) {
  df %>%
    filter(is.na(year_mean_OCI) | year_mean_OCI != 2010) %>%
    mutate(gname_td = ifelse(is.na(year_mean_OCI), 0, year_mean_OCI)) %>%
    group_by(young, ID_2) %>%
    mutate(dist_young = cur_group_id()) %>%
    ungroup()
}

run_triplediff <- function(df, outcome, min_e = -6, max_e = 5) {
  out <- ddd(
    yname         = outcome,
    tname         = "year",
    idname        = "dist_young",
    gname         = "gname_td",
    pname         = "young",
    xformla       = ~1,
    data          = prep_triplediff(df),
    control_group = "nevertreated",
    base_period   = "universal",
    est_method    = "reg",
    boot          = TRUE,
    nboot         = 500,
    cluster       = "ID_2"
  )
  agg_ddd(out, type = "eventstudy", min_e = min_e, max_e = max_e)
}

td_work_all <- run_triplediff(lfs_sum_dist_ddd,   "work")
td_work_f   <- run_triplediff(lfs_sum_dist_f_ddd, "work")
td_work_m   <- run_triplediff(lfs_sum_dist_m_ddd, "work")

td_agri_all <- run_triplediff(lfs_sum_dist_ddd,   "agri")
td_agri_f   <- run_triplediff(lfs_sum_dist_f_ddd, "agri")
td_agri_m   <- run_triplediff(lfs_sum_dist_m_ddd, "agri")

td_manu_all <- run_triplediff(lfs_sum_dist_ddd,   "manu")
td_manu_f   <- run_triplediff(lfs_sum_dist_f_ddd, "manu")
td_manu_m   <- run_triplediff(lfs_sum_dist_m_ddd, "manu")

td_hhbus_all <- run_triplediff(lfs_sum_dist_ddd,   "hhbus")
td_hhbus_f   <- run_triplediff(lfs_sum_dist_f_ddd, "hhbus")
td_hhbus_m   <- run_triplediff(lfs_sum_dist_m_ddd, "hhbus")

# ── TWFE DDD event study ──────────────────────────────────────────────────────

colours <- c("#4D4D4D", "#1B9E77")

plot_ddd_event_study <- function(df, outcome, out_file) {
  fml <- as.formula(paste0(
    outcome,
    " ~ i(ytt_mean_OCI, mean_3G_OCI*young, ref = c(-1)) + ",
    " i(mean_3G_OCI*young) +",
    " i(ytt_mean_OCI, mean_3G_OCI,  ref = c(-1)) +",
    " i(ytt_mean_OCI, young,        ref = c(-1)) +",
    "i(year) + i(ID_2) + i(young) +",
    " lnexport_all + i(year, sh_manu_exposed, ref = 2010)"
  ))

  jpeg(out_file)
  iplot(
    feols(fml, df %>% filter(year_mean_OCI != 2010), vcov = ~ID_2),
    i.select = 1,
    xlab = "Years to treatment"
  )
  dev.off()
}

plot_ddd_event_study(lfs_sum_dist_ddd,   "work",  file.path(fig_dir, "work_ddd_all.jpeg"))
plot_ddd_event_study(lfs_sum_dist_f_ddd, "work",  file.path(fig_dir, "work_ddd_f.jpeg"))
plot_ddd_event_study(lfs_sum_dist_m_ddd, "work",  file.path(fig_dir, "work_ddd_m.jpeg"))

plot_ddd_event_study(lfs_sum_dist_ddd,   "agri",  file.path(fig_dir, "agri_ddd_all.jpeg"))
plot_ddd_event_study(lfs_sum_dist_f_ddd, "agri",  file.path(fig_dir, "agri_ddd_f.jpeg"))
plot_ddd_event_study(lfs_sum_dist_m_ddd, "agri",  file.path(fig_dir, "agri_ddd_m.jpeg"))

plot_ddd_event_study(lfs_sum_dist_ddd,   "manu",  file.path(fig_dir, "manu_ddd_all.jpeg"))
plot_ddd_event_study(lfs_sum_dist_f_ddd, "manu",  file.path(fig_dir, "manu_ddd_f.jpeg"))
plot_ddd_event_study(lfs_sum_dist_m_ddd, "manu",  file.path(fig_dir, "manu_ddd_m.jpeg"))

plot_ddd_event_study(lfs_sum_dist_ddd,   "hhbus", file.path(fig_dir, "hhbus_ddd_all.jpeg"))
plot_ddd_event_study(lfs_sum_dist_f_ddd, "hhbus", file.path(fig_dir, "hhbus_ddd_f.jpeg"))
plot_ddd_event_study(lfs_sum_dist_m_ddd, "hhbus", file.path(fig_dir, "hhbus_ddd_m.jpeg"))
