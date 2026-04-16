load("Clean data/lfs_sum_dist_ddd.Rda")
load("Clean data/lfs_sum_dist_f_ddd.Rda")
load("Clean data/lfs_sum_dist_m_ddd.Rda")

colours <- c("#4D4D4D", "#1B9E77")
setFixest_coefplot(
  grid = FALSE,
  zero.par = list(type = "dotted", lty = 2),
  main = "",
  ref.line = -1,
  col = colours
)

dict <- c(
  "work" = "LFP",
  "ID_2" = "District",
  "age20_49" = "Age 20-49"
)

to_ddd_sample <- function(df) {
  df %>%
    mutate(age20_49 = ifelse(agegr == 0, 0, 1)) %>%
    filter(year_mean_OCI != 2010)
}

run_ddd_event_model <- function(df, outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = -1)",
      " + i(ytt_mean_OCI, mean_3G_OCI * age20_49, ref = -1)",
      " + age20_49 + lnexport_all | ID_2 + year"
    )
  )
  feols(fml, to_ddd_sample(df), vcov = ~ID_2)
}

plot_ddd_interaction <- function(model, out_file) {
  png(out_file)
  iplot(
    model,
    i.select = 2,
    xlab = "Years to treatment",
    main = "DDD event-study: 20-49 relative to 50+"
  )
  legend(
    "topleft",
    col = colours[2],
    pch = 1,
    lwd = 2,
    cex = 1,
    bty = "n",
    legend = "Age 20-49 interaction"
  )
  dev.off()
}

ddd_work_all <- run_ddd_event_model(lfs_sum_dist_ddd, "work")
ddd_work_f <- run_ddd_event_model(lfs_sum_dist_f_ddd, "work")
ddd_work_m <- run_ddd_event_model(lfs_sum_dist_m_ddd, "work")

ddd_agri_all <- run_ddd_event_model(lfs_sum_dist_ddd, "agri")
ddd_agri_f <- run_ddd_event_model(lfs_sum_dist_f_ddd, "agri")
ddd_agri_m <- run_ddd_event_model(lfs_sum_dist_m_ddd, "agri")

plot_ddd_interaction(
  ddd_work_all,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_all.jpeg"
)
plot_ddd_interaction(
  ddd_work_f,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_f.jpeg"
)
plot_ddd_interaction(
  ddd_work_m,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_m.jpeg"
)

plot_ddd_interaction(
  ddd_agri_m,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/agri_ddd_m.jpeg"
)

plot_ddd_interaction(
  ddd_agri_f,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/agri_ddd_f.jpeg"
)
