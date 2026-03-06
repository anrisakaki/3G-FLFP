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
  "age50plus" = "Age 50+"
)

to_ddd_sample <- function(df) {
  df %>%
    mutate(age50plus = ifelse(agegr == 0, 1, 0)) %>% # In Cleaning LFS data.R: agegr==0 is ages 50-64.
    filter(year_mean_OCI != 2010)
}

run_ddd_event_model <- function(df, outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = -1)",
      " + age50plus:i(ytt_mean_OCI, mean_3G_OCI, ref = -1)",
      " + age50plus + lnexport_all | ID_2 + year"
    )
  )
  feols(fml, to_ddd_sample(df), vcov = ~ID_2)
}

plot_ddd_interaction <- function(model, out_file) {
  png(out_file)
  iplot(
    model,
    keep = "age50plus",
    xlab = "Years to treatment",
    main = "DDD event-study: 50+ relative to 20-49"
  )
  legend(
    "topleft",
    col = colours[2],
    pch = 1,
    lwd = 2,
    cex = 1,
    bty = "n",
    legend = "Age 50+ interaction"
  )
  dev.off()
}

model_work_all_ddd <- run_ddd_event_model(lfs_sum_dist_ddd, "work")
model_work_f_ddd <- run_ddd_event_model(lfs_sum_dist_f_ddd, "work")
model_work_m_ddd <- run_ddd_event_model(lfs_sum_dist_m_ddd, "work")

etable(
  list(model_work_all_ddd, model_work_f_ddd, model_work_m_ddd),
  tex = TRUE,
  dict = dict,
  file = "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Results/LFS_DDD_ES_work_50plus.tex"
)

plot_ddd_interaction(
  model_work_all_ddd,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_es_50plus_all.jpeg"
)
plot_ddd_interaction(
  model_work_f_ddd,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_es_50plus_f.jpeg"
)
plot_ddd_interaction(
  model_work_m_ddd,
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_ddd_es_50plus_m.jpeg"
)
