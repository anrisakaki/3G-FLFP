load("Clean data/lfs_sum_dist.Rda")
load("Clean data/lfs_sum_dist_m.Rda")
load("Clean data/lfs_sum_dist_f.Rda")

colours <- c("#4D4D4D", "#1B9E77")
setFixest_coefplot(
  grid = F,
  zero.par = list(type = "dotted", lty = 2),
  main = "",
  ref.line = -1,
  col = c("#4D4D4D", "#1B9E77"),
  pt.join.par = list(lwd = 2),
  lwd = 2
)
setFixest_ssc(ssc(adj = FALSE, cluster.adj = FALSE))
dict = c("share_3G_OCI" = "3G Coverage",
         "work" = "LFP",
         "hhbus" = "Household Business",
         "oaw" = "OAW",
         "wagework" = "Wage Work",
         "agri" = "Agriculture",
         "manu" = "Manufacturing",
         "service" = "Services",
         "taxid" = "Tax ID",
         "erc" = "ERC",
         "socinsur" = "Social Insurance",
         "ID_2" = "District",
         "coverage_OCI" = "3G Coverage")

run_twfe_table <- function(df, outcomes, out_file) {
  models <- lapply(outcomes, function(outcome) {
    fml <- as.formula(paste0(outcome, " ~ i(coverage_mean_OCI) | ID_2 + year"))
    feols(fml, df, vcov = ~ID_2)
  })

  etable(
    models,
    tex = TRUE,
    dict = dict,
    file = out_file
  )
}

plot_event_study <- function(df_twfe, df_sunab, outcome, out_file) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_exposed_09, ref = 2010) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_exposed_09, ref = 2010) | ID_2 + year"))

  jpeg(out_file)
  iplot(
    list(
      feols(fml_twfe, df_twfe, vcov = ~ID_2),
      feols(fml_sunab, df_sunab, vcov = ~ID_2)
    ),
    xlab = "Years to treatment"
  )
  legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
  dev.off()
}

run_twfe_table(
  lfs_sum_dist,
  c("agri", "manu", "service"),
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Results/LFS_TWFE_OCI.tex"
)

run_twfe_table(
  lfs_sum_dist_f,
  c("work", "agri", "manu", "service"),
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Results/LFS_TWFE_OCI_f.tex"
)

run_twfe_table(
  lfs_sum_dist_m,
  c("agri", "manu", "service"),
  "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Results/LFS_TWFE_OCI_m.tex"
)

fig_dir <- "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results"

default_all <- subset(lfs_sum_dist, year_mean_OCI != 2010)
default_f <- subset(lfs_sum_dist_f, year_mean_OCI != 2010)
default_m <- subset(lfs_sum_dist_m, year_mean_OCI != 2010)

plot_event_study(default_all, lfs_sum_dist, "work", file.path(fig_dir, "work_mean_OCI.jpeg"))
plot_event_study(default_all, lfs_sum_dist, "agri", file.path(fig_dir, "agri_mean_OCI.jpeg"))
plot_event_study(default_all, lfs_sum_dist, "manu", file.path(fig_dir, "manu_mean_OCI.jpeg"))
plot_event_study(default_all, lfs_sum_dist, "service", file.path(fig_dir, "service_mean_OCI.jpeg"))
plot_event_study(default_all, lfs_sum_dist, "hhbus", file.path(fig_dir, "hhbus_mean_OCI.jpeg"))
plot_event_study(subset(lfs_sum_dist, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 subset(lfs_sum_dist, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 "taxid", file.path(fig_dir, "taxid_mean_OCI.jpeg"))
plot_event_study(subset(lfs_sum_dist, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000), 
                 subset(lfs_sum_dist, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
                 "socinsur", file.path(fig_dir, "socinsur_mean_OCI.jpeg"))

plot_event_study(default_f, lfs_sum_dist_f, "work", file.path(fig_dir, "work_mean_OCI_f.jpeg"))
plot_event_study(default_f, lfs_sum_dist_f, "agri", file.path(fig_dir, "agri_mean_OCI_f.jpeg"))
plot_event_study(default_f, lfs_sum_dist_f, "manu", file.path(fig_dir, "manu_mean_OCI_f.jpeg"))
plot_event_study(default_f, lfs_sum_dist_f, "service", file.path(fig_dir, "service_mean_OCI_f.jpeg"))
plot_event_study(default_f, lfs_sum_dist_f, "hhbus", file.path(fig_dir, "hhbus_mean_OCI_f.jpeg"))
plot_event_study(subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 "taxid", file.path(fig_dir, "taxid_mean_OCI_f.jpeg"))
plot_event_study(subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000), 
                 subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
                 "socinsur", file.path(fig_dir, "socinsur_mean_OCI_f.jpeg"))

plot_event_study(default_m, lfs_sum_dist_m, "work", file.path(fig_dir, "work_mean_OCI_m.jpeg"))
plot_event_study(default_m, lfs_sum_dist_m, "agri", file.path(fig_dir, "agri_mean_OCI_m.jpeg"))
plot_event_study(default_m, lfs_sum_dist_m, "manu", file.path(fig_dir, "manu_mean_OCI_m.jpeg"))
plot_event_study(default_m, lfs_sum_dist_m, "service", file.path(fig_dir, "service_mean_OCI_m.jpeg"))
plot_event_study(default_m, lfs_sum_dist_m, "hhbus", file.path(fig_dir, "hhbus_mean_OCI_m.jpeg"))
plot_event_study(subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
                 "taxid", file.path(fig_dir, "taxid_mean_OCI_m.jpeg"))
plot_event_study(subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000), 
                 subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
                 "socinsur", file.path(fig_dir, "socinsur_mean_OCI_m.jpeg"))