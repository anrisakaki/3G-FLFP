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
cex_main <- 1.8
cex_lab <- 1.25
cex_axis <- 1.25
setFixest_ssc(ssc(adj = F, cluster.adj = F))
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


fig_dir <- "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results"

###############
# EVENT STUDY #
###############

plot_event_study <- function(df_twfe, df_sunab, outcome, out_file) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) +
                                lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) +
                                 lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
  
  pdf(out_file)
  par(mar = c(7, 4, 4, 2), mgp = c(2, 1, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  iplot(
    list(
      feols(fml_twfe, df_twfe, vcov = ~ID_2),
      feols(fml_sunab, df_sunab, vcov = ~ID_2)
    ),
    xlab = "Years to treatment"
  )
  legend("bottom", col = colours, pch = 1, lwd = 2, cex = 1.3, bty = "n", legend = c("TWFE", "Sun & Abraham"), horiz = T, inset = c(0, -0.25), xpd = T, x.intersp = 0.3)
  dev.off()
}

default_all <- subset(lfs_sum_dist, year_mean_OCI != 2010)
default_f <- subset(lfs_sum_dist_f, year_mean_OCI != 2010)
default_m <- subset(lfs_sum_dist_m, year_mean_OCI != 2010)

plot_event_study_combined <- function(df_twfe_f, df_sunab_f, df_twfe_m, df_sunab_m, outcome, out_file) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))

  pdf(out_file, width = 12, height = 6)
  par(mfrow = c(1, 2), mar = c(7, 4, 4, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  iplot(
    list(
      feols(fml_twfe, df_twfe_m, vcov = ~ID_2),
      feols(fml_sunab, df_sunab_m, vcov = ~ID_2)
    ),
    xlab = "Years to treatment",
    main = "Male"
  )

  iplot(
    list(
      feols(fml_twfe, df_twfe_f, vcov = ~ID_2),
      feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    ),
    xlab = "Years to treatment",
    main = "Female"
  )

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours, pch = 1, lwd = 2, cex = 1.3, bty = "n",
         legend = c("TWFE", "Sun & Abraham"), horiz = TRUE, inset = c(0, 0.05), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

# Combined male-female plots for sectoral reallocation
plot_event_study_combined(default_f, lfs_sum_dist_f, default_m, lfs_sum_dist_m, "agri", file.path(fig_dir, "agri_mean_OCI_combined.pdf"))
plot_event_study_combined(default_f, lfs_sum_dist_f, default_m, lfs_sum_dist_m, "manu", file.path(fig_dir, "manu_mean_OCI_combined.pdf"))
plot_event_study_combined(default_f, lfs_sum_dist_f, default_m, lfs_sum_dist_m, "service", file.path(fig_dir, "service_mean_OCI_combined.pdf"))

# Sectoral for female only
plot_sectoral_wide_f <- function(df_twfe_f, df_sunab_f, out_file) {
  outcomes <- c("agri", "manu", "service")
  titles <- c("Agriculture", "Manufacturing", "Services")
  
  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))

    iplot(
      list(feols(fml_twfe, df_twfe_f, vcov = ~ID_2), feols(fml_sunab, df_sunab_f, vcov = ~ID_2)),
      xlab = "Years to treatment", main = titles[i]
    )
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours, pch = 1, lwd = 2, cex = 1.3, bty = "n",
         legend = c("TWFE", "Sun & Abraham"), horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_sectoral_wide_f(default_f, lfs_sum_dist_f, file.path(fig_dir, "sectoral_mean_OCI_f.pdf"))

# Combined male-female plots for informality measures
plot_event_study_combined(default_f, lfs_sum_dist_f, default_m, lfs_sum_dist_m, "hhbus", file.path(fig_dir, "hhbus_mean_OCI_combined.pdf"))

plot_event_study_combined(
  subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000),
  "taxid", file.path(fig_dir, "taxid_mean_OCI_combined.pdf"))

plot_event_study_combined(
  subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
  subset(lfs_sum_dist_m, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000),
  "socinsur", file.path(fig_dir, "socinsur_mean_OCI_combined.pdf"))

# Informality for female only
plot_informality_wide_f <- function(out_file) {
  titles <- c("HH Business", "Tax ID", "Social Insurance")

  # Different subsets for each outcome
  df_hhbus <- default_f
  df_taxid <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000)
  df_socinsur <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000)

  fml_twfe_hhbus <- as.formula("hhbus ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_hhbus <- as.formula("hhbus ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_twfe_taxid <- as.formula("taxid ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_taxid <- as.formula("taxid ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_twfe_socinsur <- as.formula("socinsur ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_socinsur <- as.formula("socinsur ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")

  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  # HH Business
  iplot(list(feols(fml_twfe_hhbus, df_hhbus, vcov = ~ID_2), feols(fml_sunab_hhbus, lfs_sum_dist_f, vcov = ~ID_2)),
        xlab = "Years to treatment", main = titles[1])

  # Tax ID
  iplot(list(feols(fml_twfe_taxid, df_taxid, vcov = ~ID_2), feols(fml_sunab_taxid, df_taxid, vcov = ~ID_2)),
        xlab = "Years to treatment", main = titles[2])

  # Social Insurance
  iplot(list(feols(fml_twfe_socinsur, df_socinsur, vcov = ~ID_2), feols(fml_sunab_socinsur, df_socinsur, vcov = ~ID_2)),
        xlab = "Years to treatment", main = titles[3])

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours, pch = 1, lwd = 2, cex = 1.3, bty = "n",
         legend = c("TWFE", "Sun & Abraham"), horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_informality_wide_f(file.path(fig_dir, "informality_mean_OCI_f.pdf"))

# Placebo

colours_old <- c("#FDBF6F", "#E6621E")
colours_age <- c("#A6CEE3", "#1F78B4", "#FDBF6F", "#E6621E")

# Sectoral for female - old only (45-64)
plot_sectoral_wide_f_old <- function(df_f, out_file) {
  outcomes <- c("agri_45_64", "manu_45_64", "service_45_64")
  titles <- c("Agriculture", "Manufacturing", "Services")

  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))

    iplot(list(feols(fml_twfe, df_f, vcov = ~ID_2), feols(fml_sunab, df_f, vcov = ~ID_2)),
          col = colours_old,
          xlab = "Years to treatment", main = titles[i])
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours_old, pch = 1, lwd = 2, cex = 1.3, bty = "n",
         legend = c("TWFE (45-64)", "Sun and Abraham (2020) (45-64)"), horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_sectoral_wide_f_old(default_f, file.path(fig_dir, "sectoral_mean_OCI_f_old.pdf"))

# Sectoral for female - young (20-44) and old (45-64)
plot_sectoral_wide_f_age <- function(df_f, out_file) {
  outcomes_base <- c("agri", "manu", "service")
  titles <- c("Agriculture", "Manufacturing", "Services")

  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes_base)) {
    out_young <- paste0(outcomes_base[i], "_20_44")
    out_old <- paste0(outcomes_base[i], "_45_64")
    fml_twfe_young <- as.formula(paste0(out_young, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab_young <- as.formula(paste0(out_young, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_twfe_old <- as.formula(paste0(out_old, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab_old <- as.formula(paste0(out_old, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))

    iplot(list(feols(fml_twfe_young, df_f, vcov = ~ID_2), feols(fml_sunab_young, df_f, vcov = ~ID_2),
               feols(fml_twfe_old, df_f, vcov = ~ID_2), feols(fml_sunab_old, df_f, vcov = ~ID_2)),
          col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
          xlab = "Years to treatment", main = titles[i])
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours_age, pch = 1, lwd = 2, lty = c(1, 1, 2, 2), cex = 1.1, bty = "n",
         legend = c("TWFE (20-44)", "Sun and Abraham (2020) (20-44)", "TWFE (45-64)", "Sun and Abraham (2020) (45-64)"),
         horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_sectoral_wide_f_age(default_f, file.path(fig_dir, "sectoral_mean_OCI_f_age.pdf"))

# Informality for female - old only (45-64)

plot_informality_wide_f_old <- function(out_file) {
  titles <- c("HH Business", "Tax ID", "Social Insurance")

  df_hhbus <- default_f
  df_taxid <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000)
  df_socinsur <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000)

  fml_twfe_hhbus <- as.formula("hhbus_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_hhbus <- as.formula("hhbus_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_twfe_taxid <- as.formula("taxid_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_taxid <- as.formula("taxid_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_twfe_socinsur <- as.formula("socinsur_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")
  fml_sunab_socinsur <- as.formula("socinsur_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year")

  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  iplot(list(feols(fml_twfe_hhbus, df_hhbus, vcov = ~ID_2), feols(fml_sunab_hhbus, lfs_sum_dist_f, vcov = ~ID_2)),
        col = colours_old,
        xlab = "Years to treatment", main = titles[1])
  iplot(list(feols(fml_twfe_taxid, df_taxid, vcov = ~ID_2), feols(fml_sunab_taxid, df_taxid, vcov = ~ID_2)),
        col = colours_old,
        xlab = "Years to treatment", main = titles[2])
  iplot(list(feols(fml_twfe_socinsur, df_socinsur, vcov = ~ID_2), feols(fml_sunab_socinsur, df_socinsur, vcov = ~ID_2)),
        col = colours_old,
        xlab = "Years to treatment", main = titles[3])

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours_old, pch = 1, lwd = 2, cex = 1.3, bty = "n",
         legend = c("TWFE (45-64)", "Sun and Abraham (2020) (45-64)"), horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_informality_wide_f_old(file.path(fig_dir, "informality_mean_OCI_f_old.pdf"))

# Informality for female - young (20-44) and old (45-64)
plot_informality_wide_f_age <- function(out_file) {
  titles <- c("HH Business", "Tax ID", "Social Insurance")
  outcomes_base <- c("hhbus", "taxid", "socinsur")

  df_hhbus <- default_f
  df_taxid <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000)
  df_socinsur <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000)
  dfs <- list(df_hhbus, df_taxid, df_socinsur)
  dfs_sunab <- list(lfs_sum_dist_f, df_taxid, df_socinsur)

  pdf(out_file, width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(5, 4, 3, 2), mgp = c(2, 1, 0), oma = c(2, 0, 0, 0), cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes_base)) {
    out_young <- paste0(outcomes_base[i], "_20_44")
    out_old <- paste0(outcomes_base[i], "_45_64")
    fml_twfe_young <- as.formula(paste0(out_young, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab_young <- as.formula(paste0(out_young, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_twfe_old <- as.formula(paste0(out_old, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))
    fml_sunab_old <- as.formula(paste0(out_old, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2010) + i(year, sh_hs_09, ref = 2010) + i(year, sh_fdi_09, ref = 2010) + i(year, sh_it_09, ref = 2010) + i(year, sh_migrant_09, ref = 2010) | ID_2 + year"))

    iplot(list(feols(fml_twfe_young, dfs[[i]], vcov = ~ID_2), feols(fml_sunab_young, dfs_sunab[[i]], vcov = ~ID_2),
               feols(fml_twfe_old, dfs[[i]], vcov = ~ID_2), feols(fml_sunab_old, dfs_sunab[[i]], vcov = ~ID_2)),
          col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
          xlab = "Years to treatment", main = titles[i])
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", col = colours_age, pch = 1, lwd = 2, lty = c(1, 1, 2, 2), cex = 1.1, bty = "n",
         legend = c("TWFE (20-44)", "Sun and Abraham (2020) (20-44)", "TWFE (45-64)", "Sun and Abraham (2020) (45-64)"),
         horiz = TRUE, inset = c(0, 0.01), xpd = TRUE, x.intersp = 0.3)
  dev.off()
}

plot_informality_wide_f_age(file.path(fig_dir, "informality_mean_OCI_f_age.pdf"))