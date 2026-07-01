load("Clean data/lfs_sum_dist.Rda")
load("Clean data/lfs_sum_dist_m.Rda")
load("Clean data/lfs_sum_dist_f.Rda")

library(fixest)
library(tidyverse)
library(did)

colours <- c("#BDBDBD", "#1B9E77")
setFixest_coefplot(
  grid = F,
  zero.par = list(type = "dotted", lty = 2),
  main = "",
  ref.line = -1,
  col = c("#BDBDBD", "#1B9E77"),
  pt.join.par = list(lwd = 2),
  lwd = 2
)
cex_main <- 2.4
cex_lab <- 2.0
cex_axis <- 1.7
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

default_all <- subset(lfs_sum_dist, year_mean_OCI != 2010)
default_f <- subset(lfs_sum_dist_f, year_mean_OCI != 2010)
default_m <- subset(lfs_sum_dist_m, year_mean_OCI != 2010)

lfs_sum_dist_f <- lfs_sum_dist_f %>%
  mutate(log_inc = log(inc),
         log_hrinc = log(hrinc),
         log_inc_20_44 = log(inc_20_44),
         log_hrinc_20_44 = log(hrinc_20_44),
         log_inc_45_64 = log(inc_45_64),
         log_hrinc_45_64 = log(hrinc_45_64))

lfs_sum_dist_m <- lfs_sum_dist_m %>%
  mutate(log_inc = log(inc),
         log_hrinc = log(hrinc),
         log_inc_20_44 = log(inc_20_44),
         log_hrinc_20_44 = log(hrinc_20_44),
         log_inc_45_64 = log(inc_45_64),
         log_hrinc_45_64 = log(hrinc_45_64))

# Callaway & Sant'Anna overlay (extra estimator; control = lnexport_all only, estimated on the full panel)
cs_green  <- "#0F6E56"   # darker than S&A green  #1B9E77  (main / "all" plots)
cs_orange <- "#B34A12"   # darker than old orange #E6621E  (45-64 and age-old)
cs_blue   <- "#145A8A"   # darker than young blue #1F78B4  (age-young)
add_cs <- function(outcome, data, col = cs_green, offset = 0.25, lty = 1) {
  yn <- sub("^log\\((.*)\\)$", "log_\\1", outcome)
  a <- tryCatch(suppressMessages(suppressWarnings({
    m <- att_gt(yname = yn, tname = "year", idname = "ID_2", gname = "year_mean_OCI",
                xformla = ~lnexport_all, data = data, control_group = "notyettreated",
                base_period = "universal")
    aggte(m, type = "dynamic", na.rm = TRUE)
  })), error = function(e) NULL)
  if (is.null(a)) return(invisible(NULL))
  keep <- a$egt != -1
  x   <- a$egt[keep] + offset
  est <- a$att.egt[keep]
  segments(x, est - 1.96 * a$se.egt[keep], x, est + 1.96 * a$se.egt[keep], col = col, lwd = 2, lty = lty)
  points(x, est, col = col, pch = 1, lwd = 2)
}

###############
# EVENT STUDY #
###############

ggdid(aggte(att_gt(
  yname = "manu",
  tname = "year",
  idname = "ID_2",
  gname = "year_mean_OCI",
  xformla = ~lnexport_all,
  data = lfs_sum_dist_f,
  control_group = "notyettreated"
  # est_method = "reg"
  ), type = "dynamic", na.rm = T))

plot_event_study <- function(df_twfe, df_sunab, outcome, out_file,
                             main = "", ylab = "Estimate and 95% Conf. Int.",
                             width = 18, height = 8.4) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

  fit_twfe <- feols(fml_twfe, df_twfe, vcov = ~ID_2)
  fit_sunab <- feols(fml_sunab, df_sunab, vcov = ~ID_2)

  pdf(out_file, width = width, height = height)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s",
      cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  plot.new()
  iplot(
    list(fit_twfe, fit_sunab),
    xlab = "Years to treatment",
    ylab = ylab,
    main = main
  )
  add_cs(outcome, df_sunab)
  plot.new()
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_event_study_combined <- function(df_twfe_f, df_sunab_f, df_twfe_m, df_sunab_m, outcome, out_file) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

  fit_twfe_m <- feols(fml_twfe, df_twfe_m, vcov = ~ID_2)
  fit_sunab_m <- feols(fml_sunab, df_sunab_m, vcov = ~ID_2)
  fit_twfe_f <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
  fit_sunab_f <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)

  pdf(out_file, width = 12, height = 8.4)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  iplot(
    list(fit_twfe_m, fit_sunab_m),
    xlab = "Years to treatment",
    main = "Male"
  )
  add_cs(outcome, lfs_sum_dist_m)

  iplot(
    list(fit_twfe_f, fit_sunab_f),
    xlab = "Years to treatment",
    main = "Female"
  )
  add_cs(outcome, lfs_sum_dist_f)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
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

  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    iplot(
      list(fit_twfe, fit_sunab),
      xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i]
    )
    add_cs(outcome, lfs_sum_dist_f)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
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

  fml_twfe_hhbus <- as.formula("hhbus ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_hhbus <- as.formula("hhbus ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_twfe_taxid <- as.formula("taxid ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_taxid <- as.formula("taxid ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_twfe_socinsur <- as.formula("socinsur ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_socinsur <- as.formula("socinsur ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")

  fit_twfe_hhbus <- feols(fml_twfe_hhbus, df_hhbus, vcov = ~ID_2)
  fit_sunab_hhbus <- feols(fml_sunab_hhbus, lfs_sum_dist_f, vcov = ~ID_2)
  fit_twfe_taxid <- feols(fml_twfe_taxid, df_taxid, vcov = ~ID_2)
  fit_sunab_taxid <- feols(fml_sunab_taxid, df_taxid, vcov = ~ID_2)
  fit_twfe_socinsur <- feols(fml_twfe_socinsur, df_socinsur, vcov = ~ID_2)
  fit_sunab_socinsur <- feols(fml_sunab_socinsur, df_socinsur, vcov = ~ID_2)
  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  # HH Business
  iplot(list(fit_twfe_hhbus, fit_sunab_hhbus),
        xlab = "Years to treatment", ylab = "Estimate and 95% Conf. Int.", main = titles[1])
  add_cs("hhbus", lfs_sum_dist_f)

  # Tax ID
  iplot(list(fit_twfe_taxid, fit_sunab_taxid),
        xlab = "Years to treatment", ylab = "", main = titles[2])
  add_cs("taxid", lfs_sum_dist_f)

  # Social Insurance
  iplot(list(fit_twfe_socinsur, fit_sunab_socinsur),
        xlab = "Years to treatment", ylab = "", main = titles[3])
  add_cs("socinsur", lfs_sum_dist_f)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_informality_wide_f(file.path(fig_dir, "informality_mean_OCI_f.pdf"))

plot_event_study(default_f, lfs_sum_dist_f, "work", file.path(fig_dir, "work_mean_OCI_f.pdf"))

# Income event study (log Income, Hours, log Hourly income), female main sample
plot_income_wide_f <- function(df_twfe_f, df_sunab_f, out_file) {
  outcomes <- c("log(inc)", "hours", "log(hrinc)")
  titles   <- c("log(Income)", "Hours", "log(Hourly income)")

  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    iplot(
      list(fit_twfe, fit_sunab),
      xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i]
    )
    add_cs(outcome, lfs_sum_dist_f)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_income_wide_f(default_f, lfs_sum_dist_f, file.path(fig_dir, "inc_mean_OCI_f.pdf"))

# Migration 
plot_mig_20_24_wide_f <- function(df_twfe_f, df_sunab_f, out_file) {
  outcomes <- c("mig12_20_24", "mig_jobsearch_20_24", "mig_newjob_20_24")
  titles   <- c("Migrated < 12 months ago", "Look for job", "Start New Job")
  
  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    
    fit_twfe <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    iplot(
      list(fit_twfe, fit_sunab),
      xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i]
    )
    add_cs(outcome, lfs_sum_dist_f)
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_mig_wide_f <- function(df_twfe_f, df_sunab_f, out_file) {
  outcomes <- c("migrant", "mig_jobsearch", "mig_newjob")
  titles   <- c("Migrated < 12 months ago", "Look for job", "Start New Job")
  
  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    
    fit_twfe <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    iplot(
      list(fit_twfe, fit_sunab),
      xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i]
    )
    add_cs(outcome, lfs_sum_dist_f)
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_mig_ru_wide_f <- function(df_twfe_f, df_sunab_f, out_file) {
  outcomes <- c("mig_ru_20_24", "mig_ru_jobsearch_20_24", "mig_ru_newjob_20_24")
  titles   <- c("Migrated < 12 months ago", "Look for job", "Start New Job")
  
  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09, ref = 2011) + i(year, sh_hs_09, ref = 2011) + i(year, sh_fdi_09, ref = 2011) + i(year, sh_it_09, ref = 2011) + i(year, sh_migrant_09, ref = 2011) | ID_2 + year"))
    
    fit_twfe <- feols(fml_twfe, df_twfe_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab_f, vcov = ~ID_2)
    iplot(
      list(fit_twfe, fit_sunab),
      xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i]
    )
    add_cs(outcome, lfs_sum_dist_f)
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours, cs_green), pch = 1, lwd = 2, cex = cex_lab, bty = "n",
         legend = c("TWFE", "Sun & Abraham", "Callaway & Sant'Anna"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_mig_20_24_wide_f(subset(default_f, year > 2010 & ytt_mean_OCI < 6),
                subset(default_f, year > 2010 & ytt_mean_OCI < 6), file.path(fig_dir, "mig_20_24_mean_OCI_f.pdf"))

plot_mig_wide_f(subset(default_f, year > 2010 & ytt_mean_OCI < 6),
                subset(default_f, year > 2010 & ytt_mean_OCI < 6), file.path(fig_dir, "mig_mean_OCI_f.pdf"))

plot_mig_ru_wide_f(subset(default_f, year > 2010 & ytt_mean_OCI < 6),
                subset(default_f, year > 2010 & ytt_mean_OCI < 6), file.path(fig_dir, "mig_ru_mean_OCI_f.pdf"))

###########
# Placebo #
###########

colours_old <- c("#FDBF6F", "#E6621E")
colours_age <- c("#A6CEE3", "#1F78B4", "#FDBF6F", "#E6621E")

# Sectoral for female - old only (45-64)
plot_sectoral_wide_f_old <- function(df_f, out_file) {
  outcomes <- c("agri_45_64", "manu_45_64", "service_45_64")
  titles <- c("Agriculture", "Manufacturing", "Services")

  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe <- feols(fml_twfe, df_f, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_f, vcov = ~ID_2)
    iplot(list(fit_twfe, fit_sunab),
          col = colours_old,
          lty = c(2, 2),
          xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i])
    add_cs(outcome, lfs_sum_dist_f, col = cs_orange, lty = 2)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_old, cs_orange), pch = 1, lwd = 2, lty = c(2, 2, 2), cex = cex_lab, bty = "n",
         legend = c("TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (45-64)"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_sectoral_wide_f_old(default_f, file.path(fig_dir, "sectoral_mean_OCI_f_old.pdf"))

# Sectoral for female - young (20-44) and old (45-64)
plot_sectoral_wide_f_age <- function(df_f, out_file) {
  outcomes_base <- c("agri", "manu", "service")
  titles <- c("Agriculture", "Manufacturing", "Services")

  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes_base)) {
    out_young <- paste0(outcomes_base[i], "_20_44")
    out_old <- paste0(outcomes_base[i], "_45_64")
    fml_twfe_young <- as.formula(paste0(out_young, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_young <- as.formula(paste0(out_young, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_twfe_old <- as.formula(paste0(out_old, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_old <- as.formula(paste0(out_old, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe_young <- feols(fml_twfe_young, df_f, vcov = ~ID_2)
    fit_sunab_young <- feols(fml_sunab_young, df_f, vcov = ~ID_2)
    fit_twfe_old <- feols(fml_twfe_old, df_f, vcov = ~ID_2)
    fit_sunab_old <- feols(fml_sunab_old, df_f, vcov = ~ID_2)

    iplot(list(fit_twfe_young, fit_sunab_young,
               fit_twfe_old, fit_sunab_old),
          col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
          xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i])
    add_cs(out_young, lfs_sum_dist_f, col = cs_blue, offset = -0.28)
    add_cs(out_old, lfs_sum_dist_f, col = cs_orange, offset = 0.28, lty = 2)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_age, cs_blue, cs_orange), pch = 1, lwd = 2, lty = c(1, 1, 2, 2, 1, 2), cex = cex_lab, bty = "n", ncol = 3,
         legend = c("TWFE (20-44)", "Sun & Abraham (20-44)", "TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (20-44)", "Callaway & Sant'Anna (45-64)"),
         inset = c(0, 0.04), xpd = T, x.intersp = 0.4)
  dev.off()
}

plot_sectoral_wide_f_age(default_f, file.path(fig_dir, "sectoral_mean_OCI_f_age.pdf"))

# Informality for female - old only (45-64)

plot_informality_wide_f_old <- function(out_file) {
  titles <- c("HH Business", "Tax ID", "Social Insurance")

  df_hhbus <- default_f
  df_taxid <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 3 | ytt_mean_OCI == -1000)
  df_socinsur <- subset(lfs_sum_dist_f, ytt_mean_OCI > -7 & ytt_mean_OCI < 6 | ytt_mean_OCI == -1000)

  fml_twfe_hhbus <- as.formula("hhbus_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_hhbus <- as.formula("hhbus_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_twfe_taxid <- as.formula("taxid_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_taxid <- as.formula("taxid_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_twfe_socinsur <- as.formula("socinsur_45_64 ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")
  fml_sunab_socinsur <- as.formula("socinsur_45_64 ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year")

  fit_twfe_hhbus <- feols(fml_twfe_hhbus, df_hhbus, vcov = ~ID_2)
  fit_sunab_hhbus <- feols(fml_sunab_hhbus, lfs_sum_dist_f, vcov = ~ID_2)
  fit_twfe_taxid <- feols(fml_twfe_taxid, df_taxid, vcov = ~ID_2)
  fit_sunab_taxid <- feols(fml_sunab_taxid, df_taxid, vcov = ~ID_2)
  fit_twfe_socinsur <- feols(fml_twfe_socinsur, df_socinsur, vcov = ~ID_2)
  fit_sunab_socinsur <- feols(fml_sunab_socinsur, df_socinsur, vcov = ~ID_2)
  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  iplot(list(fit_twfe_hhbus, fit_sunab_hhbus),
        col = colours_old, lty = c(2, 2),
        xlab = "Years to treatment", ylab = "Estimate and 95% Conf. Int.", main = titles[1])
  add_cs("hhbus_45_64", lfs_sum_dist_f, col = cs_orange, lty = 2)

  iplot(list(fit_twfe_taxid, fit_sunab_taxid),
        col = colours_old, lty = c(2, 2),
        xlab = "Years to treatment", ylab = "", main = titles[2])
  add_cs("taxid_45_64", lfs_sum_dist_f, col = cs_orange, lty = 2)

  iplot(list(fit_twfe_socinsur, fit_sunab_socinsur),
        col = colours_old, lty = c(2, 2),
        xlab = "Years to treatment", ylab = "", main = titles[3])
  add_cs("socinsur_45_64", lfs_sum_dist_f, col = cs_orange, lty = 2)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_old, cs_orange), pch = 1, lwd = 2, lty = c(2, 2, 2), cex = cex_lab, bty = "n",
         legend = c("TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (45-64)"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
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

  pdf(out_file, width = 18, height = 8.4)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes_base)) {
    out_young <- paste0(outcomes_base[i], "_20_44")
    out_old <- paste0(outcomes_base[i], "_45_64")
    fml_twfe_young <- as.formula(paste0(out_young, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_young <- as.formula(paste0(out_young, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_twfe_old <- as.formula(paste0(out_old, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_old <- as.formula(paste0(out_old, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe_young <- feols(fml_twfe_young, dfs[[i]], vcov = ~ID_2)
    fit_sunab_young <- feols(fml_sunab_young, dfs_sunab[[i]], vcov = ~ID_2)
    fit_twfe_old <- feols(fml_twfe_old, dfs[[i]], vcov = ~ID_2)
    fit_sunab_old <- feols(fml_sunab_old, dfs_sunab[[i]], vcov = ~ID_2)

    iplot(list(fit_twfe_young, fit_sunab_young,
               fit_twfe_old, fit_sunab_old),
          col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
          xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i])
    add_cs(out_young, lfs_sum_dist_f, col = cs_blue, offset = -0.28)
    add_cs(out_old, lfs_sum_dist_f, col = cs_orange, offset = 0.28, lty = 2)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_age, cs_blue, cs_orange), pch = 1, lwd = 2, lty = c(1, 1, 2, 2, 1, 2), cex = cex_lab, bty = "n", ncol = 3,
         legend = c("TWFE (20-44)", "Sun & Abraham (20-44)", "TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (20-44)", "Callaway & Sant'Anna (45-64)"),
         inset = c(0, 0.04), xpd = T, x.intersp = 0.4)
  dev.off()
}

plot_informality_wide_f_age(file.path(fig_dir, "informality_mean_OCI_f_age.pdf"))

# Single-panel event study, old workers only (45-64)
plot_event_study_old <- function(df_twfe, df_sunab, outcome, out_file,
                                 main = "", ylab = "Estimate and 95% Conf. Int.",
                                 width = 18, height = 8.4) {
  fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

  fit_twfe <- feols(fml_twfe, df_twfe, vcov = ~ID_2)
  fit_sunab <- feols(fml_sunab, df_sunab, vcov = ~ID_2)

  pdf(out_file, width = width, height = height)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s",
      cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  plot.new()
  iplot(
    list(fit_twfe, fit_sunab),
    col = colours_old, lty = c(2, 2),
    xlab = "Years to treatment", ylab = ylab, main = main
  )
  add_cs(outcome, df_sunab, col = cs_orange, lty = 2)
  plot.new()
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_old, cs_orange), pch = 1, lwd = 2, lty = c(2, 2, 2), cex = cex_lab, bty = "n",
         legend = c("TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (45-64)"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_event_study_old(default_f, lfs_sum_dist_f, "work_45_64",
                     file.path(fig_dir, "work_mean_OCI_f_old.pdf"))

# 3-panel income event study (log Income, Hours, log Hourly income), old workers only (45-64)
plot_income_wide_f_old <- function(df_twfe, df_sunab, out_file, width = 18, height = 8.4) {
  outcomes <- c("log(inc_45_64)", "hours_45_64", "log(hrinc_45_64)")
  titles   <- c("log(Income)", "Hours", "log(Hourly income)")

  pdf(out_file, width = width, height = height)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    fml_twfe <- as.formula(paste0(outcome, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab <- as.formula(paste0(outcome, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe <- feols(fml_twfe, df_twfe, vcov = ~ID_2)
    fit_sunab <- feols(fml_sunab, df_sunab, vcov = ~ID_2)
    iplot(list(fit_twfe, fit_sunab),
          col = colours_old, lty = c(2, 2),
          xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i])
    add_cs(outcome, lfs_sum_dist_f, col = cs_orange, lty = 2)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_old, cs_orange), pch = 1, lwd = 2, lty = c(2, 2, 2), cex = cex_lab, bty = "n",
         legend = c("TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (45-64)"), horiz = T, inset = c(0, 0.04), xpd = T, x.intersp = 0.3)
  dev.off()
}

plot_income_wide_f_old(default_f, lfs_sum_dist_f, file.path(fig_dir, "inc_mean_OCI_f_old.pdf"))

# Single-panel event study, young (20-44) vs old (45-64)
plot_event_study_age <- function(df_twfe, df_sunab, outcome_young, outcome_old, out_file,
                                 main = "", ylab = "Estimate and 95% Conf. Int.",
                                 width = 18, height = 8.4) {
  fml_twfe_young  <- as.formula(paste0(outcome_young, " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_sunab_young <- as.formula(paste0(outcome_young, " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_twfe_old    <- as.formula(paste0(outcome_old,   " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
  fml_sunab_old   <- as.formula(paste0(outcome_old,   " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

  fit_twfe_young <- feols(fml_twfe_young, df_twfe, vcov = ~ID_2)
  fit_sunab_young <- feols(fml_sunab_young, df_sunab, vcov = ~ID_2)
  fit_twfe_old   <- feols(fml_twfe_old,   df_twfe, vcov = ~ID_2)
  fit_sunab_old  <- feols(fml_sunab_old,  df_sunab, vcov = ~ID_2)

  pdf(out_file, width = width, height = height)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s",
      cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)
  plot.new()
  iplot(
    list(fit_twfe_young, fit_sunab_young,
         fit_twfe_old,   fit_sunab_old),
    col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
    xlab = "Years to treatment", ylab = ylab, main = main
  )
  add_cs(outcome_young, df_sunab, col = cs_blue, offset = -0.28)
  add_cs(outcome_old, df_sunab, col = cs_orange, offset = 0.28, lty = 2)
  plot.new()
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_age, cs_blue, cs_orange), pch = 1, lwd = 2, lty = c(1, 1, 2, 2, 1, 2), cex = cex_lab, bty = "n", ncol = 3,
         legend = c("TWFE (20-44)", "Sun & Abraham (20-44)", "TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (20-44)", "Callaway & Sant'Anna (45-64)"),
         inset = c(0, 0.01), xpd = T, x.intersp = 0.4)
  dev.off()
}

# 3-panel income event study (log Income, Hours, log Hourly income), young (20-44) vs old (45-64)
plot_income_wide_f_age <- function(df_twfe, df_sunab, out_file, width = 18, height = 8.4) {
  outcomes_young <- c("log(inc_20_44)", "hours_20_44", "log(hrinc_20_44)")
  outcomes_old   <- c("log(inc_45_64)", "hours_45_64", "log(hrinc_45_64)")
  titles         <- c("log(Income)", "Hours", "log(Hourly income)")

  pdf(out_file, width = width, height = height)
  par(mfrow = c(1, 3), mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), oma = c(6, 0, 0, 0), pty = "s", cex.main = cex_main, cex.lab = cex_lab, cex.axis = cex_axis)

  for (i in seq_along(titles)) {
    fml_twfe_young  <- as.formula(paste0(outcomes_young[i], " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_young <- as.formula(paste0(outcomes_young[i], " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_twfe_old    <- as.formula(paste0(outcomes_old[i],   " ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))
    fml_sunab_old   <- as.formula(paste0(outcomes_old[i],   " ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_09) + i(year, sh_hs_09) + i(year, sh_fdi_09) + i(year, sh_it_09) + i(year, sh_migrant_09) | ID_2 + year"))

    fit_twfe_young  <- feols(fml_twfe_young,  df_twfe, vcov = ~ID_2)
    fit_sunab_young <- feols(fml_sunab_young, df_sunab, vcov = ~ID_2)
    fit_twfe_old    <- feols(fml_twfe_old,    df_twfe, vcov = ~ID_2)
    fit_sunab_old   <- feols(fml_sunab_old,   df_sunab, vcov = ~ID_2)

    iplot(list(fit_twfe_young, fit_sunab_young, fit_twfe_old, fit_sunab_old),
          col = colours_age, lty = c(1, 1, 2, 2), sep = 0.1,
          xlab = "Years to treatment", ylab = if (i == 1) "Estimate and 95% Conf. Int." else "", main = titles[i])
    add_cs(outcomes_young[i], df_sunab, col = cs_blue, offset = -0.28)
    add_cs(outcomes_old[i], df_sunab, col = cs_orange, offset = 0.28, lty = 2)
  }

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), pty = "m", new = T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  legend("bottom", col = c(colours_age, cs_blue, cs_orange), pch = 1, lwd = 2, lty = c(1, 1, 2, 2, 1, 2), cex = cex_lab, bty = "n", ncol = 3,
         legend = c("TWFE (20-44)", "Sun & Abraham (20-44)", "TWFE (45-64)", "Sun & Abraham (45-64)", "Callaway & Sant'Anna (20-44)", "Callaway & Sant'Anna (45-64)"),
         inset = c(0, 0.04), xpd = T, x.intersp = 0.4)
  dev.off()
}

plot_income_wide_f_age(default_f, lfs_sum_dist_f, file.path(fig_dir, "inc_mean_OCI_f_age.pdf"))

plot_event_study_age(default_f, lfs_sum_dist_f, "work_20_44", "work_45_64",
                     file.path(fig_dir, "work_mean_OCI_f_age.pdf"))
