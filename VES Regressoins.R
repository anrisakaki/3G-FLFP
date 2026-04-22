fig_dir <- "C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results"

plot_event_study_ves <- function(df, outcome, out_file) {
  fml_twfe <- as.formula(paste0(
    "asinh(", outcome, ") ~ i(ytt_mean_OCI, mean_3G_OCI, ref = c(-1, -1000)) + lnexport_all + i(year, sh_manu_exposed_09, ref = 2009) | id + year"
  ))
  fml_sunab <- as.formula(paste0(
    "asinh(", outcome, ") ~ sunab(year_mean_OCI, year) + lnexport_all + i(year, sh_manu_exposed_09, ref = 2009) | id + year"
  ))

  jpeg(out_file)
  iplot(
    list(
      feols(fml_twfe, df, vcov = ~ID_2),
      feols(fml_sunab, df, vcov = ~ID_2)
    ),
    i.select = 1,
    xlab = "Years to treatment"
  )
  legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
  dev.off()
}

# All firms
plot_event_study_ves(ves_all, "asinh(fworkers)", file.path(fig_dir, "ves_fworkers_mean_OCI.jpeg"))
plot_event_study_ves(ves_all, "asinh(mworkers)", file.path(fig_dir, "ves_mworkers_mean_OCI.jpeg"))

# SOE
plot_event_study_ves(subset(ves_all, soe == 1), "asinh(fworkers)", file.path(fig_dir, "ves_fworkers_soe_mean_OCI.jpeg"))
plot_event_study_ves(subset(ves_all, soe == 1), "asinh(mworkers)", file.path(fig_dir, "ves_mworkers_soe_mean_OCI.jpeg"))

# Private
plot_event_study_ves(subset(ves_all, private == 1), "asinh(fworkers)", file.path(fig_dir, "ves_fworkers_private_mean_OCI.jpeg"))
plot_event_study_ves(subset(ves_all, private == 1), "asinh(mworkers)", file.path(fig_dir, "ves_mworkers_private_mean_OCI.jpeg"))

# FDI
plot_event_study_ves(subset(ves_all, fdi == 1 & manu == 1), "asinh(fworkers)", file.path(fig_dir, "ves_fworkers_fdi_mean_OCI.jpeg"))
plot_event_study_ves(subset(ves_all, fdi == 1), "asinh(mworkers)", file.path(fig_dir, "ves_mworkers_fdi_mean_OCI.jpeg"))
