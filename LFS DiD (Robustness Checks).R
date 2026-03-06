#######
# ALL #
#######

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_med_OCI.jpeg")
iplot(list(
  feols(work ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, year_med_OCI != 2010), vcov = ~ID_2),
  feols(work ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

# Sector 

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/agri_med_OCI.jpeg")
iplot(list(
  feols(agri ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, year_med_OCI != 2010), vcov = ~ID_2),
  feols(agri ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/manu_med_OCI.jpeg")
iplot(list(
  feols(manu ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, year_med_OCI != 2010), vcov = ~ID_2),
  feols(manu ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/service_med_OCI.jpeg")
iplot(list(
  feols(service ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, year_med_OCI != 2010), vcov = ~ID_2),
  feols(service ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

# Formality 

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/hhbus_med_OCI.jpeg")
iplot(list(
  feols(hhbus ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all | ID_2 + year,
        subset(lfs_sum_dist, year_med_OCI != 2010), vcov = ~ID_2),
  feols(hhbus ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/socinsur_med_OCI.jpeg")
iplot(list(
  feols(socinsur ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, ytt_med_OCI < 6 & ytt_med_OCI > -7), vcov = ~ID_2),
  feols(socinsur ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist, year > 2010), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/taxid_med_OCI.jpeg")
iplot(list(
  feols(taxid ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2),
  feols(taxid ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

##########
# Female #
##########

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_med_OCI_f.jpeg")
iplot(list(
  feols(work ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, year_med_OCI != 2010), vcov = ~ID_2),
  feols(work ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_f, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/agri_med_OCI_f.jpeg")
iplot(list(
  feols(agri ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, year_med_OCI != 2010), vcov = ~ID_2),
  feols(agri ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_f, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/manu_med_OCI_f.jpeg")
iplot(list(
  feols(manu ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, year_med_OCI != 2010), vcov = ~ID_2),
  feols(manu ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_f, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/service_med_OCI_f.jpeg")
iplot(list(
  feols(service ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, year_med_OCI != 2010), vcov = ~ID_2),
  feols(service ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_f, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

## Formality 

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/hhbus_med_OCI_f.jpeg")
iplot(list(
  feols(hhbus ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, year_med_OCI != 2010), vcov = ~ID_2),
  feols(hhbus ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_f, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/socinsur_med_OCI_f.jpeg")
iplot(list(
  feols(socinsur ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, ytt_med_OCI > -7 & ytt_med_OCI < 6), vcov = ~ID_2),
  feols(socinsur ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist_f, ytt_med_OCI > -7 & ytt_med_OCI < 6), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/taxid_med_OCI_f.jpeg")
iplot(list(
  feols(taxid ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_f, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2),
  feols(taxid ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist_f, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

########
# Male #
########

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/work_med_OCI_m.jpeg")
iplot(list(
  feols(work ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, year_med_OCI != 2010), vcov = ~ID_2),
  feols(work ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_m, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

# Sector

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/agri_med_OCI_m.jpeg")
iplot(list(
  feols(agri ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all | ID_2 + year,
        subset(lfs_sum_dist_m, year_med_OCI != 2010), vcov = ~ID_2),
  feols(agri ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_m, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/manu_med_OCI_m.jpeg")
iplot(list(
  feols(manu ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, year_med_OCI != 2010), vcov = ~ID_2),
  feols(manu ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_m, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/service_med_OCI_m.jpeg")
iplot(list(
  feols(service ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, year_med_OCI != 2010), vcov = ~ID_2),
  feols(service ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_m, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

## Formality 

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/hhbus_med_OCI_m.jpeg")
iplot(list(
  feols(hhbus ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, year_med_OCI != 2010), vcov = ~ID_2),
  feols(hhbus ~ sunab(year_med_OCI, year)  | ID_2 + year,
        lfs_sum_dist_m, vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/socinsur_med_OCI_m.jpeg")
iplot(list(
  feols(socinsur ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, ytt_med_OCI > -7 & ytt_med_OCI < 6), vcov = ~ID_2),
  feols(socinsur ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist_m, ytt_med_OCI > -7 & ytt_med_OCI < 6), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Results/taxid_med_OCI_m.jpeg")
iplot(list(
  feols(taxid ~ i(ytt_med_OCI, mean_3G_OCI, ref = -1) + lnexport_all  | ID_2 + year,
        subset(lfs_sum_dist_m, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2),
  feols(taxid ~ sunab(year_med_OCI, year)  | ID_2 + year,
        subset(lfs_sum_dist_m, ytt_med_OCI > -7 & ytt_med_OCI < 3), vcov = ~ID_2)), xlab = "Years to treatment")
legend("topleft", col = colours, pch = 1, lwd = 2, cex = 1, bty = "n", legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()
