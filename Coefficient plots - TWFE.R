setFixest_coefplot(dict = dict, grid = F, zero.par = list( type="dotted", lty=2), main = "")

# Work
png("Results/work_twfe.png")
iplot(feols(work ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/work_mf_twfe.png")
iplot(
  list(
    feols(work ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(work ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment", split = TRUE,
  dodge = 0.3
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# Wage work

png("Results/wagework_twfe.png")
iplot(feols(wagework ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/wagework_mf_twfe.png")
iplot(
  list(
    feols(wagework ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(wagework ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# OAB - Agri 

png("Results/oab_agri_twfe.png")
iplot(feols(oab_agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/oab_agri_mf_twfe.png")
iplot(
  list(
    feols(oab_agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(oab_agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# OAB - Nonagri 

png("Results/oab_nonagri_twfe.png")
iplot(feols(oab_nonagri ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/oab_nonagri_mf_twfe.png")
iplot(
  list(
    feols(oab_nonagri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(oab_nonagri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# Informal
png("Results/informal_twfe.png")
iplot(feols(informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/informal_mf_twfe.png")
iplot(
  list(
    feols(informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# Agri 

png("Results/agri_twfe.png")
iplot(feols(agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/agri_mf_twfe.png")
iplot(
  list(
    feols(agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(agri ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

png("Results/agri_informal_twfe.png")
iplot(feols(agri_informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/agri_informal_mf_twfe.png")
iplot(
  list(
    feols(agri_informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(agri_informal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# Manu 

png("Results/manu_twfe.png")
iplot(feols(manu ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/manu_mf_twfe.png")
iplot(
  list(
    feols(manu ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(manu ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

png("Results/manu_formal_twfe.png")
iplot(feols(manu_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/manu_formal_mf_twfe.png")
iplot(
  list(
    feols(manu_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(manu_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

# Service 

png("Results/service_twfe.png")
iplot(feols(service ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/service_mf_twfe.png")
iplot(
  list(
    feols(service ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(service ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()

png("Results/service_formal_twfe.png")
iplot(feols(service_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
              dist + year, vhlss_dist_did, vcov = ~dist), xlab = "Years to treatment")
dev.off()

png("Results/service_formal_mf_twfe.png")
iplot(
  list(
    feols(service_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_m_dist_did, vcov = ~dist),
    feols(service_formal ~ i(time_to_treat, ref = c(-2, -1000)) | 
            dist + year, vhlss_f_dist_did, vcov = ~dist)
  ), xlab = "Years to treatment"
)
legend("bottomleft", col = 1:2, pch = 1, lwd = 2, cex = 0.7, bty = "n", 
       legend = c("Male", "Female"))
dev.off()
