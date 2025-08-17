vhlss_all_dist <- bind_rows(vhlss04_dist, vhlss06_dist, vhlss08_dist, vhlss10_dist,
                            vhlss12_dist, vhlss14_dist, vhlss16_dist, vhlss18_dist) %>% 
  group_by(year, tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         children_u7 = sum(age < 7, na.rm = T),
         nchild = sum(age < 18, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(prov2018, dist2018) %>%
  mutate(dist = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(formal = ifelse(work == 1 & informal == 0, 1, 0),
         informal = ifelse(work == 0, NA, informal),
         formal = ifelse(work == 0, NA, formal),
         wagework = ifelse(work == 0, NA, wagework),
         oab_agri = ifelse(work == 0, NA, oab_agri),
         oab_nonagri = ifelse(work == 0, NA, oab_nonagri),
         agri_informal = agri*informal,
         manu_informal = manu*informal,
         service_informal = service*informal,
         agri_formal = agri*formal,
         manu_formal = manu*formal,
         service_formal = service*formal) %>% 
  select(year, prov2018, dist2018, dist, tinh, huyen, xa, diaban, hoso, everything()) %>% 
  left_join(umts_dist) %>% 
  mutate(time_to_treat = year - first_treated)

save(vhlss_all_dist, file = "Clean data/vhlss_all_dist.Rda")
write_dta(vhlss_all_dist, "Clean data/vhlss_all_dist.dta")

# summarising at district level 
vhlss_dist_did_sum <- function(i){
  i %>% 
    mutate(time_to_treat = ifelse(is.na(first_treated), -1000, time_to_treat),
           first.treat.csdid = ifelse(is.na(first_treated), 0, first_treated),
           first_treated2 = ifelse(time_to_treat == -1000, 10000, first_treated)) %>% 
    group_by(year, dist, time_to_treat, first_treated2, first.treat.csdid) %>% 
    summarise(
      work = weighted.mean(work, hhwt, na.rm = T),
      wagework = weighted.mean(wagework, hhwt, na.rm = T),
      oab_agri = weighted.mean(oab_agri, hhwt, na.rm = T),
      oab_nonagri = weighted.mean(oab_nonagri, hhwt, na.rm = T),
      informal = weighted.mean(informal, hhwt, na.rm = T),
      agri = weighted.mean(agri, hhwt, na.rm = T),
      manu = weighted.mean(manu, hhwt, na.rm = T),
      service = weighted.mean(service, hhwt, na.rm = T),
      agri_informal = weighted.mean(agri_informal, hhwt, na.rm = T),
      manu_informal = weighted.mean(manu_informal, hhwt, na.rm = T),
      service_informal = weighted.mean(service_informal, hhwt, na.rm = T),
      agri_formal = weighted.mean(agri_formal, hhwt, na.rm = T),
      manu_formal = weighted.mean(manu_formal, hhwt, na.rm = T),
      service_formal = weighted.mean(service_formal, hhwt, na.rm = T)
    ) %>% 
    mutate(coverage = ifelse(time_to_treat > -1, 1, 0),
           coverage = ifelse(year < 2010, NA, coverage))
}


vhlss_dist_did <- vhlss_all_dist %>% 
  filter(age > 19 & age < 65) %>% 
  vhlss_dist_did_sum()

vhlss_f_dist_did <- vhlss_all_dist %>% 
  filter(age > 19 & age < 65 & female == 1) %>% 
  vhlss_dist_did_sum()

vhlss_m_dist_did <- vhlss_all_dist %>% 
  filter(age > 19 & age < 65 & female == 0) %>% 
  vhlss_dist_did_sum()

save(vhlss_dist_did, file = "Clean data/vhlss_dist_did.Rda")
save(vhlss_f_dist_did, file = "Clean data/vhlss_f_dist_did.Rda")
save(vhlss_m_dist_did, file = "Clean data/vhlss_m_dist_did.Rda")

vhlss_hhbus_did <- hhbus_all %>% 
  mutate(time_to_treat = ifelse(is.na(first_treated), -1000, time_to_treat),
         first.treat.csdid = ifelse(is.na(first_treated), 0, first_treated),
         first_treated2 = ifelse(time_to_treat == -1000, 10000, first_treated)) %>% 
  group_by(year, distid, time_to_treat, first_treated2, first.treat.csdid) %>% 
  summarise(
    erc = weighted.mean(erc, hhwt, na.rm = T),
    manu = weighted.mean(manu, hhwt, na.rm = T),
    service = weighted.mean(service, hhwt, na.rm = T),
    manu_erc = weighted.mean(manu_erc, hhwt, na.rm = T),
    service_erc = weighted.mean(service_erc, hhwt, na.rm = T),
    annual_rev = weighted.mean(annual_rev, hhwt, na.rm = T)
  ) %>% 
  mutate(coverage = ifelse(time_to_treat > -1, 1, 0),
         coverage = ifelse(year < 2010, NA, coverage))

save(vhlss_hhbus_did, file = "Clean data/vhlss_hhbus_did.Rda")