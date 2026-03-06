dn08 <- ec_list[[9]] %>% 
  mutate(year = 2008,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 6, 1, 0),
         private = ifelse(lhdn == 7 ,1, 0),
         fdi = ifelse(lhdn == 12, 1, 0),
         email = ifelse(co_email == 1, 1, 0),
         website= ifelse(co_web == 1, 1, 0),
         internet = ifelse(co_int == 1, 1, 0),
         online_transaction = ifelse(giaodich > 0, 1, 0),
         online_transaction = ifelse(is.na(online_transaction), 0, online_transaction),
         giaodich = ifelse(giaodich == 0, NA, giaodich),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = ld11,
         fworkers_jan = ld12,
         workers_ss_jan = ld21,
         fworkers_ss_jan = ld22,
         workers_dec = ld13,
         fworkers_dec = ld14,
         workers_ss_dec = ld23,
         fworkers_ss_dec = ld24,
         online_value = giaodich) %>% 
  select(year, tinh, huyen, ma_thue, ma_thue2, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         email, website, internet, online_transaction, online_value, soe, private, fdi, agri, manu, service)

dn09 <- ec_list[[10]] %>% 
  mutate(year = 2009,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 6, 1, 0),
         private = ifelse(lhdn == 7 ,1, 0),
         fdi = ifelse(lhdn == 12, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = ld11,
         fworkers_jan = ld12,
         workers_ss_jan = ld21,
         fworkers_ss_jan = ld22,
         workers_dec = ld13,
         fworkers_dec = ld14,
         workers_ss_dec = ld23,
         fworkers_ss_dec = ld24) %>% 
  select(year, tinh, huyen, ma_thue, ma_thue2, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn10 <- ec_list[[11]] %>% 
  mutate(year = 2010,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 6, 1, 0),
         private = ifelse(lhdn == 7 ,1, 0),
         fdi = ifelse(lhdn == 12, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = ld11,
         fworkers_jan = ld12,
         workers_ss_jan = ld21,
         fworkers_ss_jan = ld22,
         workers_dec = ld13,
         fworkers_dec = ld14,
         workers_ss_dec = ld23,
         fworkers_ss_dec = ld24) %>% 
  select(year, tinh, huyen, ma_thue, ma_thue2, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)


dn11 <- ec_list[[12]] %>% 
  mutate(year = 2011,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 6, 1, 0),
         private = ifelse(lhdn == 7 ,1, 0),
         fdi = ifelse(lhdn == 12, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld12,
         workers_ss_dec = ld21,
         fworkers_ss_dec = ld22) %>% 
  select(year, tinh, huyen, ma_thue, ma_thue2, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn12 <- ec_list[[13]] %>% 
  mutate(year = 2012,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld12,
         workers_ss_dec = ld21,
         fworkers_ss_dec = ld22) %>% 
  select(year, tinh, huyen, ma_thue, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn13 <- ec_list[[14]] %>% 
  mutate(year = 2013,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld12,
         workers_ss_dec = ld21,
         fworkers_ss_dec = ld22) %>% 
  select(year, tinh, huyen, ma_thue, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn14 <- ec_list[[15]] %>% 
  mutate(year = 2014,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld12,
         workers_ss_dec = ld21,
         fworkers_ss_dec = ld22) %>% 
  select(year, tinh, huyen, ma_thue, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn15 <- ec_list[[16]] %>% 
  mutate(year = 2015,
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld12,
         workers_ss_dec = ld21,
         fworkers_ss_dec = ld22) %>% 
  select(year, tinh, huyen, ma_thue, madn, macs, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn16 <- ec_list[[17]] %>% 
  mutate(year = 2016,
         nganh_kd = as.numeric(nganh_kd),
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld21,
         workers_ss_dec = ld31) %>% 
  select(year, tinh, huyen, ma_thue, capso, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

dn17 <- ec_list[[18]] %>% 
  mutate(year = 2017,
         nganh_kd = as.numeric(nganh_kd),
         tinh = as.numeric(tinh),
         huyen = as.numeric(huyen),
         soe = ifelse(lhdn < 5, 1, 0),
         private = ifelse(lhdn == 6 ,1, 0),
         fdi = ifelse(lhdn == 11, 1, 0),
         agri = ifelse(nganh_kd < 5100, 1, 0),
         manu = ifelse(nganh_kd > 9900 & nganh_kd < 41000, 1, 0),
         service = ifelse(nganh_kd > 43900, 1, 0)) %>% 
  rename(vsic = nganh_kd,
         workers_jan = tsld,
         fworkers_jan = tsldnu,
         workers_dec = ld11,
         fworkers_dec = ld21,
         workers_ss_dec = ld31) %>% 
  select(year, tinh, huyen, ma_thue, vsic, lhdn, starts_with("workers"), starts_with("fworkers"),
         soe, private, fdi, agri, manu, service)

ves_all <- bind_rows(dn08, dn09, dn10, dn11, dn12, dn13, dn14, dn15, dn16, dn17) %>% 
  filter(ma_thue != "") %>% 
  mutate(
    mst = if_else(
      is.na(ma_thue2) | ma_thue2 == "",
      ma_thue,
      paste0(ma_thue, ma_thue2)
    )
  ) %>% 
  group_by(tinh, huyen, mst) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup() %>% 
  select(year, id, mst, tinh, huyen, starts_with("ma_thue"), madn, capso, everything()) %>% 
  merge(dist_3G) %>% 
  merge(export_ctrl) %>% 
  mutate(ytt_OCI = year - year_OCI,
         ytt_mean_OCI = year - year_mean_OCI,
         ytt_med_OCI = year - year_med_OCI,
         ytt_mean_CB = year - year_mean_CB,
         ytt_med_CB = year - year_med_CB,
         across(starts_with("ytt"), ~replace(., is.na(.), 0)),
         across(starts_with("year"), ~replace(., is.na(.), 0))) %>% 
  rename_with(~ str_remove(.x, "_dec$"), ends_with("_dec")) %>% 
  mutate(mworkers = workers-fworkers,
         mworkers_ss = workers_ss-fworkers_ss) %>% 
  ungroup()

save(ves_all, file = "Clean data/ves_all.Rda")
write_dta(ves_all, "Clean data/ves_all.dta")
