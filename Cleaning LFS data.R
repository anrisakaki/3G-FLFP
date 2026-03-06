# LFS 2010 - LFS 2018 uses VSIC 07
# LFS 2019 - uses VSIC 18

lfs10 <- lfs10 %>% 
  rename(age = C5,
         ethnicity = C6A,
         marst = c8,
         educattain = c9,
         occ = c14, # ISCO 08
         emp = c15,
         org = c16,
         ind = c18, # VSCO 09
         enterprise_sz = c19,
         yearsworked = c20,
         hours = c22,
         inc = c28,
         rural = ttnt,
         wt = WIEGH_DCTDT) %>%
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(c10 == 1 | c11 == 1 | c12 == 1 | c13 == 1, 1, 0),
    work2 = ifelse(c21 == 1, 1, 0),
    agri = ifelse(ind < 50, 1, 0),
    manu = ifelse(ind > 99 & ind < 350, 1, 0),
    service = ifelse(ind > 449, 1, 0),
    nonagri = ifelse(ind > 50, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    monthint = ifelse(kydt == 1, 4, 10),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2010
  ) %>%
  left_join(lfs10_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work,
                occ, org, ind, emp, hhbus, agri, manu, service, nonagri, inc, yearsworked, wt) 

lfs11 <- lfs11 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         ethnicity = C6A,
         marst = C12,
         educattain = C11,
         occ = C35,
         org = C36,
         ind = C38,
         enterprise_sz = C40,
         emp = C41,
         payment = C43,
         loc = C45,
         yearsworked = C46,
         inc = C48,
         hours = C52,
         wt = WIEGH_DCTDT,
         rural = TTNT) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C13 == 1 | C14 == 1 | C15 < 3 | C16 < 3| C18 == 1, 1, 0),
    unpaid = ifelse(C14 == 1, 1, 0),
    work2 = ifelse(C55 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    socinsur = ifelse(C39B == 1, 1, 0),
    taxid = ifelse(C39A == 1, 1, 0),
    accounting = ifelse(C39C == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2011
  ) %>% 
  left_join(lfs11_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, taxid, socinsur, accounting, inc, yearsworked, wt) 

lfs12_weights <- lfs12.a %>% 
  select(TINH, DIABAN, STT, C2, C3, C4T, C4N, C5, weigh_TDT)

lfs12 <- lfs12 %>% 
  left_join(lfs12_weights) %>% 
  distinct() %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         hoso = HOSO,
         age = C5,
         monthint = THANGDT,
         ethnicity = C6A,
         marst = C14,
         educattain = C13,
         occ = C37,
         org = C38,
         ind = C40,
         enterprise_sz = C42,
         emp = C43,
         payment = C45,
         loc = C47,
         yearsworked = C48,
         hours = C53,
         inc = thunhap1,
         wt = weigh_TDT) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C15 == 1| C16 == 1 | C17 < 3 | C18 < 3 | C20 == 1, 1, 0),
    unpaid = ifelse(C16 == 1, 1, 0),
    work2 = ifelse(C57 == 1, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    erc = ifelse(C41A == 1, 1, 0),
    socinsur = ifelse(C41C == 1, 1, 0),
    taxid = ifelse(C41B == 1, 1, 0),
    accounting = ifelse(C41D == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2012
  ) %>% 
  left_join(lfs12_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, taxid, socinsur, accounting, inc, yearsworked, wt) 

lfs13 <- lfs13 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         ethnicity = C6A,
         marst = C8,
         educattain = C15,
         occ = C22,
         org = C23,
         ind = C25,
         loc = C26,
         enterprise_sz = C29,
         emp = C28,
         payment = C31,
         yearsworked = C33,
         hours = C39,
         wt = WIEGH_DCTDT,
         inc = TONGTHUNHAP) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C16 == 1 | C17 == 1 | C18 == 1 | C19 < 3 | C21 == 1, 1, 0),
    unpaid = ifelse(C17 == 1, 1, 0),
    work2 = ifelse(C43 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    erc = ifelse(C27A == 1, 1, 0),
    taxid = ifelse(C27B == 1, 1, 0),
    socinsur = ifelse(C27C == 1, 1, 0),
    accounting = ifelse(C27D == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2013
  ) %>% 
  left_join(lfs13_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, taxid, socinsur, accounting, inc, yearsworked, wt) 

lfs14 <- lfs14 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         ethnicity = C6A,
         marst = C8,
         educattain = C15,
         occ = C22,
         org = C23,
         ind = C25,
         loc = C26,
         enterprise_sz = C29,
         emp = C28,
         payment = C31,
         yearsworked = C34,
         hours = C41,
         wt = WIEGH_DCTDT,
         inc = TONGTHUNHAP) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C16 == 1 | C17 == 1 | C18 == 1 | C19 < 3 | C21 == 1, 1, 0),
    unpaid = ifelse(C17 == 1, 1, 0),
    work2 = ifelse(C45 < 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    erc = ifelse(C27A == 1, 1, 0),
    taxid = ifelse(C27B == 1, 1, 0),
    socinsur = ifelse(C27C == 1, 1, 0),
    accounting = ifelse(C27D == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2014
  ) %>% 
  left_join(lfs14_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, taxid, socinsur, accounting, inc, yearsworked, wt) 

lfs15 <- lfs15 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         huyen = HUYEN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         marst = C7,
         educattain = C12,
         occ = C22,
         org = C24,
         ind = C23,
         loc = C27,
         emp = C28,
         payment = C31,
         yearsworked = C37,
         hours = C41,
         wt = Weight_final_2019,
         inc = C40A) %>% 
  mutate(
    C14 = as.numeric(C14),
    C15 = as.numeric(C15),
    C16 = as.numeric(C16),
    C19 = as.numeric(C19),
    C21 = as.numeric(C21),
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C14 == 1 | C15 == 1 | C16 == 1 | C19 == 1 | C21 == 1, 1, 0),
    unpaid = ifelse(C16 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    erc = ifelse(C26 == 1, 1, 0),
    socinsur = ifelse(C32 == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2015
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, socinsur, inc, yearsworked, wt) 

lfs16 <- lfs16 %>% 
  rename(tinh = TINH,
         huyen = Mahuyen,
         hoso = Hoso,
         STT = stt,
         monthint = THANGDT,
         age = c5,
         marst = c7,
         educattain = c12,
         occ = c22,
         org = c24,
         ind = c23,
         loc = c27,
         emp = c28,
         payment = c31,
         yearsworked = c37,
         hours = c41,
         wt = Weight_final_2019,
         inc = c40) %>% 
  mutate(
    female = ifelse(c3 == 2, 1, 0),
    work = ifelse(c14 == 1 | c15 == 1 | c16 == 1 | c19 == 1 | c21 == 1, 1, 0),
    unpaid = ifelse(c16 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    socinsur = ifelse(c32 == 1, 1, 0),
    erc = ifelse(c26 == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2016
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, socinsur, inc, yearsworked, wt) 

lfs17 <- lfs17 %>% 
  rename(tinh = TINH,
         huyen = HUYEN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         marst = C9,
         educattain = C14,
         occ = C24,
         org = C26,
         ind = C25,
         emp = C30,
         payment = C33,
         wt = weight_final_2019) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C16 == 1 | C17 == 1 | C18 == 1 | C21 == 1 | C22 == 1, 1, 0),
    unpaid = ifelse(C18 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    socinsur = ifelse(C34 == 1, 1, 0),
    erc = ifelse(C28 == 1, 1, 0),
    inc = NA_real_,
    year = 2017
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, socinsur, inc, wt) 

lfs18 <- lfs18 %>% 
  rename(tinh = TINH,
         huyen = HUYEN,
         hoso = HOSO,
         monthint = THANGDT,
         age = C5,
         marst = C9,
         educattain = C17,
         occ = C29C,
         org = C31,
         ind = C30C,
         emp = C35,
         payment = C38,
         wt = Weight_final_2019) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C21 == 1 | C22 == 1 | C23 == 1 | C26 == 1 | C27 == 1, 1, 0),
    unpaid = ifelse(C22 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    socinsur = ifelse(C34 == 1, 1, 0),
    erc = ifelse(C28 == 1, 1, 0),
    inc = NA_real_,
    year = 2018
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, nonagri, erc, socinsur, inc, wt) 


lfs_all <- bind_rows(lfs10, lfs11, lfs12, lfs13, lfs14, lfs15, lfs16, lfs17, lfs18) %>% 
  mutate(work = ifelse(is.na(work), 0, work),
         huyen = ifelse(tinh == 87 & huyen == 868, 870, huyen),
         huyen = ifelse(tinh == 87 & huyen == 866, 873, huyen),
         huyen = ifelse(tinh == 4 & huyen == 51, 40, huyen)) 

save(lfs_all, file = "Clean data/lfs_all.Rda")
write_dta(lfs_all, "Clean data/lfs_all.dta")

lfs_sum <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 1) %>% 
  group_by(year) %>% 
  summarise(
    nworkers = sum(work == 1, na.rm = T),
    work = mean(work, na.rm = T),
    hhbus = mean(hhbus, na.rm = T),
    unpaid = sum(unpaid == 1, na.rm = T),
    agri = mean(agri, na.rm = T),
    manu = mean(manu, na.rm = T),
    service = mean(service, na.rm = T),
    taxid = mean(taxid, na.rm = T),
    socinsur = mean(socinsur, na.rm = T),
    inc = mean(inc, na.rm = T)
  ) %>% 
  mutate(unpaid = unpaid/nworkers,
         unpaid = ifelse(year == 2010, NA, unpaid))

# By district
lfs_sum_dist_fn <- function(i){
  i %>%
    summarise(
      n = n(),
      work = mean(work, na.rm = T),
      hhbus = mean(hhbus, na.rm = T),
      unpaid = mean(unpaid, na.rm = T),
      agri = mean(agri, na.rm = T),
      manu = mean(manu, na.rm = T),
      service = mean(service, na.rm = T),
      taxid = mean(taxid, na.rm = T),
      socinsur = mean(socinsur, na.rm = T),
      inc = mean(inc, na.rm = T)
    )
}

# Age groups 

lfs_sum_dist_all <- lfs_all %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_29 <- lfs_all %>% 
  filter(age > 19 & age < 30) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_29"), -c(year, tinh, huyen))

lfs_sum_dist_30_39 <- lfs_all %>% 
  filter(age > 29 & age < 40) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())%>%
  rename_with(~paste0(.,"_30_39"), -c(year, tinh, huyen))

lfs_sum_dist_40_49 <- lfs_all %>% 
  filter(age > 39 & age < 50) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_40_49"), -c(year, tinh, huyen))

lfs_sum_dist_20_49 <- lfs_all %>% 
  filter(age > 19 & age < 50) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_49"), -c(year, tinh, huyen))

lfs_sum_dist_50_59 <- lfs_all %>% 
  filter(age > 49 & age < 60) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_59"), -c(year, tinh, huyen))

lfs_sum_dist_60_64 <- lfs_all %>% 
  filter(age > 59 & age < 65) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_60_64"), -c(year, tinh, huyen))

lfs_sum_dist_50_64 <- lfs_all %>% 
  filter(age > 49 & age < 65) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_64"), -c(year, tinh, huyen))

lfs_sum_dist <- list(lfs_sum_dist_all, lfs_sum_dist_20_29, lfs_sum_dist_30_39, lfs_sum_dist_40_49,
                     lfs_sum_dist_50_59, lfs_sum_dist_60_64, lfs_sum_dist_20_49, lfs_sum_dist_50_64, dist_3G, export_ctrl) %>% 
  reduce(full_join) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  mutate(ytt_OCI = year - year_OCI,
         ytt_mean_OCI = year - year_mean_OCI,
         ytt_med_OCI = year - year_med_OCI,
         ytt_mean_CB = year - year_mean_CB,
         ytt_med_CB = year - year_med_CB,
         across(starts_with("ytt"), ~replace(., is.na(.), 0)),
         across(starts_with("year"), ~replace(., is.na(.), 0))) %>% 
  select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB"), everything()) 

# Female 

lfs_sum_dist_all_f <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_29_f <- lfs_all %>% 
  filter(age > 19 & age < 30 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_29"), -c(year, tinh, huyen))

lfs_sum_dist_30_39_f <- lfs_all %>% 
  filter(age > 29 & age < 40 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())%>%
  rename_with(~paste0(.,"_30_39"), -c(year, tinh, huyen))

lfs_sum_dist_40_49_f <- lfs_all %>% 
  filter(age > 39 & age < 50 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_40_49"), -c(year, tinh, huyen))

lfs_sum_dist_20_49_f <- lfs_all %>% 
  filter(age > 19 & age < 50 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_49"), -c(year, tinh, huyen))

lfs_sum_dist_50_59_f <- lfs_all %>% 
  filter(age > 49 & age < 60 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_59"), -c(year, tinh, huyen))

lfs_sum_dist_60_64_f <- lfs_all %>% 
  filter(age > 59 & age < 65 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_60_64"), -c(year, tinh, huyen))

lfs_sum_dist_50_64_f <- lfs_all %>% 
  filter(age > 49 & age < 65 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_64"), -c(year, tinh, huyen))

lfs_sum_dist_f <- list(lfs_sum_dist_all_f, lfs_sum_dist_20_29_f, lfs_sum_dist_30_39_f, lfs_sum_dist_40_49_f,
                       lfs_sum_dist_50_59_f, lfs_sum_dist_60_64_f, lfs_sum_dist_20_49_f, lfs_sum_dist_50_64_f, dist_3G, export_ctrl) %>% 
  reduce(full_join) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  mutate(ytt_OCI = year - year_OCI,
         ytt_mean_OCI = year - year_mean_OCI,
         ytt_med_OCI = year - year_med_OCI,
         ytt_mean_CB = year - year_mean_CB,
         ytt_med_CB = year - year_med_CB,
         across(starts_with("ytt"), ~replace(., is.na(.), 0)),
         across(starts_with("year"), ~replace(., is.na(.), 0))) %>% 
  select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB"), everything()) 

# Male 

lfs_sum_dist_all_m <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_29_m <- lfs_all %>% 
  filter(age > 19 & age < 30 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_29"), -c(year, tinh, huyen))

lfs_sum_dist_30_39_m <- lfs_all %>% 
  filter(age > 29 & age < 40 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())%>%
  rename_with(~paste0(.,"_30_39"), -c(year, tinh, huyen))

lfs_sum_dist_40_49_m <- lfs_all %>% 
  filter(age > 39 & age < 50 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_40_49"), -c(year, tinh, huyen))

lfs_sum_dist_20_49_m <- lfs_all %>% 
  filter(age > 19 & age < 50 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_49"), -c(year, tinh, huyen))

lfs_sum_dist_50_59_m <- lfs_all %>% 
  filter(age > 49 & age < 60 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_59"), -c(year, tinh, huyen))

lfs_sum_dist_60_64_m <- lfs_all %>% 
  filter(age > 59 & age < 65 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_60_64"), -c(year, tinh, huyen))

lfs_sum_dist_50_64_m <- lfs_all %>% 
  filter(age > 49 & age < 65 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>%  
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_50_64"), -c(year, tinh, huyen))

lfs_sum_dist_m <- list(lfs_sum_dist_all_m, lfs_sum_dist_20_29_m, lfs_sum_dist_30_39_m, lfs_sum_dist_40_49_m,
                       lfs_sum_dist_50_59_m, lfs_sum_dist_60_64_m, lfs_sum_dist_20_49_m, lfs_sum_dist_50_64_m, dist_3G, export_ctrl) %>% 
  reduce(full_join) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  mutate(ytt_OCI = year - year_OCI,
         ytt_mean_OCI = year - year_mean_OCI,
         ytt_med_OCI = year - year_med_OCI,
         ytt_mean_CB = year - year_mean_CB,
         ytt_med_CB = year - year_med_CB,
         across(starts_with("ytt"), ~replace(., is.na(.), 0)),
         across(starts_with("year"), ~replace(., is.na(.), 0))) %>% 
  select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB"), everything()) 

save(lfs_sum_dist_m, file = "Clean data/lfs_sum_dist_m.Rda")
write_dta(lfs_sum_dist_m, "Clean data/lfs_sum_dist_m.dta")
save(lfs_sum_dist_f, file = "Clean data/lfs_sum_dist_f.Rda")
write_dta(lfs_sum_dist_f, "Clean data/lfs_sum_dist_f.dta")
save(lfs_sum_dist, file = "Clean data/lfs_sum_dist.Rda")
write_dta(lfs_sum_dist, "Clean data/lfs_sum_dist.dta")

#########################
# PREPARING LFS FOR DDD #
#########################

lfs_sum_dist_20_49_ddd <- lfs_sum_dist_20_49 %>% 
  rename_with(~ str_remove(.x, "_20_49$")) %>% 
  mutate(agegr = 1)

lfs_sum_dist_50_64_ddd <- lfs_sum_dist_50_64 %>% 
  rename_with(~ str_remove(.x, "_50_64$")) %>% 
  mutate(agegr = 0)

lfs_sum_dist_ddd <- bind_rows(
  lfs_sum_dist_20_49_ddd,
  lfs_sum_dist_50_64_ddd
) %>% 
  merge(dist_3G) %>% 
  merge(export_ctrl) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  mutate(
    ytt_OCI = year - year_OCI,
    ytt_mean_OCI = year - year_mean_OCI,
    ytt_med_OCI = year - year_med_OCI,
    ytt_mean_CB = year - year_mean_CB,
    ytt_med_CB = year - year_med_CB,
    across(starts_with("ytt"), ~ replace(., is.na(.), 0)),
    across(starts_with("year"), ~ replace(., is.na(.), 0)),
    post_med_OCI = ifelse(ytt_med_OCI >= 0, 1, 0),
    treatpost_med_OCI = post_med_OCI * med_3G_OCI
  )

# Female

lfs_sum_dist_20_49_f_ddd <- lfs_sum_dist_f %>% 
  select(year, ID_2, ends_with("_20_49")) %>% 
  rename_with(~ str_remove(.x, "_20_49$")) %>% 
  mutate(agegr = 1)

lfs_sum_dist_50_64_f_ddd <- lfs_sum_dist_f %>% 
  select(year, ID_2, ends_with("_50_64")) %>% 
  rename_with(~ str_remove(.x, "_50_64$")) %>% 
  mutate(agegr = 0)

lfs_sum_dist_f_ddd <- bind_rows(
  lfs_sum_dist_20_49_f_ddd,
  lfs_sum_dist_50_64_f_ddd
) %>% 
  merge(dist_3G) %>% 
  merge(export_ctrl) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  ungroup() %>% 
  mutate(
    ytt_OCI = year - year_OCI,
    ytt_mean_OCI = year - year_mean_OCI,
    ytt_med_OCI = year - year_med_OCI,
    ytt_mean_CB = year - year_mean_CB,
    ytt_med_CB = year - year_med_CB,
    across(starts_with("ytt"), ~ replace(., is.na(.), 0)),
    across(starts_with("year"), ~ replace(., is.na(.), 0)),
    post_med_OCI = ifelse(ytt_med_OCI >= 0, 1, 0),
    treatpost_med_OCI = post_med_OCI * med_3G_OCI
  )

lfs_sum_dist_20_49_m_ddd <- lfs_sum_dist_20_49_m %>% 
  rename_with(~ str_remove(.x, "_20_49$")) %>% 
  mutate(agegr = 1)

lfs_sum_dist_50_64_m_ddd <- lfs_sum_dist_50_64_m %>% 
  rename_with(~ str_remove(.x, "_50_64$")) %>% 
  mutate(agegr = 0)

lfs_sum_dist_m_ddd <- bind_rows(
  lfs_sum_dist_20_49_m_ddd,
  lfs_sum_dist_50_64_m_ddd
) %>% 
  merge(dist_3G) %>% 
  merge(export_ctrl) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mean_3G_OCI)) %>% 
  mutate(
    ytt_OCI = year - year_OCI,
    ytt_mean_OCI = year - year_mean_OCI,
    ytt_med_OCI = year - year_med_OCI,
    ytt_mean_CB = year - year_mean_CB,
    ytt_med_CB = year - year_med_CB,
    across(starts_with("ytt"), ~ replace(., is.na(.), 0)),
    across(starts_with("year"), ~ replace(., is.na(.), 0)),
    post_med_OCI = ifelse(ytt_med_OCI >= 0, 1, 0),
    treatpost_med_OCI = post_med_OCI * med_3G_OCI
  )

save(lfs_sum_dist_m_ddd, file = "Clean data/lfs_sum_dist_m_ddd.Rda")
write_dta(lfs_sum_dist_m_ddd, "Clean data/lfs_sum_dist_m_ddd.dta")
save(lfs_sum_dist_f_ddd, file = "Clean data/lfs_sum_dist_f_ddd.Rda")
write_dta(lfs_sum_dist_f_ddd, "Clean data/lfs_sum_dist_f_ddd.dta")
save(lfs_sum_dist_ddd, file = "Clean data/lfs_sum_dist_ddd.Rda")
write_dta(lfs_sum_dist_ddd, "Clean data/lfs_sum_dist_ddd.dta")

#################
# SUMMARY STATS #
#################

lfs_sum_dist %>% 
  group_by(med_3G_OCI) %>% 
  summarise(
    n = n_distinct(ID_2),
    work = mean(work[year == 2010], na.rm = T),
    agri = mean(agri[year == 2010], na.rm = T),
    manu = mean(manu[year == 2010], na.rm = T),
    service = mean(service[year == 2010], na.rm = T),
    hhbus = mean(hhbus[year == 2010], na.rm = T),
    coverage = mean(share_3G_OCI),
    coverage16 = mean(share_3G_OCI[year == 2016])
  )

lfs_sum_dist %>% 
  group_by(mean_3G_OCI) %>% 
  summarise(
    n = n_distinct(ID_2),
    work = sd(work, na.rm = T),
    agri = sd(agri, na.rm = T),
    manu = sd(manu, na.rm = T),
    service = sd(service, na.rm = T),
    hhbus = sd(hhbus, na.rm = T),
    coverage = sd(share_3G_OCI),
    coverage16 = sd(share_3G_OCI[year == 2016])
  )

lfs_sum_dist %>% 
  group_by(mean_3G_OCI) %>% 
  summarise(
    n = n_distinct(ID_2),
    work = mean(work, na.rm = T),
    agri = mean(agri, na.rm = T),
    manu = mean(manu, na.rm = T),
    service = mean(service, na.rm = T),
    hhbus = mean(hhbus, na.rm = T),
    coverage = mean(share_3G_OCI),
    coverage16 = mean(share_3G_OCI[year == 2016])
  )

lfs_sum_year_m <- lfs_sum_dist_m %>% 
  group_by(year, mean_3G_OCI) %>% 
  lfs_sum_dist_fn() 

lfs_sum_year_f <- lfs_sum_dist_f %>% 
  group_by(year, mean_3G_OCI) %>% 
  lfs_sum_dist_fn() 
