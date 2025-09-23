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
         wt = WIEGH_DCTDT) %>%
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(c10 == 1, 1, 0),
    work2 = ifelse(c21 == 1, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    agri = ifelse(ind < 50, 1, 0),
    manu = ifelse(ind > 99 & ind < 350, 1, 0),
    service = ifelse(ind > 449, 1, 0),
    nonagri = ifelse(ind > 50, 1, 0),
    monthint = ifelse(kydt == 1, 4, 10),
    year = 2010
  ) %>%
  dplyr::select(year, tinh, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month))

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
         wt = WIEGH_DCTDT) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C13 == 1, 1, 0),
    work2 = ifelse(C55 == 1, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2011
  ) %>% 
  dplyr::select(year, tinh, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt, nganhcap2) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs12 <- lfs12 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
         age = C5,
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
         wt = weigh_TDT,
         inc = TONGTHUNHAP) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C15 == 1, 1, 0),
    work2 = ifelse(C57 == 1, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2012
  ) %>% 
  group_by(tinh, dban) %>%
  ungroup() %>%
  dplyr::select(year, tinh, dban, STT, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) 

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
    work = ifelse(C16 == 1, 1, 0),
    work2 = ifelse(C43 == 1, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2013
  ) %>% 
  dplyr::select(year, tinh, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

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
    work = ifelse(C16 == 1, 1, 0),
    work2 = ifelse(C45 < 3, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    formal = ifelse(org > 2, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(org < 3 & ind > 500, 1, 0),
    year = 2014
  ) %>% 
  dplyr::select(year, tinh, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

# HỘ KINH DOANH CÁ THỂ (2014) = CƠ SỞ KD CÁ THỂ (2015)
# HỘ/CÁ NHÂN (2014) = CÁ NHÂN LÀM TỰ DO (2015) + HỘ NLTS (2015)

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
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C14 == 1 | C15 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2015
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

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
    work = ifelse(c14 == 1 | c15 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2016
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs17 <- lfs17 %>% 
  rename(tinh = TINH,
         dban = DIABAN,
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
         yearsworked = C36,
         hours = C40,
         wt = weight_final_2019,
         inc = C39) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C16 == 1 | C17 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2017
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs18 <- lfs18 %>% 
  rename(tinh = TINH,
         huyen = HUYEN,
         dban = DIABAN,
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
         yearsworked = C41,
         hours = C45,
         wt = Weight_final_2019,
         inc = C44) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C21 == 1 | C22 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2018
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs19 <- lfs19 %>% 
  rename(tinh = MATINH,
         huyen = MAHUYEN,
         dban = MADIABAN,
         hoso = HOSO,
         monthint = THANGDT,
         STT = C00,
         age = C05,
         marst = C09A,
         educattain = C17B,
         occ = C43C,
         org = C45,
         ind = C44C,
         emp = C47,
         yearsworked = C52,
         wt = Weight_final_2019,
         inc = C70) %>% 
  mutate(
    female = ifelse(C03 == 2, 1, 0),
    work = ifelse(C19 == 1 | C20 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2019
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs20 <- lfs20 %>% 
  rename(tinh = MATINH,
         huyen = MAHUYEN,
         dban = MADIABAN,
         hoso = HOSO,
         monthint = ThangDT,
         STT = IDTV,
         age = C05,
         marst = C08,
         educattain = C16,
         occ = C43B,
         org = C46,
         ind = C45B,
         emp = C48,
         yearsworked = C54,
         wt = cal_weight_2020,
         inc = C72) %>% 
  mutate(
    female = ifelse(C03 == 2, 1, 0),
    work = ifelse(C21 == 1 | C20 == 1, 1, 0),
    informal = ifelse(org < 4, 1, 0),
    formal = ifelse(org > 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    year = 2020
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, occ, org, ind, emp,
         agri, manu, service, nonagri, formal, informal, inc, yearsworked, wt) %>%
  left_join(prov_umts_month) %>% 
  mutate(ttt = (year - first_year) * 12 + (monthint - first_month)) 

lfs_all <- bind_rows(lfs10, lfs11, lfs12, lfs13, lfs14, lfs15, lfs16, lfs17, lfs18, lfs19, lfs20) %>% 
  mutate(informal = ifelse(work == 0, NA, informal),
         agri_informal = agri*informal,
         manu_formal = manu*formal,
         service_formal = service*formal,
         nonagri_formal = nonagri*formal,
         ytt = floor(ttt/12)) 

lfs_sum_fn <- function(i){
  i %>% 
    summarise(
      work = weighted.mean(work, wt, na.rm = T),
      informal = weighted.mean(informal, wt, na.rm = T),
      formal = weighted.mean(formal, wt, na.rm = T),
      agri_informal = weighted.mean(agri_informal, wt, na.rm = T),
      manu_formal = weighted.mean(manu_formal, wt, na.rm = T),
      service_formal = weighted.mean(service_formal, wt, na.rm = T),
      nonagri_formal = weighted.mean(nonagri_formal, wt, na.rm = T)
    )
}

lfs_sum_prov<- lfs_all %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(ytt, ttt, tinh, year) %>% 
  lfs_sum_fn() %>% 
  group_by(tinh, year) %>% 
  mutate(prov_year = cur_group_id()) %>% 
  ungroup()

lfs_sum_f <- lfs_all %>% 
  filter(female == 1 & age > 19 & age < 65) %>% 
  group_by(year) %>% 
  lfs_sum_fn()

save(lfs_sum_prov, file = "Clean data/lfs_sum_prov.Rda")
