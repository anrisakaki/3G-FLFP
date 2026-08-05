library(tidyverse)   
library(haven)       
library(kableExtra)

lfs10_distid <- read.csv("Raw Data/LFS/lfs_dist_10.csv")
lfs11_distid <- read.csv("Raw Data/LFS/lfs_dist_11.csv")
lfs12_distid <- read.csv("Raw Data/LFS/lfs_dist_12.csv")
lfs13_distid <- read.csv("Raw Data/LFS/lfs_dist_13.csv")
lfs14_distid <- read.csv("Raw Data/LFS/lfs_dist_14.csv")

lfs10   <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS_2010_final_DCTDT_GUI.sav")
lfs11   <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS_2011_final_DCTDT_GUI.sav")
lfs12   <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS-2012-add_var_weight_goc.sav")
lfs12.a <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS_2012_final_DCTDT_GUI.sav")
lfs13   <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS_2013_final_DCTDT_GUI.sav")
lfs14   <- read_sav("Raw Data/LFS/Micr_LFS_2010-2014/LFS_2014_final_DCTDT_GUI.sav")
lfs15   <- read_dta("Raw Data/LFS/LFS_2015_final_full.dta")
lfs16   <- read_dta("Raw Data/LFS/LFS_2016_final_full.dta")
lfs17   <- read_dta("Raw Data/LFS/LFS_2017_final_full.dta")

export_ctrl <- read_dta("Clean data/province level/export_province_year_isic4.dta")
load("Clean data/dist_3G.Rda")                 
load("Clean data/district_controls_09.Rda")

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
    work2 = ifelse(c21 == 9, NA, ifelse(c21 == 1, 1, 0)),
    agri = ifelse(ind < 50, 1, 0),
    manu = ifelse(ind > 99 & ind < 350, 1, 0),
    service = ifelse(ind > 449, 1, 0),
    construction = ifelse(ind > 390 & ind < 450, 1, 0),
    nonagri = ifelse(ind > 50, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    monthint = ifelse(kydt == 1, 4, 10),
    inc = ifelse(inc <= 0, NA, inc),
    migrant = NA_real_,
    mig5 = NA_real_,
    mig_jobsearch = NA_real_,
    mig_newjob = NA_real_,
    mig_rural_urban = NA_real_,
    year = 2010
  ) %>%
  left_join(lfs10_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, work2,
                migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban,
                occ, org, ind, emp, hhbus, agri, manu, service, construction, nonagri, inc, hours, yearsworked, wt) 

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
    work2 = ifelse(C55 < 3, 1, 0),
    agri2 = ifelse(C59 < 500, 1, 0),
    manu2 = ifelse(C59 > 990 & C59 < 3500, 1, 0),
    service2 = ifelse(C59 > 4500, 1, 0),
    migrant = ifelse(C8 == 2 | C8 == 3, 1, 0),
    mig5 = NA_real_,
    mig_jobsearch = ifelse(migrant == 1, ifelse(C10 == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(C10 == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(C9 == 2 & rural == 1, 1, 0), NA),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 3, 1, 0),
    socinsur = ifelse(C39B == 1, 1, 0),
    taxid = ifelse(C39A == 1, 1, 0),
    accounting = ifelse(C39C == 1, 1, 0),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2011
  ) %>% 
  left_join(lfs11_distid) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, work2,
                migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban,
                unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, agri2, manu2, service2, construction, nonagri, taxid, socinsur, accounting, inc, hours,
                yearsworked, wt) 

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
    work2 = ifelse(C57 < 3, 1, 0),
    agri2 = ifelse(C61 < 500, 1, 0),
    manu2 = ifelse(C61 > 990 & C61 < 3500, 1, 0),
    service2 = ifelse(C61 > 4500, 1, 0),
    migrant = ifelse(C8 == 1 | C8 == 2, 1, 0),
    mig5 = NA_real_,
    mig_jobsearch = ifelse(migrant == 1, ifelse(C10 == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(C10 == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(C9 == 2 & TTNT == 1, 1, 0), NA),
    formal = ifelse(org > 2, 1, 0),
    informal = ifelse(org < 3, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
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
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, agri2, manu2, service2, construction, nonagri, erc, taxid, socinsur, accounting, inc, hours,
                yearsworked, wt) 

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
    work2 = ifelse(C43 < 3, 1, 0),
    agri2 = ifelse(C47 < 500, 1, 0),
    manu2 = ifelse(C47 > 990 & C47 < 3500, 1, 0),
    service2 = ifelse(C47 > 4500, 1, 0),
    migrant = ifelse(C9 == 1 | C9 == 2, 1, 0),
    mig5 = NA_real_,
    mig_jobsearch = ifelse(migrant == 1, ifelse(C12 == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(C12 == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(C10 == 2 & TTNT == 1, 1, 0), NA),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
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
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, agri2, manu2, service2, construction, nonagri, erc, taxid, socinsur, accounting, inc, hours,
                yearsworked, wt) 

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
    agri2 = ifelse(C49 < 500, 1, 0),
    manu2 = ifelse(C49 > 990 & C49 < 3500, 1, 0),
    service2 = ifelse(C49 > 4500, 1, 0),
    migrant = ifelse(C9 == 1 | C9 == 2, 1, 0),
    mig5 = NA_real_,
    mig_jobsearch = ifelse(migrant == 1, ifelse(C12 == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(C12 == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(C10 == 2 & TTNT == 1, 1, 0), NA),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
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
  dplyr::select(year, tinh, huyen, dban, hoso, STT, rural, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, agri2, manu2, service2, construction, nonagri, erc, taxid, socinsur, accounting, inc, hours,
                yearsworked, wt) 

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
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    erc = ifelse(C26 == 1, 1, 0),
    socinsur = ifelse(C32 == 1, 1, 0),
    work2 = ifelse(C43 == 1, 1, 0),
    migrant = ifelse(as.numeric(C8) < 4, 1, 0),
    mig5 = ifelse(as.numeric(C8) < 5, 1, 0),
    mig_jobsearch = ifelse(migrant == 1, ifelse(as.numeric(C11) == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(as.numeric(C11) == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(as.numeric(C9) == 2 & as.numeric(TTNT) == 1, 1, 0), NA),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2015
  ) %>% 
  dplyr::select(year, tinh, huyen, dban, hoso, STT, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, construction, nonagri, erc, socinsur, inc, hours, 
                yearsworked, wt) 

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
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
    erc = ifelse(c26 == 1, 1, 0),
    work2 = ifelse(c43 == 1, 1, 0),
    migrant = ifelse(as.numeric(c8) < 4, 1, 0),
    mig5 = ifelse(as.numeric(c8) < 5, 1, 0),
    mig_jobsearch = ifelse(migrant == 1, ifelse(as.numeric(c11) == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(as.numeric(c11) == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(as.numeric(c9) == 2 & as.numeric(TTNT) == 1, 1, 0), NA),
    inc = ifelse(inc <= 0, NA, inc),
    year = 2016
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, construction, nonagri, erc, socinsur, inc, hours,
                yearsworked, wt) 

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
         hours = C40A,
         inc = C39A,
         wt = weight_final_2019) %>% 
  mutate(
    female = ifelse(C3 == 2, 1, 0),
    work = ifelse(C16 == 1 | C17 == 1 | C18 == 1 | C21 == 1 | C22 == 1, 1, 0),
    unpaid = ifelse(C18 == 1, 1, 0),
    agri = ifelse(ind < 500, 1, 0),
    manu = ifelse(ind > 990 & ind < 3500, 1, 0),
    service = ifelse(ind > 4500, 1, 0),
    construction = ifelse(ind > 3900 & ind < 4500, 1, 0),
    nonagri = ifelse(ind > 500, 1, 0),
    hhbus = ifelse(org < 4, 1, 0),
    socinsur = ifelse(C34 == 1, 1, 0),
    erc = ifelse(C28 == 1, 1, 0),
    work2 = ifelse(C40B > 0, 1, 0),
    migrant = ifelse(as.numeric(C10) < 4, 1, 0),
    mig5 = ifelse(as.numeric(C10) < 5, 1, 0),
    mig_jobsearch = ifelse(migrant == 1, ifelse(as.numeric(C13) == 1, 1, 0), NA),
    mig_newjob = ifelse(migrant == 1, ifelse(as.numeric(C13) == 2, 1, 0), NA),
    mig_rural_urban = ifelse(migrant == 1, ifelse(as.numeric(C11) == 2 & as.numeric(TTNT) == 1, 1, 0), NA),
    year = 2017
  ) %>% 
  dplyr::select(year, tinh, huyen, hoso, STT, monthint, age, female, marst, educattain, work, work2, migrant, mig5, mig_jobsearch, mig_newjob, mig_rural_urban, unpaid, occ, org,
                ind, emp, hhbus, agri, manu, service, construction, nonagri, erc, socinsur, inc, hours, wt) 

lfs_all <- bind_rows(lfs10, lfs11, lfs12, lfs13, lfs14, lfs15, lfs16, lfs17) %>%
  mutate(work = ifelse(is.na(work), 0, work),
         age = as.numeric(age)) %>%
  mutate(
    ## Hà Nội (tinh 1)
    huyen = ifelse(tinh == 1  & huyen == 21,  19,  huyen),   # Nam Từ Liêm      -> Từ Liêm
    ## Tuyên Quang (tinh 8)
    huyen = ifelse(tinh == 8  & huyen == 71,  72,  huyen),   # Lâm Bình         -> Nà Hang
    ## Điện Biên (tinh 11)
    huyen = ifelse(tinh == 11 & huyen == 103, 96,  huyen),   # Nậm Pồ           -> Mường Nhé
    ## Lai Châu (tinh 12)
    huyen = ifelse(tinh == 12 & huyen == 112, 107, huyen),   # Nậm Nhùn         -> Mường Tè
    ## Sơn La (tinh 14)
    huyen = ifelse(tinh == 14 & huyen == 128, 123, huyen),   # Vân Hồ           -> Mộc Châu
    ## Nghệ An (tinh 40)
    huyen = ifelse(tinh == 40 & huyen == 432, 421, huyen),   # Hoàng Mai        -> Quỳnh Lưu
    ## Hà Tĩnh (tinh 42)
    huyen = ifelse(tinh == 42 & huyen == 449, 447, huyen),   # Kỳ Anh (thị xã)  -> Kỳ Anh
    ## Quảng Bình (tinh 44)
    huyen = ifelse(tinh == 44 & huyen == 458, 454, huyen),   # Ba Đồn           -> Quảng Trạch
    ## Ninh Thuận (tinh 58)
    huyen = ifelse(tinh == 58 & huyen == 589, 587, huyen),   # Thuận Nam        -> Ninh Phước
    ## Kon Tum (tinh 62)
    huyen = ifelse(tinh == 62 & huyen == 618, 616, huyen),   # Ia H' Drai       -> Sa Thầy
    ## Gia Lai (tinh 64)
    huyen = ifelse(tinh == 64 & huyen == 639, 633, huyen),   # Chư Pưh          -> Chư Sê
    ## Bình Phước (tinh 70)
    huyen = ifelse(tinh == 70 & huyen == 688, 691, huyen),   # Bù Gia Mập       -> Phước Long
    huyen = ifelse(tinh == 70 & huyen == 698, 691, huyen),   # Phú Riềng        -> Phước Long
    huyen = ifelse(tinh == 70 & huyen == 690, 694, huyen),   # Hớn Quản         -> Bình Long
    ## Bình Dương (tinh 74)
    huyen = ifelse(tinh == 74 & huyen == 719, 721, huyen),   # Bàu Bàng         -> Bến Cát
    huyen = ifelse(tinh == 74 & huyen == 726, 723, huyen),   # Bắc Tân Uyên     -> Tân Uyên
    ## Long An (tinh 80)
    huyen = ifelse(tinh == 80 & huyen == 795, 798, huyen),   # Kiến Tường       -> Mộc Hóa
    ## Tiền Giang (tinh 82)
    huyen = ifelse(tinh == 82 & huyen == 817, 820, huyen),   # Cai Lậy (thị xã) -> Cai Lậy
    ## Bến Tre (tinh 83)
    huyen = ifelse(tinh == 83 & huyen == 838, 833, huyen),   # Mỏ Cày Bắc       -> Mỏ Cày Nam
    ## Trà Vinh (tinh 84)
    huyen = ifelse(tinh == 84 & huyen == 851, 850, huyen),   # Duyên Hải (t.xã) -> Duyên Hải
    ## Kiên Giang (tinh 91)
    huyen = ifelse(tinh == 91 & huyen == 914, 902, huyen),   # Giang Thành      -> Kiên Lương
    ## Hậu Giang (tinh 93)
    huyen = ifelse(tinh == 93 & huyen == 937, 936, huyen),   # Long Mỹ (thị xã) -> Long Mỹ
    ## Sóc Trăng (tinh 94)
    huyen = ifelse(tinh == 94 & huyen == 951, 946, huyen)    # Trần Đề          -> Long Phú
    ## Three crosswalk-only Châu Thành renumberings -- (80,708)->808, (82,877)->821,
    ## (91,892)->905 -- are omitted: those source codes never appear in the
    ## 2010-2017 microdata, so they would never fire.
  )

save(lfs_all, file = "Clean data/lfs_all.Rda")
write_dta(lfs_all, "Clean data/lfs_all.dta")

lfs_sum <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 1) %>% 
  group_by(year) %>% 
  summarise(
    nworkers = sum(work == 1, na.rm = T),
    work = mean(work, na.rm = T),
    hhbus = mean(hhbus, na.rm = T),
    unpaid = mean(unpaid, na.rm = T),
    agri = mean(agri, na.rm = T),
    manu = mean(manu, na.rm = T),
    service = mean(service, na.rm = T),
    construction = mean(construction, na.rm = T),
    taxid = mean(taxid, na.rm = T),
    socinsur = mean(socinsur, na.rm = T),
    inc = mean(inc, na.rm = T),
    migrant = mean(migrant, na.rm = T),
    mig5 = mean(mig5, na.rm = T),
    mig_jobsearch = mean(mig_jobsearch, na.rm = T),
    mig_newjob = mean(mig_newjob, na.rm = T),
    mig_rural_urban = mean(mig_rural_urban, na.rm = T)
  )

# By district
lfs_sum_dist_fn <- function(i){
  i %>%
    summarise(
      n = n(),
      work = mean(work, na.rm = T),
      work2 = mean(work2, na.rm = T),
      migrant = mean(migrant, na.rm = T),
      mig5 = mean(mig5, na.rm = T),
      mig_jobsearch = mean(mig_jobsearch, na.rm = T),
      mig_newjob = mean(mig_newjob, na.rm = T),
      mig_rural_urban = mean(mig_rural_urban, na.rm = T),
      hhbus = mean(hhbus, na.rm = T),
      unpaid = mean(unpaid, na.rm = T),
      agri = mean(agri, na.rm = T),
      manu = mean(manu, na.rm = T),
      service = mean(service, na.rm = T),
      agri2 = mean(agri2, na.rm = T),
      manu2 = mean(manu2, na.rm = T),
      service2 = mean(service2, na.rm = T),
      taxid = mean(taxid, na.rm = T),
      socinsur = mean(socinsur, na.rm = T),
      inc = mean(inc, na.rm = T),
      hours = mean(hours, na.rm = T),
      hrinc = mean(inc/hours, na.rm = T)
    )
}

lfs_treat_fn <- function(i){
  i %>%
    distinct() %>% 
    ungroup() %>% 
    mutate(ytt_OCI = year - year_OCI,
           ytt_mean_OCI = year - year_mean_OCI,
           ytt_med_OCI = year - year_med_OCI,
           across(starts_with("ytt"), ~replace(., is.na(.), -1000)),
           across(starts_with("year"), ~replace(., is.na(.), 0)),
           post_mean_OCI = ifelse(year_mean_OCI > 0 & year >= year_mean_OCI, 1, 0),
           post_med_OCI = ifelse(year_med_OCI > 0 & year >= year_med_OCI, 1, 0)) %>% 
    select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB"), everything()) 
}

# Age groups 

lfs_sum_dist_all <- lfs_all %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_44 <- lfs_all %>% 
  filter(age > 19 & age < 45) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_44"), -c(year, tinh, huyen))

lfs_sum_dist_45_64 <- lfs_all %>% 
  filter(age > 44 & age < 65) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_45_64"), -c(year, tinh, huyen))

lfs_sum_dist_20_24 <- lfs_all %>%
  filter(age > 19 & age < 25) %>%
  group_by(year, tinh, huyen) %>%
  summarise(mig12_20_24         = mean(migrant, na.rm = T),
            mig_jobsearch_20_24 = mean(mig_jobsearch, na.rm = T),
            mig_newjob_20_24    = mean(mig_newjob, na.rm = T),
            
            mig_ru_20_24 = mean(mig_rural_urban, na.rm = T),
            mig_ru_jobsearch_20_24 = mean(mig_jobsearch[mig_rural_urban == 1], na.rm = T),
            mig_ru_newjob_20_24 = mean(mig_newjob[mig_rural_urban == 1], na.rm = T))

dist_3G <- dist_3G %>% filter(year > 2009)

lfs_sum_dist <- list(lfs_sum_dist_all, lfs_sum_dist_20_24, lfs_sum_dist_20_44, lfs_sum_dist_45_64,
                     dist_3G, district_controls_09, export_ctrl) %>% 
  reduce(full_join) %>% 
  group_by(ID_2) %>%
  filter(n_distinct(year) == 8) %>%
  lfs_treat_fn()

# Female 

lfs_sum_dist_all_f <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_44_f <- lfs_all %>% 
  filter(age > 19 & age < 45 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_44"), -c(year, tinh, huyen))

lfs_sum_dist_45_64_f <- lfs_all %>% 
  filter(age > 44 & age < 65 & female == 1) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_45_64"), -c(year, tinh, huyen))

lfs_sum_dist_20_24_f <- lfs_all %>%
  filter(age > 19 & age < 25 & female == 1) %>%
  group_by(year, tinh, huyen) %>%
  summarise(mig12_20_24         = mean(migrant, na.rm = T),
            mig_jobsearch_20_24 = mean(mig_jobsearch, na.rm = T),
            mig_newjob_20_24    = mean(mig_newjob, na.rm = T),
            
            mig_ru_20_24 = mean(mig_rural_urban, na.rm = T),
            mig_ru_jobsearch_20_24 = mean(mig_jobsearch[mig_rural_urban == 1], na.rm = T),
            mig_ru_newjob_20_24 = mean(mig_newjob[mig_rural_urban == 1], na.rm = T))

lfs_sum_dist_f <- list(lfs_sum_dist_all_f, lfs_sum_dist_20_24_f, lfs_sum_dist_20_44_f, lfs_sum_dist_45_64_f,
                       dist_3G, district_controls_09, export_ctrl) %>% 
  reduce(full_join) %>% 
  group_by(ID_2) %>%
  filter(n_distinct(year) == 8) %>%
  ungroup() %>% 
  lfs_treat_fn()

# Male 

lfs_sum_dist_all_m <- lfs_all %>% 
  filter(age > 19 & age < 65 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything())

lfs_sum_dist_20_44_m <- lfs_all %>% 
  filter(age > 19 & age < 45 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_20_44"), -c(year, tinh, huyen))

lfs_sum_dist_45_64_m <- lfs_all %>% 
  filter(age > 44 & age < 65 & female == 0) %>% 
  group_by(year, tinh, huyen) %>% 
  lfs_sum_dist_fn() %>% 
  select(year, tinh, huyen, everything()) %>%
  rename_with(~paste0(.,"_45_64"), -c(year, tinh, huyen))

lfs_sum_dist_20_24_m <- lfs_all %>%
  filter(age > 19 & age < 25 & female == 0) %>%
  group_by(year, tinh, huyen) %>%
  summarise(mig12_20_24         = mean(migrant, na.rm = T),
            mig_jobsearch_20_24 = mean(mig_jobsearch, na.rm = T),
            mig_newjob_20_24    = mean(mig_newjob, na.rm = T),
            
            mig_ru_20_24 = mean(mig_rural_urban, na.rm = T),
            mig_ru_jobsearch_20_24 = mean(mig_jobsearch[mig_rural_urban == 1], na.rm = T),
            mig_ru_newjob_20_24 = mean(mig_newjob[mig_rural_urban == 1], na.rm = T))

lfs_sum_dist_m <- list(lfs_sum_dist_all_m, lfs_sum_dist_20_24_m, lfs_sum_dist_20_44_m, lfs_sum_dist_45_64_m, 
                       dist_3G, district_controls_09, export_ctrl) %>% 
  reduce(full_join) %>% 
  group_by(ID_2) %>%
  filter(n_distinct(year) == 8) %>%
  ungroup() %>% 
  lfs_treat_fn()
  
save(lfs_sum_dist_m, file = "Clean data/lfs_sum_dist_m.Rda")
write_dta(lfs_sum_dist_m, "Clean data/lfs_sum_dist_m.dta")
save(lfs_sum_dist_f, file = "Clean data/lfs_sum_dist_f.Rda")
write_dta(lfs_sum_dist_f, "Clean data/lfs_sum_dist_f.dta")
save(lfs_sum_dist, file = "Clean data/lfs_sum_dist.Rda")
write_dta(lfs_sum_dist, "Clean data/lfs_sum_dist.dta")

#################
# SUMMARY STATS #
#################

female_stats <- lfs_all %>%
  filter(age > 19 & age < 65) %>%
  group_by(tinh, huyen) %>%
  summarise(female = mean(female, na.rm = TRUE), .groups = "drop") %>%
  left_join(lfs_sum_dist %>% distinct(tinh, huyen, mean_3G_OCI), by = c("tinh", "huyen"))

female_row <- female_stats %>%
  group_by(mean_3G_OCI) %>%
  summarise(mean = mean(female, na.rm = TRUE),
            sd   = sd(female, na.rm = TRUE),
            min  = min(female, na.rm = TRUE),
            max  = max(female, na.rm = TRUE))

n_ctrl <- lfs_sum_dist %>% filter(mean_3G_OCI == 0) %>% distinct(ID_2) %>% nrow()
n_trt  <- lfs_sum_dist %>% filter(mean_3G_OCI == 1) %>% distinct(ID_2) %>% nrow()

lfs_sum_stats <- lfs_sum_dist %>%
  group_by(mean_3G_OCI) %>%
  summarise(
    lfp_mean     = mean(work[year == 2010],         na.rm = TRUE),
    lfp_sd       = sd(work[year == 2010],           na.rm = TRUE),
    lfp_min      = min(work[year == 2010],          na.rm = TRUE),
    lfp_max      = max(work[year == 2010],          na.rm = TRUE),
    migrant_mean = mean(migrant[year == 2011],      na.rm = TRUE),
    migrant_sd   = sd(migrant[year == 2011],        na.rm = TRUE),
    migrant_min  = min(migrant[year == 2011],       na.rm = TRUE),
    migrant_max  = max(migrant[year == 2011],       na.rm = TRUE),
    hhbus_mean   = mean(hhbus[year == 2010],        na.rm = TRUE),
    hhbus_sd     = sd(hhbus[year == 2010],          na.rm = TRUE),
    hhbus_min    = min(hhbus[year == 2010],         na.rm = TRUE),
    hhbus_max    = max(hhbus[year == 2010],         na.rm = TRUE),
    agri_mean    = mean(agri[year == 2010],         na.rm = TRUE),
    agri_sd      = sd(agri[year == 2010],           na.rm = TRUE),
    agri_min     = min(agri[year == 2010],          na.rm = TRUE),
    agri_max     = max(agri[year == 2010],          na.rm = TRUE),
    manu_mean    = mean(manu[year == 2010],         na.rm = TRUE),
    manu_sd      = sd(manu[year == 2010],           na.rm = TRUE),
    manu_min     = min(manu[year == 2010],          na.rm = TRUE),
    manu_max     = max(manu[year == 2010],          na.rm = TRUE),
    service_mean = mean(service[year == 2010],      na.rm = TRUE),
    service_sd   = sd(service[year == 2010],        na.rm = TRUE),
    service_min  = min(service[year == 2010],       na.rm = TRUE),
    service_max  = max(service[year == 2010],       na.rm = TRUE),
    cov_mean     = mean(ppn_3G_OCI,               na.rm = TRUE),
    cov_sd       = sd(ppn_3G_OCI,                 na.rm = TRUE),
    cov_min      = min(ppn_3G_OCI,                na.rm = TRUE),
    cov_max      = max(ppn_3G_OCI,                na.rm = TRUE),
    cov16_mean   = mean(ppn_3G_OCI[year == 2017], na.rm = TRUE),
    cov16_sd     = sd(ppn_3G_OCI[year == 2017],   na.rm = TRUE),
    cov16_min    = min(ppn_3G_OCI[year == 2017],  na.rm = TRUE),
    cov16_max    = max(ppn_3G_OCI[year == 2017],  na.rm = TRUE)
  )

ctrl <- lfs_sum_stats %>% filter(mean_3G_OCI == 0)
trt  <- lfs_sum_stats %>% filter(mean_3G_OCI == 1)
f_ctrl <- female_row %>% filter(mean_3G_OCI == 0)
f_trt  <- female_row %>% filter(mean_3G_OCI == 1)

# Assemble table
tab <- tibble(
  Variable = c(
    "Female",
    "LFP",
    "Share who migrated in the last 12 months",
    "Household business share",
    "Agriculture share",
    "Manufacturing share",
    "Service share",
    "Share of district with 3G coverage (all years)",
    "Share of district with 3G coverage in 2017"
  ),
  ctrl_mean = round(c(f_ctrl$mean, ctrl$lfp_mean, ctrl$migrant_mean, ctrl$hhbus_mean, ctrl$agri_mean,
                      ctrl$manu_mean, ctrl$service_mean, ctrl$cov_mean, ctrl$cov16_mean), 2),
  ctrl_sd   = round(c(f_ctrl$sd,   ctrl$lfp_sd, ctrl$migrant_sd, ctrl$hhbus_sd,  ctrl$agri_sd,
                      ctrl$manu_sd,  ctrl$service_sd, ctrl$cov_sd,  ctrl$cov16_sd), 2),
  ctrl_min  = round(c(f_ctrl$min,  ctrl$lfp_min, ctrl$migrant_min, ctrl$hhbus_min, ctrl$agri_min,
                      ctrl$manu_min, ctrl$service_min, ctrl$cov_min, ctrl$cov16_min), 2),
  ctrl_max  = round(c(f_ctrl$max,  ctrl$lfp_max, ctrl$migrant_max, ctrl$hhbus_max, ctrl$agri_max,
                      ctrl$manu_max, ctrl$service_max, ctrl$cov_max, ctrl$cov16_max), 2),
  trt_mean  = round(c(f_trt$mean,  trt$lfp_mean, trt$migrant_mean,  trt$hhbus_mean,  trt$agri_mean,
                      trt$manu_mean,  trt$service_mean,  trt$cov_mean,  trt$cov16_mean), 2),
  trt_sd    = round(c(f_trt$sd,    trt$lfp_sd, trt$migrant_sd,  trt$hhbus_sd,   trt$agri_sd,
                      trt$manu_sd,   trt$service_sd,   trt$cov_sd,   trt$cov16_sd), 2),
  trt_min   = round(c(f_trt$min,   trt$lfp_min, trt$migrant_min,  trt$hhbus_min,  trt$agri_min,
                      trt$manu_min,  trt$service_min,  trt$cov_min,  trt$cov16_min), 2),
  trt_max   = round(c(f_trt$max,   trt$lfp_max, trt$migrant_max,  trt$hhbus_max,  trt$agri_max,
                      trt$manu_max,  trt$service_max,  trt$cov_max,  trt$cov16_max), 2)
)

header <- c(1, 4, 4)
names(header) <- c(" ",
                   paste0("Control $N = ", n_ctrl, "$"),
                   paste0("Treated $N = ", n_trt, "$"))

kable(tab,
      format    = "latex",
      booktabs  = TRUE,
      col.names = c("", "Mean", "S.D.", "Min", "Max", "Mean", "S.D.", "Min", "Max"),
      align     = c("l", "c", "c", "c", "c", "c", "c", "c", "c"),
      caption   = "Descriptive Statistics by Treatment Status (initially in 2010)") 
