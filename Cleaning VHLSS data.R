ivid <- c("tinh", "huyen", "xa", "diaban", "hoso", "matv")
hhid <- c("tinh", "huyen", "xa", "diaban", "hoso")

#################
# Labour market #
#################

clean_vhlss_fn <- function(i){
  
  i %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         agri_hh = ifelse(m4ac1b == 1, 1, 0),
         service_hh = ifelse(m4ac1c == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac14 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         yrschool = as.numeric(m2ac1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         agri = ifelse(m4ac4 < 4, 1, 0),
         manu = ifelse(m4ac4 > 9 & m4ac4 < 34, 1, 0),
         service = ifelse(m4ac4 > 43, 1, 0),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac20 < 3, 1, 0),
         agri_informal = ifelse(m4ac8a == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(m4ac8a == 2 & work == 1, 1, 0)) %>% 
    rename(age = m1ac5,
           marst = m1ac6,
           educattain = m2ac2a,
           days = m4ac6,
           days2 = m4ac18,
           hours = m4ac7,
           hours2 = m4ac19,
           occ = m4ac3,
           occ2 = m4ac15,
           ind = m4ac4,
           ind2 = m4ac16,
           org = m4ac8a,
           org2 = m4ac20,
           ethnicity = dantoc)
}

# 2004 
vhlss04 <- list(m123a_04, m4a_04) %>% 
  reduce(merge, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv", "ky")) %>% 
  merge(wt04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac13 == 1, 1, 0),
         inc = m4ac11 + m4ac12e,
         yrschool = as.numeric(m2c1),
         informal = ifelse(m4ac10a == 2 | m4ac10a == 3, 1, 0),
         informal2 = ifelse(m4ac20 == 2 | m4ac20 == 3, 1, 0),
         agri = ifelse(m4ac5 < 6, 1, 0),
         manu = ifelse(m4ac5 > 9 & m4ac5 < 34, 1, 0),
         service = ifelse(m4ac5 > 45, 1, 0),
         agri_informal = ifelse(agri == 1 & informal == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & informal == 1 & work == 1, 1, 0),
         service_informal = ifelse(service ==1 & informal == 1 & work == 1, 1, 0),
         nonagri_informal = ifelse(informal == 1 & work == 1 & m4ac5 > 5, 1, 0),
         year = 2004) %>% 
  rename(age = m1ac5,
         marst = m1ac6,
         educattain = m2c3a,
         days = m4ac7,
         days2 = m4ac17,
         hours = m4ac8,
         hours2 = m4ac18,
         occ = m4ac4,
         occ2 = m4ac14,
         ind = m4ac5,
         ind2 = m4ac15,
         org = m4ac10a,
         org2 = m4ac20,
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) 

save(vhlss04, file = ("Clean data/vhlss04.Rda"))
write_dta(vhlss04, "Clean data/vhlss04.dta")

# 2006 
wt06 <- wt06 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)

vhlss06 <- list(m1a_06, m2a_06, m4a_06) %>% 
  reduce(merge) %>% 
  merge(wt06) %>% 
  mutate(diaban = as.numeric(diaban),
         female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac13 == 1, 1, 0),
         inc = m4ac11 + m4ac12f,
         yrschool = as.numeric(m2ac1),
         informal = ifelse(m4ac10a == 2 | m4ac10a == 3, 1, 0),
         informal2 = ifelse(m4ac20 == 2 | m4ac20 == 3, 1, 0),
         agri = ifelse(m4ac5 < 6, 1, 0),
         manu = ifelse(m4ac5 > 9 & m4ac5 < 34, 1, 0),
         service = ifelse(m4ac5 > 45, 1, 0),
         agri_informal = ifelse(agri == 1 & informal == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & informal == 1 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & informal == 1 & work == 1, 1, 0),
         nonagri_informal = ifelse(informal == 1 & work == 1 & m4ac5 > 5, 1, 0),
         year = 2006) %>% 
  rename(age = m1ac5,
         marst = m1ac6,
         educattain = m2ac3a,
         days = m4ac7,
         days2 = m4ac17,
         hours = m4ac8,
         hours2 = m4ac18,
         occ = m4ac4,
         occ2 = m4ac14,
         ind = m4ac5,
         ind2 = m4ac15,
         org = m4ac10a,
         org2 = m4ac20,
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) 

save(vhlss06, file = ("Clean data/vhlss06.Rda"))
write_dta(vhlss06, "Clean data/vhlss06.dta")

# 2008

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(merge) %>% 
  merge(ho1_08) %>%   
  merge(wt08) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac13 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         yrschool = as.numeric(m2ac1),
         informal = ifelse(m4ac10a == 2 | m4ac10a == 3, 1, 0),
         informal2 = ifelse(m4ac20 == 2 | m4ac20 == 3, 1, 0),
         agri = ifelse(m4ac5 < 4, 1, 0),
         manu = ifelse(m4ac5 > 9 & m4ac5 < 34, 1, 0),
         service = ifelse(m4ac5 > 43, 1, 0),
         agri_informal = ifelse(agri == 1 & informal == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & informal == 1 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & informal == 1 & work == 1, 1, 0),
         nonagri_informal = ifelse(informal == 1 & work == 1 & m4ac5 > 4, 1, 0),
         year = 2008) %>% 
  rename(age = m1ac5,
         marst = m1ac6,
         educattain = m2ac3a,
         days = m4ac7,
         days2 = m4ac17,
         hours = m4ac8,
         hours2 = m4ac18,
         occ = m4ac4,
         occ2 = m4ac14,
         ind = m4ac5,
         ind2 = m4ac15,
         org = m4ac10a,
         org2 = m4ac20,
         ethnicity = dantoc,
         hhwt = wt9) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso)

save(vhlss08, file = ("Clean data/vhlss08.Rda"))
write_dta(vhlss08, "Clean data/vhlss08.dta")

# 2010

vhlss10 <- list(m1a_10, m2a1_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(merge) %>% 
  merge(ho1_10) %>%   
  merge(wt10) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2010,
         hhwt = wt9) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) 

save(vhlss10, file = ("Clean data/vhlss10.Rda"))
write_dta(vhlss10, "Clean data/vhlss10.dta")

# 2012
vhlss12 <- list(m1a_12, m2a_12) %>% 
  reduce(merge) %>% 
  merge(ho1_12) %>% 
  merge(wt12) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2012,
         hhwt = wt9) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) 

save(vhlss12, file = ("Clean data/vhlss12.Rda"))
write_dta(vhlss12, "Clean data/vhlss12.dta")

# 2014

m1a_14 <- m1a_14 %>%
  select(-m1ac6) %>%
  rename(m1ac6 = m1ac8)

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(merge) %>% 
  merge(ho1_14) %>% 
  merge(wt14) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2014,
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) 

save(vhlss14, file = ("Clean data/vhlss14.Rda"))
write_dta(vhlss14, "Clean data/vhlss14.dta")

# 2016

m1a_16 <- m1a_16 %>% rename(matv = m1ama)
m2ab_16 <- m2ab_16 %>% rename(matv = m2ma)
m4a_16 <- m4a_16 %>% rename(matv = m4ama)

vhlss16 <- list(m1a_16, m2ab_16, m4a_16) %>% 
  reduce(merge) %>% 
  merge(ho1_16) %>% 
  merge(wt16) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         internet = ifelse(m1ac16 == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac17 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         yrschool = as.numeric(m2ac1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         agri = ifelse(m4ac4 < 4, 1, 0),
         manu = ifelse(m4ac4 > 9 & m4ac4 < 34, 1, 0),
         service = ifelse(m4ac4 > 43, 1, 0),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac23 < 3, 1, 0),
         agri_informal = ifelse(m4ac8a == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(m4ac8a == 2 & work == 1, 1, 0),
         year = 2016) %>% 
  rename(age = m1ac5,
         marst = m1ac8,
         educattain = m2ac2a,
         days = m4ac6,
         days2 = m4ac21,
         hours = m4ac7,
         hours2 = m4ac22,
         occ = m4ac3,
         occ2 = m4ac18,
         ind = m4ac4,
         ind2 = m4ac19,
         org = m4ac8a,
         org2 = m4ac23,
         ethnicity = dantoc,
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)

save(vhlss16, file = ("Clean data/vhlss16.Rda"))
write_dta(vhlss16, "Clean data/vhlss16.dta")

# 2018

m1a_18 <- m1a_18 %>% rename(matv = m1ama)
m2v_18 <- m2v_18 %>% rename(matv = m2vma)
m4a_18 <- m4a_18 %>% rename(matv = m4ama)

vhlss18 <- list(m1a_18, m2v_18, m4a_18) %>% 
  reduce(merge) %>% 
  merge(ho1_18) %>% 
  merge(wt18) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         internet = ifelse(m1ac16 == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac17 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         yrschool = as.numeric(m2vc1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         urban = ifelse(ttnt == 1, 1, 0),
         agri = ifelse(m4ac4 < 4, 1, 0),
         manu = ifelse(m4ac4 > 9 & m4ac4 < 34, 1, 0),
         service = ifelse(m4ac4 > 43, 1, 0),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac23 < 3, 1, 0),
         agri_informal = ifelse(m4ac8a == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(m4ac8a == 2 & work == 1, 1, 0),
         year = 2018) %>% 
  rename(age = m1ac5,
         marst = m1ac8,
         educattain = m2vc2a,
         days = m4ac6,
         days2 = m4ac21,
         hours = m4ac7,
         hours2 = m4ac22,
         occ = m4ac3,
         occ2 = m4ac18,
         ind = m4ac4,
         ind2 = m4ac19,
         org = m4ac8a,
         org2 = m4ac23,
         ethnicity = dantoc,
         hhwt = wt36) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, service, manu, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt) %>% 
  group_by(tinh, huyen, xa, diaban, hoso)

save(vhlss18, file = ("Clean data/vhlss18.Rda"))
write_dta(vhlss18, "Clean data/vhlss18.dta")

# 2020

wt20 <- wt20 %>% select(tinh, huyen, xa, diaban, ttnt, wt36, wt45) %>% distinct()
m1a_20 <- m1a_20 %>% rename(matv = m1ama)
m2v_20 <- m2v_20 %>% rename(matv = m2vma)
m4a_20 <- m4a_20 %>% rename(matv = m4ama)

vhlss20 <- list(m1a_20, m2v_20, m4a_20) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_20, by = hhid) %>% 
  merge(wt20, by = c("tinh", "huyen", "xa", "diaban", "ttnt")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         internet = ifelse(m1ac15 == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac14 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         yrschool = as.numeric(m2vc1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         urban = ifelse(ttnt == 1, 1, 0),
         agri = ifelse(m4ac4 < 4, 1, 0),
         manu = ifelse(m4ac4 > 9 & m4ac4 < 34, 1, 0),
         service = ifelse(m4ac4 > 43, 1, 0),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac20 < 3, 1, 0),
         agri_informal = ifelse(m4ac8a == 1 & work == 1, 1, 0),
         manu_informal = ifelse(manu == 1 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(service == 1 & m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(m4ac8a == 2 & work == 1, 1, 0),
         year = 2020) %>% 
  rename(age = m1ac5,
         marst = m1ac8,
         educattain = m2vc2a,
         days = m4ac6,
         days2 = m4ac18,
         hours = m4ac7,
         hours2 = m4ac19,
         occ = m4ac3,
         occ2 = m4ac15,
         ind = m4ac4,
         ind2 = m4ac16,
         org = m4ac8a,
         org2 = m4ac20,
         ethnicity = dantoc,
         hhwt = wt36) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri, manu, service, informal, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, informal2, occ2, org2, ind2, days2, hours2, hhwt)

vhlss_all <- bind_rows(vhlss04, vhlss06, vhlss08, vhlss10, vhlss12, vhlss14, vhlss16, vhlss18, vhlss20) %>% 
  group_by(year, tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         children_u7 = sum(age < 7, na.rm = T),
         nchild = sum(age < 18, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(
    work = ifelse(age < 16 & age > 64, NA, work),
    wagework = ifelse(work == 0 | is.na(work), NA, wagework),
    work2 = ifelse(age < 16 & age > 64 | work == 0, NA, work2),
    informal = ifelse(work == 0, NA, informal),
    informal2 = ifelse(work2 == 0, NA, informal2),
    agri = ifelse(work == 0, NA, agri),
    manu = ifelse(work == 0, NA, manu),
    service = ifelse(work == 0, NA, service),
    agri_informal = ifelse(work == 0, NA, agri_informal),
    manu_informal = ifelse(work == 0, NA, manu_informal),
    service_informal = ifelse(work == 0, NA, service_informal)
  )

sum_vhlss_fn <- function(i){
  i %>% 
    summarise(
      n = sum(hhwt, na.rm = T),
      work = sum(hhwt[work == 1], na.rm = T),
      work2 = sum(hhwt[work2 == 1], na.rm = T),
      informal = sum(hhwt[informal == 1], na.rm = T),
      informal2 = sum(hhwt[informal2 == 1], na.rm = T),
      agri = sum(hhwt[agri == 1], na.rm = T),
      manu = sum(hhwt[manu == 1], na.rm = T),
      service = sum(hhwt[service == 1], na.rm = T),
      agri_informal = sum(hhwt[agri_informal == 1], na.rm = T),
      manu_informal = sum(hhwt[manu_informal == 1], na.rm = T),
      service_informal = sum(hhwt[service_informal == 1], na.rm = T),
      nonagri_informal = sum(hhwt[nonagri_informal == 1], na.rm = T)
    )
}

sum_vhlss <- vhlss_all %>% 
  group_by(year) %>% 
  sum_vhlss_fn() %>% 
  mutate(
    lfp = work/n,
    informal_perc = informal/work,
    informal2_perc = informal2/work2,
    agri_perc = agri/work,
    manu_perc = manu/work,
    service_perc = service/work,
    agri_informal_perc = agri_informal/work,
    manu_informal_perc = manu_informal/work,
    service_informal_perc = service_informal/work,
    nonagri_informal_perc = nonagri_informal/work
  )

female_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & age > 19 & age < 65) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() %>% 
  mutate(
    flfp = work/n,
    fwork2_perc = work2/work,
    informal_perc = informal/work,
    informal2_perc = informal2/work2,
    agri_perc = agri/work,
    manu_perc = manu/work,
    service_perc = service/work,
    agri_informal_perc = agri_informal/work,
    manu_informal_perc = manu_informal/work,
    service_informal_perc = service_informal/work,
    nonagri_informal_perc = nonagri_informal/work
  )

male_sum_vhlss <- vhlss_all %>% 
  filter(female == 0 & age > 19 & age < 65) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() %>% 
  mutate(
    mlfp = work/n,
    mwork2_perc = work2/work,
    informal_perc = informal/work,
    informal2_perc = informal2/work2,
    agri_informal_perc = agri_informal/work,
    manu_informal_perc = manu_informal/work,
    service_informal_perc = service_informal/work,
    nonagri_informal_perc = nonagri_informal/work
  )

mothers_u7_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & children_u7 > 0) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() %>% 
  mutate(
    flfp = work/n,
    fwork2_perc = work2/work,
    informal_perc = informal/work,
    informal2_perc = informal2/work2,
    agri_informal_perc = agri_informal/work,
    manu_informal_perc = manu_informal/work,
    service_informal_perc = service_informal/work,
    nonagri_informal_perc = nonagri_informal/work
  )

mothers_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & nchild > 0) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() %>% 
  mutate(
    flfp = work/n,
    fwork2_perc = work2/work,
    informal_perc = informal/work,
    informal2_perc = informal2/work2,
    agri_informal_perc = agri_informal/work,
    manu_informal_perc = manu_informal/work,
    service_informal_perc = service_informal/work,
    nonagri_informal_perc = nonagri_informal/work
  )

##########
# Assets #
##########

devices04 <- m6b_04 %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 34]),
            landline = sum(m6bc3[m6bma == 33]),
            computer = sum(m6bc3[m6bma == 46]), .groups = "drop") %>% 
  merge(wt04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2004)

devices06 <- m6b_06 %>% 
  mutate(diaban = as.numeric(diaban)) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 34]),
            landline = sum(m6bc3[m6bma == 33]),
            computer = sum(m6bc3[m6bma == 46]), .groups = "drop") %>% 
  merge(wt06) %>% 
  distinct() %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2006)

devices08 <- m6b_08 %>% 
  mutate(diaban = as.numeric(diaban)) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 34]),
            landline = sum(m6bc3[m6bma == 33]),
            computer = sum(m6bc3[m6bma == 46]), .groups = "drop") %>% 
  merge(wt08) %>%
  rename(hhwt = wt9) %>% 
  mutate(year = 2008)

devices10 <- m6b_10 %>% 
  mutate(diaban = as.numeric(diaban)) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6c3[m6c2 == 12]),
            landline = sum(m6c3[m6c2 == 11]),
            computer = sum(m6c3[m6c2 == 20]), .groups = "drop") %>% 
  merge(wt10) %>% 
  rename(hhwt = wt9) %>% 
  mutate(year = 2010)

devices12 <- m6b_12 %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6c3[m6c2 == 12]),
            landline = sum(m6c3[m6c2 == 11]),
            computer = sum(m6c3[m6c2 == 20]), .groups = "drop") %>% 
  merge(wt12) %>% 
  rename(hhwt = wt9) %>% 
  mutate(year = 2012)

devices14 <- m6b_14 %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 12]),
            landline = sum(m6bc3[m6bma == 11]),
            computer = sum(m6bc3[m6bma == 20]), .groups = "drop") %>% 
  merge(wt14) %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2014)

devices16 <- m6b_16 %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 12]),
            landline = sum(m6bc3[m6bma == 11]),
            computer = sum(m6bc3[m6bma == 20]), .groups = "drop") %>% 
  merge(wt16) %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2016)

devices18 <- m6b_18 %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 12]),
            landline = sum(m6bc3[m6bma == 11]),
            computer = sum(m6bc3[m6bma == 20]), .groups = "drop") %>% 
  merge(wt18) %>% 
  distinct() %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2018)

devices20 <- m6b_20 %>% 
  mutate(m6bma = as.numeric(m6bma),
         m6bc3 = as.numeric(m6bc3)) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(mob = sum(m6bc3[m6bma == 12]),
            landline = sum(m6bc3[m6bma == 11]),
            computer = sum(m6bc3[m6bma == 20]), .groups = "drop") %>% 
  merge(wt20) %>% 
  distinct() %>% 
  rename(hhwt = wt45) %>% 
  mutate(year = 2020)

devices_sum <- bind_rows(devices04, devices06, devices08, devices10, devices12, devices14, devices16, devices18, devices20) %>% 
  group_by(year) %>% 
  summarise(
    n = sum(hhwt, na.rm = T),
    mob = sum(hhwt[mob > 0], na.rm = T),
    landline = sum(hhwt[landline > 0], na.rm = T),
    computer = sum(hhwt[computer > 0], na.rm = T)
  ) %>% 
  mutate(
    mob_perc = mob/n,
    landline_perc = landline/n,
    computer_perc = computer/n
  )
