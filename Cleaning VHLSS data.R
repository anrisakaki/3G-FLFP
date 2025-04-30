ivid <- c("tinh", "huyen", "xa", "diaban", "hoso", "matv")
hhid <- c("tinh", "huyen", "xa", "diaban", "hoso")

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
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac20 < 3, 1, 0),
         informal = ifelse(work == 0, NA, informal),
         informal2 = ifelse(work2 == 0, NA, informal2),
         agri_informal = ifelse(m4ac8a == 1 & work == 1, 1, 0),
         agri_informal = ifelse(work == 0, NA, agri_informal),
         manu_informal = ifelse(m4ac4 > 9 & m4ac4 < 34 & m4ac8a == 2 & work == 1, 1, 0),
         manu_informal = ifelse(work == 0, NA, manu_informal),
         service_informal = ifelse(m4ac4 > 43 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(work == 0, NA, service_informal),
         nonagri_informal = ifelse(m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(work == 0, NA, nonagri_informal)) %>% 
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

# 2008

wt08 <- wt08 %>% select(tinh, huyen, xa, diaban, wt9)

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_08, by = hhid) %>%   
  merge(wt08, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac13 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         yrschool = as.numeric(m2ac1),
         informal = ifelse(m4ac10a == 2 | m4ac10a == 3, 1, 0),
         informal2 = ifelse(m4ac20 == 2 | m4ac20 == 3, 1, 0),
         informal = ifelse(work == 0, NA, informal),
         informal2 = ifelse(work2 == 0, NA, informal2),
         agri_informal = ifelse(m4ac5 < 4 & informal == 1 & work == 1, 1, 0),
         agri_informal = ifelse(work == 0, NA, agri_informal),
         manu_informal = ifelse(m4ac5 > 9 & m4ac5 < 34 & informal == 1 & work == 1, 1, 0),
         manu_informal = ifelse(work == 0, NA, manu_informal),
         service_informal = ifelse(m4ac5 > 43 & informal == 1 & work == 1, 1, 0),
         service_informal = ifelse(work == 0, NA, service_informal),
         nonagri_informal = ifelse(informal == 1 & work == 1 & m4ac5 > 4, 1, 0),
         nonagri_informal = ifelse(work == 0, NA, nonagri_informal),
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
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss08, file = ("Clean data/vhlss08.Rda"))
write_dta(vhlss08, "Clean data/vhlss08.dta")

# 2010

wt10 <- wt10 %>% select(-ttnt)

vhlss10 <- list(m1a_10, m2a1_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_10, by = hhid) %>%   
  merge(wt10, by = c("tinh", "huyen", "xa", "diaban", "hoso")) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2010,
         hhwt = wt9) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss10, file = ("Clean data/vhlss10.Rda"))
write_dta(vhlss10, "Clean data/vhlss10.dta")

# 2012
wt12 <- wt12 %>% select(-ttnt)

vhlss12 <- list(m1a_12, m2a_12) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_12, by = hhid) %>% 
  merge(wt12, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2012,
         hhwt = wt9) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss12, file = ("Clean data/vhlss12.Rda"))
write_dta(vhlss12, "Clean data/vhlss12.dta")

# 2014

wt14 <- wt14 %>% select(-ttnt)
m1a_14 <- m1a_14 %>%
  select(-m1ac6) %>%
  rename(m1ac6 = m1ac8)

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_14, by = hhid) %>% 
  merge(wt14, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  clean_vhlss_fn() %>% 
  mutate(year = 2014,
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss14, file = ("Clean data/vhlss14.Rda"))
write_dta(vhlss14, "Clean data/vhlss14.dta")

# 2016

wt16 <- wt16 %>% select(-ttnt)
m1a_16 <- m1a_16 %>% rename(matv = m1ama)
m2ab_16 <- m2ab_16 %>% rename(matv = m2ma)
m4a_16 <- m4a_16 %>% rename(matv = m4ama)

vhlss16 <- list(m1a_16, m2ab_16, m4a_16) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_16, by = hhid) %>% 
  merge(wt16, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         internet = ifelse(m1ac16 == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac17 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         yrschool = as.numeric(m2ac1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac23 < 3, 1, 0),
         informal = ifelse(work == 0, NA, informal),
         informal2 = ifelse(work2 == 0, NA, informal2),
         agri_informal = ifelse(m4ac8a == 1 & m4ac4 < 4 & work == 1, 1, 0),
         agri_informal = ifelse(work == 0, NA, agri_informal),
         manu_informal = ifelse(m4ac4 > 9 & m4ac4 < 34 & m4ac8a == 2 & work == 1, 1, 0),
         manu_informal = ifelse(work == 0, NA, manu_informal),
         service_informal = ifelse(m4ac4 > 43 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(work == 0, NA, service_informal),
         nonagri_informal = ifelse(m4ac8a == 2 & m4ac4 > 3 & work == 1, 1, 0),
         nonagri_informal = ifelse(work == 0, NA, nonagri_informal),
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
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss16, file = ("Clean data/vhlss16.Rda"))
write_dta(vhlss16, "Clean data/vhlss16.dta")

# 2018

wt18 <- wt18 %>% select(-ttnt)

m1a_18 <- m1a_18 %>% rename(matv = m1ama)
m2v_18 <- m2v_18 %>% rename(matv = m2vma)
m4a_18 <- m4a_18 %>% rename(matv = m4ama)

vhlss18 <- list(m1a_18, m2v_18, m4a_18) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_18, by = hhid) %>% 
  merge(wt18, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         internet = ifelse(m1ac16 == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         agri_hh = ifelse(m4ac1b == 1, 1, 0),
         service_hh = ifelse(m4ac1c == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac17 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         yrschool = as.numeric(m2vc1),
         m4ac4 = ifelse(m4ac4 > 99, 1, m4ac4),
         urban = ifelse(ttnt == 1, 1, 0),
         informal = ifelse(m4ac8a < 3, 1, 0),
         informal2 = ifelse(m4ac23 < 3, 1, 0),
         informal = ifelse(work == 0, NA, informal),
         informal2 = ifelse(work2 == 0, NA, informal2),
         agri_informal = ifelse(m4ac8a == 1 & m4ac4 < 4 & work == 1, 1, 0),
         agri_informal = ifelse(work == 0, NA, agri_informal),
         manu_informal = ifelse(m4ac4 > 9 & m4ac4 < 34 & m4ac8a == 2 & work == 1, 1, 0),
         manu_informal = ifelse(work == 0, NA, manu_informal),
         service_informal = ifelse(m4ac4 > 43 & m4ac8a == 2 & work == 1, 1, 0),
         service_informal = ifelse(work == 0, NA, service_informal),
         nonagri_informal = ifelse(m4ac4 > 3 & m4ac8a == 2 & work == 1, 1, 0),
         nonagri_informal = ifelse(work == 0, NA, nonagri_informal),
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
         hhwt = wt45) %>% 
  select(year, tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, informal, informal2, agri_informal, nonagri_informal, manu_informal, service_informal, 
         occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, hhwt)
save(vhlss18, file = ("Clean data/vhlss18.Rda"))
write_dta(vhlss18, "Clean data/vhlss18.dta")

vhlss_all <- bind_rows(vhlss08, vhlss10, vhlss12, vhlss14, vhlss16, vhlss18) %>% 
  mutate(
    age_group = case_when(
      age <= 9 ~ "0-9",
      age <= 19 ~ "10-19",
      age <= 29 ~ "20-29",
      age <= 39 ~ "30-39",
      age <= 49 ~ "40-49",
      age <= 59 ~ "50-59",
      age <= 64 ~ "60-64",
      TRUE ~ "65+" 
    )
  ) %>%
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(
    work = ifelse(age < 16 & age > 64, NA, work),
    wagework = ifelse(work == 0 | is.na(work), NA, wagework),
    work2 = ifelse(age < 16 & age > 64 | work == 0, NA, work2),
  )

sum_vhlss_fn <- function(i){
  i %>% 
    summarise(
      n = sum(hhwt, na.rm = T),
      work = sum(hhwt[work == 1], na.rm = T),
      work2 = sum(hhwt[work2 == 1], na.rm = T),
      informal = sum(hhwt[informal == 1], na.rm = T),
      informal2 = sum(hhwt[informal2 == 1], na.rm = T),
      agri_informal = sum(hhwt[agri_informal == 1], na.rm = T),
      manu_informal = sum(hhwt[manu_informal == 1], na.rm = T),
      service_informal = sum(hhwt[service_informal == 1], na.rm = T),
      nonagri_informal = sum(hhwt[nonagri_informal == 1], na.rm = T)
    )
}

fsum_age_vhlss <- vhlss_all %>% 
  group_by(year, age_group) %>% 
  filter(age > 19 & age < 64 & female == 1) %>% 
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

female_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & age > 19 & age < 65) %>% 
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
