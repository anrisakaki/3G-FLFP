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
         org2 = ifelse(m4ac20 == 1, 1, 0)) %>% 
    rename(age = m1ac5,
           marst = m1ac6,
           yrschool = m2ac1,
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
           ethnicity = dantoc)
}

# 2010

wt10 <- wt10 %>% select(-ttnt)

vhlss10 <- list(m1a_10, m2a1_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_10, by = hhid) %>%   
  merge(wt10, by = c("tinh", "huyen", "xa", "diaban", "hoso")) %>% 
  clean_vhlss_fn() %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri_hh, service_hh, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, wt9)

# 2012
wt12 <- wt12 %>% select(-ttnt)

vhlss12 <- list(m1a_12, m2a_12) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_12, by = hhid) %>% 
  merge(wt12, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         agri_hh = ifelse(m4ac1b == 1, 1, 0),
         service_hh = ifelse(m4ac1c == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac14 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0),
         org2 = ifelse(m4ac20 == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         marst = m1ac6,
         yrschool = m2ac1,
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
         ethnicity = dantoc) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, educattain, urban, ethnicity,
         work, wagework, agri_hh, service_hh, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, 
         wt9, wt36, wt45)
save(vhlss12, file = ("Clean data/vhlss12.Rda"))
write_dta(vhlss12, "Clean data/vhlss12.dta")

# 2014

wt14 <- wt14 %>% select(-ttnt)

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_14, by = hhid) %>% 
  merge(wt14, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         agri_hh = ifelse(m4ac1b == 1, 1, 0),
         service_hh = ifelse(m4ac1c == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac14 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         marst = m1ac8,
         yrschool = m2ac1,
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
         ethnicity = dantoc) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, urban, ethnicity,
         work, wagework, agri_hh, service_hh, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, wt45)

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
         agri_hh = ifelse(m4ac1b == 1, 1, 0),
         service_hh = ifelse(m4ac1c == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         work2 = ifelse(m4ac17 == 1, 1, 0),
         inc = m4ac11 + m4ac12a + m4ac12b,
         urban = ifelse(ttnt == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         marst = m1ac8,
         yrschool = m2ac1,
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
         ethnicity = dantoc) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, female, age, marst, yrschool, urban, ethnicity, internet,
         work, wagework, agri_hh, service_hh, occ, org, ind, days, hours, inc, work2, occ2, org2, ind2, days2, hours2, wt45)  
