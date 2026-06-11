library(haven)
library(tidyverse)

phc09 <- read_dta(file = "Raw data/phc09.dta")

# Province level controls from 2009 Population and Housing Census

# Share of employed workers in manufacturing and IT
# IT: ind codes 600-639 (ISIC divisions 60-63: broadcasting, telecoms, computer programming, information services)
prov_emp_09 <- phc09 %>%
  filter(empstat == 1) %>%
  mutate(
    manu = as.numeric(indgen == 30),
    it   = as.numeric(ind > 599 & ind < 640)
  ) %>%
  group_by(geo1_vn2009) %>%
  summarise(
    sh_prov_manu_09 = mean(manu, na.rm = TRUE),
    sh_prov_it_09   = mean(it,   na.rm = TRUE)
  )

prov_edu_09 <- phc09 %>%
  filter(edattain %in% 1:4) %>%
  mutate(hs_above = as.numeric(edattain >= 3)) %>%
  group_by(geo1_vn2009) %>%
  summarise(sh_prov_hs_09 = mean(hs_above, na.rm = TRUE))

prov_fdi_09 <- phc09 %>%
  filter(empstat == 1) %>%
  mutate(fdi = as.numeric(empsect == 23)) %>%
  group_by(geo1_vn2009) %>%
  summarise(sh_prov_fdi_09 = mean(fdi, na.rm = TRUE))

province_controls_09 <- left_join(prov_emp_09, prov_edu_09, by = "geo1_vn2009") %>%
  left_join(prov_fdi_09,  by = "geo1_vn2009") %>%
  rename(tinh = geo1_vn2009)

# District level controls from 2009 Population and Housing Census

dist_emp_09 <- phc09 %>%
  filter(empstat == 1) %>%
  mutate(manu = as.numeric(indgen == 30)) %>%
  group_by(geo2_vn2009) %>%
  summarise(sh_manu_09 = weighted.mean(manu, perwt, na.rm = TRUE))

dist_edu_09 <- phc09 %>%
  filter(edattain %in% 1:4) %>%
  mutate(hs_above = as.numeric(edattain >= 3)) %>%
  group_by(geo2_vn2009) %>%
  summarise(sh_hs_09 = weighted.mean(hs_above, perwt, na.rm = TRUE))

dist_it_09 <- phc09 %>%
  filter(empstat == 1) %>%
  mutate(it = as.numeric(ind > 599 & ind < 640)) %>%
  group_by(geo2_vn2009) %>%
  summarise(sh_it_09 = weighted.mean(it, perwt, na.rm = TRUE))

dist_fdi_09 <- phc09 %>%
  filter(empstat == 1) %>%
  mutate(fdi = as.numeric(empsect == 23)) %>%
  group_by(geo2_vn2009) %>%
  summarise(sh_fdi_09 = weighted.mean(fdi, perwt, na.rm = TRUE))

dist_mig_09 <- phc09 %>%
  mutate(migrate = as.numeric(migrate5 == 12)) %>%
  group_by(geo2_vn2009) %>%
  summarise(sh_migrant_09 = weighted.mean(migrate, perwt, na.rm = TRUE))

# tinh and huyen decomposed from geo2_vn2009 to match LFS district identifiers
district_controls_09 <- list(dist_emp_09, dist_edu_09, dist_it_09, dist_fdi_09, dist_mig_09) %>% 
  reduce(full_join) %>% 
  mutate(
    tinh  = floor(as.numeric(geo2_vn2009) / 1000),
    huyen = as.numeric(geo2_vn2009) %% 1000
  ) %>% 
  merge(province_controls_09) %>% 
  select(-geo2_vn2009) %>% 
  select(tinh, huyen, everything())

save(district_controls_09, file = "Clean data/district_controls_09.Rda")
write_dta(district_controls_09, "Clean data/district_controls_09.dta")
