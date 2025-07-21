# HHID

hhid1012 <- ho1_12 %>% 
  filter(ghepho == 1) %>% 
  select(tinh, huyen, xa, diaban, hoso, tinh2010, huyen2010, xa2010, diaban2010, hoso2010) %>% 
  filter(if_all(c(tinh2010, huyen2010, xa2010, diaban2010, hoso2010), ~ !is.na(.))) %>% 
  rename(
    tinh10 = tinh2010,
    huyen10 = huyen2010,
    xa10 = xa2010,
    diaban10 = diaban2010,
    hoso10 = hoso2010,
    tinh12 = tinh,
    xa12 = xa,
    diaban12 = diaban,
    hoso12 = hoso
  )

hhid1214 <- ho1_14 %>% 
  select(tinh, huyen, xa, diaban, hoso, tinh12, huyen12, xa12, diaban12, hoso12) %>% 
  filter(if_all(c(tinh12, huyen12, xa12, diaban12, hoso12), ~ !is.na(.))) %>% 
  rename(
    tinh14 = tinh,
    huyen14 = huyen,
    xa14 = xa,
    diaban14 = diaban,
    hoso14 = hoso
  )

hhid1416 <- ho1_16 %>% 
  select(tinh, huyen, xa, diaban, hoso, tinh14, huyen14, xa14, diaban14, hoso14) %>% 
  filter(if_all(c(tinh14, huyen14, xa14, diaban14, hoso14), ~ !is.na(.))) %>% 
  rename(
    tinh16 = tinh,
    huyen16 = huyen,
    xa16 = xa,
    diaban16 = diaban,
    hoso16 = hoso
  )

hhid1618 <- ho1_18 %>% 
  select(tinh, huyen, xa, diaban, hoso, tinh16, huyen16, xa16, diaban16, hoso16) %>% 
  filter(if_all(c(tinh16, huyen16, xa16, diaban16, hoso16), ~ !is.na(.))) %>% 
  rename(
    tinh18 = tinh,
    huyen18 = huyen,
    xa18 = xa,
    diaban18 = diaban,
    hoso18 = hoso
  )

hhid1014 <- list(hhid1012, hhid1214) %>% 
  reduce(merge)

# IVID
ivid14 <- m1c_14 %>%
  select(tinh, huyen, xa, diaban, hoso, matv, m1cc3) %>% 
  rename(tinh14 = tinh,
         huyen14 = huyen,
         xa14 = xa,
         diaban14 = diaban,
         hoso14 = hoso,
         matv14 = matv,
         matv12 = m1cc3)
ivid12 <- m1c_12 %>% 
  select(tinh, huyen, xa, diaban, hoso, m1cc7, m1cc3) %>% 
  rename(tinh12 = tinh,
         huyen12 = huyen,
         xa12 = xa,
         diaban12 = diaban,
         hoso12 = hoso,
         matv12 = m1cc7,
         matv10 = m1cc3)

ivid1014 <- list(hhid1014, ivid12, ivid14) %>% 
  reduce(merge) %>% 
  mutate(ivid = row_number())

ivid10 <- ivid1014 %>% 
  select(ends_with("10"), ivid) %>% 
  rename(tinh = tinh10,
         huyen = huyen10,
         xa = xa10,
         diaban = diaban10,
         hoso = hoso10,
         matv = matv10) %>% 
  mutate(year = 2010)

ivid12 <- ivid1014 %>% 
  select(ends_with("12"), ivid) %>% 
  rename(tinh = tinh12,
         huyen = huyen12,
         xa = xa12,
         diaban = diaban12,
         hoso = hoso12,
         matv = matv12) %>% 
  mutate(year = 2012)

ivid14 <- ivid1014 %>% 
  select(ends_with("14"), ivid) %>% 
  rename(tinh = tinh14,
         huyen = huyen14,
         xa = xa14,
         diaban = diaban14,
         hoso = hoso14,
         matv = matv14) %>% 
  mutate(year = 2014)

ivid1014 <- bind_rows(ivid10, ivid12, ivid14) 

save(ivid1014, file = ("Clean data/vhlss1014_iv_panel.Rda"))

vhlss1014 <- vhlss_all_dist %>% 
  filter(year > 2009 & year < 2015) %>% 
  merge(ivid1014)
