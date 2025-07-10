# Set up

hhid0810 <- ho1_10

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

hhid1418 <- list(hhid1416, hhid1416, hhid1618) %>% 
  reduce(merge)

vhlss1014 <- bind_rows(vhlss10, vhlss12, vhlss14)
vhlss1014 <- merge(vhlss1014, panel1014)
save(vhlss1014, file = ("Clean data/vhlss1014_panel.Rda"))
write_dta(vhlss1014, "Clean data/vhlss1014_panel.dta")
