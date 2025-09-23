# Consistent district codes 

dist18 <- dist9919 %>%
  dplyr::select(prov2018, dist2018, provname2018, distname2018) %>%
  distinct() %>% 
  rename(NAME_1 = provname2018) %>% 
  mutate(NAME_1 = str_remove(NAME_1, "^Tỉnh\\s+"),
         NAME_1 = str_remove(NAME_1, "^Thành phố\\s+"))

dist0418 <- dist9919 %>% 
  dplyr::select(prov2004, dist2004, prov2018, dist2018) %>% 
  rename(tinh = prov2004,
         huyen = dist2004) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist0618 <- dist9919 %>% 
  dplyr::select(prov2004, dist2004, prov2006, dist2006, prov2018, dist2018) %>% 
  rename(tinh = prov2006,
         huyen = dist2006) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist0818 <- dist9919 %>% 
  dplyr::select(prov2008, dist2008, prov2018, dist2018) %>% 
  rename(tinh = prov2008,
         huyen = dist2008) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist1018 <- dist9919 %>% 
  dplyr::select(prov2010, dist2010, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2010,
         huyen = dist2010,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

dist1218 <- dist9919 %>% 
  dplyr::select(prov2012, dist2012, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2012,
         huyen = dist2012,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

dist1418 <- dist9919 %>% 
  dplyr::select(prov2014, dist2014, prov2018, dist2018, provname2018, distname2018) %>% 
  rename(tinh = prov2014,
         huyen = dist2014,
         NAME_1 = provname2018,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(NAME_1 = str_remove(NAME_1, "^Tỉnh\\s+"),
         NAME_1 = str_remove(NAME_1, "^Thành phố\\s+"))

dist1618 <- dist9919 %>% 
  dplyr::select(prov2016, dist2016, prov2018, dist2018, provname2018, distname2018) %>% 
  rename(tinh = prov2016,
         huyen = dist2016,
         NAME_1 = provname2018,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(NAME_1 = str_remove(NAME_1, "^Tỉnh\\s+"),
         NAME_1 = str_remove(NAME_1, "^Thành phố\\s+"))

vnmap2 <- st_transform(vnmap2, st_crs(cb13_int)) %>% 
  mutate(
    NAME_2 = case_when(
      NAME_2 == 'Cao Lãnh (Thành phố)' ~ 'Cao Lãnh',
      NAME_2 == 'Hồng Ngự (Thị xã)' ~ 'Hồng Ngự',
      NAME_2 == 'Long Mỹ (Thị xã)' ~ 'Long Mỹ',
      NAME_2 == 'Kỳ Anh (Thị xã)' ~ 'Kỳ Anh',
      NAME_2 == 'Cai Lậy (Thị xã)' ~ 'Cai Lậy',
      NAME_2 == 'Duyên Hải (Thị xã)' ~ 'Duyên Hải',
      TRUE ~ NAME_2),  
    NAME_2 = if_else(
      str_detect(NAME_2, "Thành Phố|Quận"),
      NAME_2,
      paste(TYPE_2, NAME_2, sep = " ")
    )
  )

vnmap2$dist_area <- as.numeric(st_area(vnmap2))
dist_area <- vnmap2 %>% st_drop_geometry() %>% dplyr::select(GID_2, NAME_1, NAME_2, dist_area)

cb13_int$area_m2 <- as.numeric(st_area(cb13_int))
cb14_int$area_m2 <- as.numeric(st_area(cb14_int)) 
cb15_int$area_m2 <- as.numeric(st_area(cb15_int)) 
cb16_int$area_m2 <- as.numeric(st_area(cb16_int)) 
cb17_int$area_m2 <- as.numeric(st_area(cb17_int)) 
cb18_int$area_m2 <- as.numeric(st_area(cb18_int)) 
cb19_int$area_m2 <- as.numeric(st_area(cb19_int)) 
cb20_int$area_m2 <- as.numeric(st_area(cb20_int)) 

cb_dist_cov_fn <- function(i) {
  i %>% 
    st_drop_geometry() %>% 
    group_by(GID_2) %>% 
    summarise(dist_cov = sum(area_m2), .groups = "drop") %>% 
    left_join(dist_area) %>% 
    mutate(dist_cov_share = dist_cov/dist_area,
           dist_cov_round = round(dist_cov_share, 2))
}

cb13_cov_dist <- cb13_int %>% cb_dist_cov_fn() %>% mutate(year = 2013)
cb14_cov_dist <- cb14_int %>% cb_dist_cov_fn() %>% left_join(dist1418) %>% mutate(year = 2014)
cb15_cov_dist <- cb15_int %>% cb_dist_cov_fn() %>% mutate(year = 2015)
cb16_cov_dist <- cb16_int %>% cb_dist_cov_fn() %>% left_join(dist1618) %>% mutate(year = 2016)
cb17_cov_dist <- cb17_int %>% cb_dist_cov_fn() %>% mutate(year = 2017)
cb18_cov_dist <- cb18_int %>% cb_dist_cov_fn() %>% left_join(dist18, by = c("NAME_2" = "distname2018", "NAME_1")) %>% mutate(year = 2018, tinh = prov2018, huyen = dist2018) 
cb19_cov_dist <- cb19_int %>% cb_dist_cov_fn() %>% mutate(year = 2019)
cb20_cov_dist <- cb20_int %>% cb_dist_cov_fn() %>% left_join(dist18, by = c("NAME_2" = "distname2018", "NAME_1")) %>% mutate(year = 2020, tinh = prov2018, huyen = dist2018)

cb_3G_1320 <- bind_rows(cb13_cov_dist, cb14_cov_dist, cb15_cov_dist, cb16_cov_dist, 
                        cb17_cov_dist, cb18_cov_dist, cb19_cov_dist, cb20_cov_dist) %>% 
  dplyr::select(-c(prov2018, dist2018, tinh, huyen, dist_cov, dist_area)) 

cb_3G_vhlss <- bind_rows(cb14_cov_dist, cb16_cov_dist, cb18_cov_dist, cb20_cov_dist) %>% 
  group_by(GID_2) %>% 
  mutate(distid = cur_group_id()) %>% 
  ungroup() %>% 
  dplyr::select(-c(prov2018, dist2018, GID_2, dist_cov, dist_area)) 

save(cb_3G_1320, file = "Clean data/cb_3G_1320.Rda")
write_dta(cb_3G_1320, "Clean data/cb_3G_1320.dta")
save(cb_3G_vhlss, file = "Clean data/cb_3G_vhlss.Rda")
write_dta(cb_3G_vhlss, "Clean data/cb_3G_vhlss.dta")

# Province 
vnmap1 <- st_transform(vnmap1, st_crs(cb13_int_prov))
vnmap1$prov_area <- as.numeric(st_area(vnmap1))
prov_area <- vnmap1 %>% st_drop_geometry() %>% dplyr::select(GID_1, NAME_1, prov_area)

cb13_int_prov$area_m2 <- as.numeric(st_area(cb13_int_prov))
cb14_int_prov$area_m2 <- as.numeric(st_area(cb14_int_prov)) 
cb15_int_prov$area_m2 <- as.numeric(st_area(cb15_int_prov)) 
cb16_int_prov$area_m2 <- as.numeric(st_area(cb16_int_prov)) 
cb17_int_prov$area_m2 <- as.numeric(st_area(cb17_int_prov)) 
cb18_int_prov$area_m2 <- as.numeric(st_area(cb18_int_prov)) 
cb19_int_prov$area_m2 <- as.numeric(st_area(cb19_int_prov)) 
cb20_int_prov$area_m2 <- as.numeric(st_area(cb20_int_prov)) 

cb_prov_cov_fn <- function(i) {
  i %>% 
    st_drop_geometry() %>% 
    group_by(GID_1) %>% 
    summarise(prov_cov = sum(area_m2), .groups = "drop") %>% 
    left_join(prov_area) %>% 
    mutate(prov_cov_share = prov_cov/prov_area,
           prov_cov_round = round(prov_cov_share, 2))
}

cb13_cov_prov <- cb13_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2013)
cb14_cov_prov <- cb14_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2014)
cb15_cov_prov <- cb15_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2015)
cb16_cov_prov <- cb16_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2016)
cb17_cov_prov <- cb17_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2017)
cb18_cov_prov <- cb18_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2018)
cb19_cov_prov <- cb19_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2019)
cb20_cov_prov <- cb20_int_prov %>% cb_prov_cov_fn() %>% mutate(year = 2020)

cb_3G_prov_1320 <- bind_rows(cb13_cov_prov, cb14_cov_prov, cb15_cov_prov, cb16_cov_prov,
                             cb17_cov_prov, cb18_cov_prov, cb19_cov_prov, cb20_cov_prov) %>% 
  dplyr::select(-c(GID_1, prov_cov, prov_area)) %>% 
  mutate(tinh = recode(NAME_1,
                       'An Giang' = 89,
                       'Bà Rịa - Vũng Tàu' = 77,
                       'Bắc Giang' = 24,
                       'Bắc Kạn' = 6,
                       'Bạc Liêu' = 95,
                       'Bắc Ninh' = 27,
                       'Bến Tre' = 83,
                       'Bình Định' = 52,
                       'Bình Dương' = 74,
                       'Bình Phước' = 70,
                       'Bình Thuận' = 60,
                       'Cà Mau' = 96,
                       'Cần Thơ' = 92,
                       'Cao Bằng' = 4,
                       'Đà Nẵng' = 48,
                       'Đắk Lắk' = 66,
                       'Đắk Nông' = 67,
                       'Điện Biên' = 11,
                       'Đồng Nai' = 75,
                       'Đồng Tháp' = 87,
                       'Gia Lai' = 64,
                       'Hà Giang' = 2,
                       'Hà Nam' = 35,
                       'Hà Nội' = 1,
                       'Hà Tĩnh' = 42,
                       'Hải Dương' = 30,
                       'Hải Phòng' = 31,
                       'Hậu Giang' = 93,
                       'Hồ Chí Minh' = 79,
                       'Hoà Bình' = 17,
                       'Hưng Yên' = 33,
                       'Khánh Hòa' = 56,
                       'Kiên Giang' = 91,
                       'Kon Tum' = 62,
                       'Lai Châu' = 12,
                       'Lâm Đồng' = 68,
                       'Lạng Sơn' = 20,
                       'Lào Cai' = 10,
                       'Long An' = 80,
                       'Nam Định' = 36,
                       'Nghệ An' = 40,
                       'Ninh Bình' = 37,
                       'Ninh Thuận' = 58,
                       'Phú Thọ' = 25,
                       'Phú Yên' = 54,
                       'Quảng Bình' = 44,
                       'Quảng Nam' = 49,
                       'Quảng Ngãi' = 51,
                       'Quảng Ninh' = 22,
                       'Quảng Trị' = 45,
                       'Sóc Trăng' = 94,
                       'Sơn La' = 14,
                       'Tây Ninh' = 72,
                       'Thái Bình' = 34,
                       'Thái Nguyên' = 19,
                       'Thanh Hóa' = 38,
                       'Thừa Thiên Huế' = 46,
                       'Tiền Giang' = 82,
                       'Trà Vinh' = 84,
                       'Tuyên Quang' = 8,
                       'Vĩnh Long' = 86,
                       'Vĩnh Phúc' = 26,
                       'Yên Bái' = 15,
                       .default = NA_real_)) %>% 
  dplyr::select(-NAME_1) %>% 
  dplyr::select(tinh, everything())

save(cb_3G_prov_1320, file = "Clean data/cb_3G_prov_1320.Rda")
write_dta(cb_3G_prov_1320, "Clean data/cb_3G_prov_1320.dta")

# Maps 

ggplot() +
  geom_sf(data = cb13, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "255" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov13.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb14, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov14.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb15, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov15.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb16, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov16.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb17, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov17.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb18, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov18.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb19, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov19.jpeg", width = 7, height = 7)

ggplot() +
  geom_sf(data = cb20, aes(fill = as.factor(DN)), color = NA) +  
  geom_sf(data = vnmap2, fill = NA, color = "darkgrey", size = 10) + 
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 20) + 
  scale_fill_manual(values = c("0" = "white", "1" = "#CDB4DB")) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("Figures/3G_cov20.jpeg", width = 7, height = 7)
