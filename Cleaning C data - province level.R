##############################
# PROVINCE-LEVEL 3G COVERAGE #
##############################

# Collins Bartholomew 

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

# OpenCelliD Data

opencell$created_recode <- as.POSIXct(opencell$created, origin = "1970-01-01", tz = "UTC")
opencell$year_created <- year(opencell$created_recode)
opencell$month_created <- month(opencell$created_recode)
opencell$day_created <- day(opencell$created_recode)

opencell$lastseen_recode <- as.POSIXct(opencell$updated, origin = "1970-01-01", tz = "UTC")
opencell$year_lastseen <- year(opencell$lastseen_recode)
opencell$month_lastseen <- month(opencell$lastseen_recode)
opencell$day_lastseen <- day(opencell$lastseen_recode)

gdf <- st_as_sf(opencell, coords = c("lon", "lat"), crs = 4326)
gdf <- st_transform(gdf, crs = 32648) %>% st_make_valid()
gdf$lon <- st_coordinates(st_transform(gdf, crs = 4326))[, "X"]
gdf$lat <- st_coordinates(st_transform(gdf, crs = 4326))[, "Y"]

prov_umts <- gdf %>%
  filter(radio == "UMTS" & year_created > 2009) %>% 
  mutate(range = 4000)


## Monthly 
vnmap1 <- st_transform(vnmap1, st_crs(gdf))
prov_points <- st_join(prov_umts, vnmap1["NAME_1"])

prov_umts_month <- prov_points %>%
  st_drop_geometry() %>%
  group_by(NAME_1) %>%
  summarise(
    first_year = min(year_created, na.rm = TRUE),
    first_month = month_created[which.min(year_created*12 + month_created)]
  ) %>% 
  rename(tinh = NAME_1) %>% 
  mutate(tinh = recode(tinh,
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
  filter(!is.na(tinh))

save(prov_umts_month, file = "Clean data/prov_umts_month.Rda")

# Share of province covered by 3G

compute_3G_coverage <- function(prov_umts, vnmap1, year_range, proj_crs) {
  
  prov_filtered <- prov_umts %>%
    filter(year_created >= year_range[1] & year_created <= year_range[2])
  
  prov_filtered <- prov_filtered %>%
    mutate(coverage_area = map2(geometry, range, ~ st_make_valid(st_buffer(.x, dist = as.numeric(.y)))))
  
  coverage_sf <- st_sf(geometry = st_sfc(prov_filtered$coverage_area, crs = proj_crs))
  
  merged_coverage <- coverage_sf %>%
    st_union() %>%
    st_sf(geometry = .) %>%
    st_make_valid()
  
  prov_cov <- st_intersection(vnmap1, merged_coverage) %>%
    mutate(coverage_m2 = st_area(geometry),
           coverage_m2 = as.numeric(set_units(coverage_m2, "m^2", mode = "standard"))) %>% 
    st_drop_geometry() %>% 
    dplyr::select(NAME_1, coverage_m2)
  
  return(prov_cov)
}

prov_covfn <- function(i){
  i %>% 
    right_join(prov_area, by = "NAME_1") %>% 
    mutate(
      coverage_share = coverage_m2 / prov_area
    ) %>% 
    dplyr::select(NAME_1, coverage_share) %>% 
    left_join(vnmap1, by = "NAME_1") %>% 
    st_as_sf()
}

vhlss_provcov_fn <- function(i){
  i %>% 
    rename_all(tolower) %>% 
    st_drop_geometry() %>% 
    mutate(tinh = recode(name_1,
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
    dplyr::select(tinh, coverage_share) %>% 
    mutate(coverage_share = ifelse(is.na(coverage_share), 0, coverage_share))
}

prov_cov10 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2010), proj_crs) %>% st_drop_geometry()
prov_cov11 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2011), proj_crs) %>% st_drop_geometry()
prov_cov12 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2012), proj_crs) %>% st_drop_geometry()
prov_cov13 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2013), proj_crs) %>% st_drop_geometry()
prov_cov14 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2014), proj_crs) %>% st_drop_geometry()
prov_cov15 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2015), proj_crs) %>% st_drop_geometry()
prov_cov16 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2016), proj_crs) %>% st_drop_geometry()
prov_cov17 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2017), proj_crs) %>% st_drop_geometry()
prov_cov18 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2018), proj_crs) %>% st_drop_geometry()
prov_cov19 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2019), proj_crs) %>% st_drop_geometry()
prov_cov20 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2020), proj_crs) %>% st_drop_geometry()

prov_cov10_shp <- prov_cov10 %>% prov_covfn()
prov_cov11_shp <- prov_cov11 %>% prov_covfn()
prov_cov12_shp <- prov_cov12 %>% prov_covfn()
prov_cov13_shp <- prov_cov13 %>% prov_covfn()
prov_cov14_shp <- prov_cov14 %>% prov_covfn()
prov_cov15_shp <- prov_cov15 %>% prov_covfn()
prov_cov16_shp <- prov_cov16 %>% prov_covfn()
prov_cov17_shp <- prov_cov17 %>% prov_covfn()
prov_cov18_shp <- prov_cov18 %>% prov_covfn()
prov_cov19_shp <- prov_cov19 %>% prov_covfn()
prov_cov20_shp <- prov_cov20 %>% prov_covfn()

prov_cov10 <- prov_cov10_shp %>% vhlss_provcov_fn() %>% rename(cov_2010 = coverage_share)
prov_cov11 <- prov_cov11_shp %>% vhlss_provcov_fn() %>% rename(cov_2011 = coverage_share)
prov_cov12 <- prov_cov12_shp %>% vhlss_provcov_fn() %>% rename(cov_2012 = coverage_share)
prov_cov13 <- prov_cov13_shp %>% vhlss_provcov_fn() %>% rename(cov_2013 = coverage_share)
prov_cov14 <- prov_cov14_shp %>% vhlss_provcov_fn() %>% rename(cov_2014 = coverage_share)
prov_cov15 <- prov_cov15_shp %>% vhlss_provcov_fn() %>% rename(cov_2015 = coverage_share)
prov_cov16 <- prov_cov16_shp %>% vhlss_provcov_fn() %>% rename(cov_2016 = coverage_share)
prov_cov17 <- prov_cov17_shp %>% vhlss_provcov_fn() %>% rename(cov_2017 = coverage_share)
prov_cov18 <- prov_cov18_shp %>% vhlss_provcov_fn() %>% rename(cov_2018 = coverage_share)
prov_cov19 <- prov_cov19_shp %>% vhlss_provcov_fn() %>% rename(cov_2019 = coverage_share)
prov_cov20 <- prov_cov20_shp %>% vhlss_provcov_fn() %>% rename(cov_2020 = coverage_share)

prov_cov10 <- prov_cov10_shp %>% vhlss_provcov_fn() %>% mutate(year = 2010)
prov_cov11 <- prov_cov11_shp %>% vhlss_provcov_fn() %>% mutate(year = 2011)
prov_cov12 <- prov_cov12_shp %>% vhlss_provcov_fn() %>% mutate(year = 2012)
prov_cov13 <- prov_cov13_shp %>% vhlss_provcov_fn() %>% mutate(year = 2013)
prov_cov14 <- prov_cov14_shp %>% vhlss_provcov_fn() %>% mutate(year = 2014)
prov_cov15 <- prov_cov15_shp %>% vhlss_provcov_fn() %>% mutate(year = 2015)
prov_cov16 <- prov_cov16_shp %>% vhlss_provcov_fn() %>% mutate(year = 2016)
prov_cov17 <- prov_cov17_shp %>% vhlss_provcov_fn() %>% mutate(year = 2017)
prov_cov18 <- prov_cov18_shp %>% vhlss_provcov_fn() %>% mutate(year = 2018)
prov_cov19 <- prov_cov19_shp %>% vhlss_provcov_fn() %>% mutate(year = 2019)
prov_cov20 <- prov_cov20_shp %>% vhlss_provcov_fn() %>% mutate(year = 2020)

umts_coverage <- bind_rows(prov_cov10, prov_cov11, prov_cov12, prov_cov13, prov_cov14,
                           prov_cov15, prov_cov16, prov_cov17, prov_cov18, prov_cov19,
                           prov_cov20)

save(umts_coverage, file = "Clean data/umts_coverage.Rda")
write_dta(umts_coverage, "Clean data/umts_coverage.Rda")

prov3g <- list(umts_coverage, prov_umts_month, cb_3G_prov_1320) %>% 
  reduce(full_join) %>% 
  rename(
    prov_cov_share_cb = prov_cov_share,
    prov_cov_share_oc = coverage_share,
    first_treated_yr = first_year,
    first_treated_m = first_month
  ) %>% 
  dplyr::select(tinh, year, prov_cov_share_cb, prov_cov_share_oc, first_treated_yr, first_treated_m)

save(prov3g, file = "Clean data/prov3g.Rda")
write_dta(prov3g, "Clean data/prov3g.dta")
