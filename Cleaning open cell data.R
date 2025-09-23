# 2G - GSM
# 3G - UMTS
# 4G - LTE

proj_crs <- st_crs(32648)

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
  filter(radio == "UMTS" & range < 100000 & year_created > 2009) 

####################
# PROVINCE-MONTHLY #
####################
vnmap1_32648 <- st_transform(vnmap1, st_crs(gdf))
prov_points <- st_join(prov_umts, vnmap1_32648["NAME_1"])

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

#############################
# PROVINCE - LEVEL COVERAGE #
#############################


#############################
# DISTRICT - LEVEL COVERAGE #
#############################

dist18 <- dist9919 %>% select(prov2018, dist2018, distname2018) %>% distinct()

dist0418 <- dist9919 %>% 
  select(prov2004, dist2004, prov2018, dist2018) %>% 
  rename(tinh = prov2004,
         huyen = dist2004) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist0618 <- dist9919 %>% 
  select(prov2004, dist2004, prov2006, dist2006, prov2018, dist2018) %>% 
  rename(tinh = prov2006,
         huyen = dist2006) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist0818 <- dist9919 %>% 
  select(prov2008, dist2008, prov2018, dist2018) %>% 
  rename(tinh = prov2008,
         huyen = dist2008) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018)) %>% 
  mutate(dist_coverage_share = 0,
         dist_coverage = 0)

dist1018 <- dist9919 %>% 
  select(prov2010, dist2010, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2010,
         huyen = dist2010,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

dist1218 <- dist9919 %>% 
  select(prov2012, dist2012, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2012,
         huyen = dist2012,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

dist1418 <- dist9919 %>% 
  select(prov2014, dist2014, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2014,
         huyen = dist2014,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

dist1618 <- dist9919 %>% 
  select(prov2016, dist2016, prov2018, dist2018, distname2018) %>% 
  rename(tinh = prov2016,
         huyen = dist2016,
         NAME_2 = distname2018) %>% 
  distinct() %>% 
  filter(!is.na(huyen)) %>% 
  filter(!is.na(dist2018))

vnmap2 <- st_transform(vnmap2, crs = 32648) %>%
  st_make_valid() %>%
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

compute_3G_district_coverage <- function(prov_umts, vnmap2, year_range, proj_crs) {
  
  prov_filtered <- prov_umts %>%
    filter(year_created >= year_range[1] & year_created <= year_range[2])
  
  prov_filtered <- prov_filtered %>%
    mutate(coverage_area = map2(geometry, range, ~ st_make_valid(st_buffer(.x, dist = as.numeric(.y)))))
  
  coverage_sf <- st_sf(geometry = st_sfc(prov_filtered$coverage_area, crs = proj_crs))
  
  merged_coverage <- coverage_sf %>%
    st_union() %>%
    st_sf(geometry = .) %>%
    st_make_valid()
  
  dist_cov <- st_intersection(vnmap2, merged_coverage) %>%
    mutate(coverage_m2 = st_area(geometry),
           coverage_m2 = as.numeric(set_units(coverage_m2, "m^2", mode = "standard"))) %>% 
    st_drop_geometry() %>% 
    select(NAME_1, NAME_2, coverage_m2)
  
  return(dist_cov)
}

dist_covfn <- function(i){
  i %>% 
    right_join(dist_area, by = c("NAME_1", "NAME_2")) %>% 
    mutate(
      dist_area = as.numeric(set_units(dist_area, "m^2", mode = "standard")),
      coverage_share = coverage_m2 / dist_area
    ) %>% 
    select(NAME_1, NAME_2, coverage_share) %>% 
    left_join(vnmap2, by = c("NAME_1", "NAME_2")) %>% 
    mutate(coverage_share = ifelse(is.na(coverage_share), 0, coverage_share)) %>% 
  st_as_sf()
}

vhlss_distcov_fn <- function(i){
  i %>% 
    st_drop_geometry() %>% 
    mutate(coverage = ifelse(coverage_share > 0, 1, 0),
           tinh = recode(NAME_1,
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
    select(tinh, NAME_2, coverage_share, coverage) 
}

dist_cov10 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2010), proj_crs) %>% st_drop_geometry()
dist_cov11 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2011), proj_crs) %>% st_drop_geometry()
dist_cov12 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2012), proj_crs) %>% st_drop_geometry()
dist_cov13 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2013), proj_crs) %>% st_drop_geometry()
dist_cov14 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2014), proj_crs) %>% st_drop_geometry()
dist_cov15 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2015), proj_crs) %>% st_drop_geometry()
dist_cov16 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2016), proj_crs) %>% st_drop_geometry()
dist_cov17 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2017), proj_crs) %>% st_drop_geometry()
dist_cov18 <- compute_3G_district_coverage(prov_umts, vnmap2, c(2010, 2018), proj_crs) %>% st_drop_geometry()

dist_area <- vnmap2 %>%
  mutate(dist_area = st_area(geometry)) %>%
  st_drop_geometry() %>% 
  select(NAME_1, NAME_2, dist_area)

dist_cov10_shp <- dist_cov10 %>% dist_covfn()
dist_cov11_shp <- dist_cov11 %>% dist_covfn()
dist_cov12_shp <- dist_cov12 %>% dist_covfn()
dist_cov13_shp <- dist_cov13 %>% dist_covfn()
dist_cov14_shp <- dist_cov14 %>% dist_covfn()
dist_cov15_shp <- dist_cov15 %>% dist_covfn()
dist_cov16_shp <- dist_cov16 %>% dist_covfn()
dist_cov17_shp <- dist_cov17 %>% dist_covfn()
dist_cov18_shp <- dist_cov18 %>% dist_covfn()

dist_cov18 <- dist_cov18_shp %>%
  vhlss_distcov_fn() %>%
  rename(prov2018 = tinh,
         distname2018 = NAME_2) %>% 
  full_join(dist18, by = c("prov2018", "distname2018")) %>% 
  mutate(year = 2018)

dist_cov10 <- dist_cov10_shp %>%
  vhlss_distcov_fn() %>%
  left_join(dist1018, by = c("tinh", "NAME_2")) %>% 
  rename(dist_coverage_share = coverage_share,
         dist_coverage = coverage) %>% 
  select(-c(NAME_2)) %>% 
  select(tinh, huyen, everything()) %>% 
  mutate(year = 2010)

dist_cov11 <- dist_cov11_shp %>% vhlss_distcov_fn() %>% mutate(year = 2011)

dist_cov12 <- dist_cov12_shp  %>%
  vhlss_distcov_fn() %>%
  left_join(dist1218, by = c("tinh", "NAME_2")) %>% 
  rename(dist_coverage_share = coverage_share,
         dist_coverage = coverage) %>% 
  select(-c(NAME_2)) %>% 
  select(tinh, huyen, everything()) %>% 
  mutate(year = 2012)

dist_cov13 <- dist_cov13_shp %>% vhlss_distcov_fn() %>% mutate(year = 2013)

dist_cov14 <- dist_cov14_shp %>%
  vhlss_distcov_fn() %>%
  left_join(dist1418, by = c("tinh", "NAME_2")) %>% 
  rename(dist_coverage_share = coverage_share,
         dist_coverage = coverage) %>% 
  select(-c(NAME_2)) %>% 
  select(tinh, huyen, everything()) %>% 
  mutate(year = 2014)

dist_cov15 <- dist_cov15_shp %>% vhlss_distcov_fn() %>% mutate(year = 2015)

dist_cov16 <- dist_cov16_shp %>% 
  vhlss_distcov_fn() %>%
  left_join(dist1618, by = c("tinh", "NAME_2")) %>% 
  rename(dist_coverage_share = coverage_share,
         dist_coverage = coverage) %>% 
  select(-c(NAME_2)) %>% 
  select(tinh, huyen, everything()) %>% 
  mutate(year = 2016)

dist_cov17 <- dist_cov17_shp %>% vhlss_distcov_fn() %>% mutate(year = 2017)

dist_cov18a <- dist_cov18 %>% rename(dist_coverage_share = coverage_share) %>% select(-distname2018) 

umts_dist_a <- bind_rows(dist_cov10, dist_cov12, dist_cov14, dist_cov16, dist_cov18a) %>% 
  filter(!is.na(prov2018)) %>% 
  group_by(year) %>%
  summarise(n = n(),
            umts = sum(dist_coverage_share > 0, na.rm = T))

umts_dist <- bind_rows(dist_cov10, dist_cov12, dist_cov14, dist_cov16, dist_cov18a) %>% 
  filter(dist_coverage_share > 0 & !is.na(prov2018)) %>%
  group_by(prov2018, dist2018) %>%
  summarise(first_treated = min(year), .groups = "drop")

save(umts_dist, file = "Clean data/umts_dist.Rda")
write_dta(umts_dist, "Clean data/umts_dist.dta")

# Cleaning open cell data for LFS 