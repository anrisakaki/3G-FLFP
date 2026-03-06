ruralid <- wards_rural %>% 
  mutate(
    type = word(wardname, 1),
    wardname = wardname %>%
      str_remove("^(Phường|Thị|Xã.)\\s*") %>%
      str_trim(),
    urban = ifelse(type == "Xã", 0, 1),
    distname = distname %>%
      str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP.)\\s*") %>%
      str_trim(),
    provname = provname %>%
      str_remove("^(Tỉnh|Thành phố)\\s*") %>%
      str_trim(),
  ) %>% 
  group_by(provname, distname) %>% 
  mutate(mean_urban = mean(urban)) %>% 
  ungroup() %>% 
  mutate(urban = ifelse(mean_urban < 0.5, 0, 1)) %>% 
  select(provname, distname, urban) %>% 
  distinct() %>% 
  rename(
    NAME_2 = distname,
    NAME_1 = provname) %>% 
  mutate(
    NAME_1 = case_when(
    NAME_1 == "Đắk Nông" ~ "Đăk Nông",
    NAME_1 == "Hồ Chí Minh" ~ "Hồ Chí Minh city",
    NAME_1 == "Hoà Bình" ~ "Hòa Bình",
    NAME_1 == "Thừa Thiên Huế" ~ "Thừa Thiên - Huế",
    TRUE ~ NAME_1
  ),
  NAME_2 = case_when(
    NAME_2 == "Châu Đốc" ~ "Chau Doc",
    NAME_2 == "Long Xuyên" ~ "Long Xuyen Township",
    NAME_2 == "Bà Rịa" ~ "Ba Ria",
    NAME_2 == "Bắc Kạn" ~ "Bac Kan",
    NAME_2 == "Na Rì" ~ "Na Ri",
    NAME_2 == "Hoà Bình" ~ "Hòa Bình",
    NAME_2 == "Quy Nhơn" ~ "Qui Nhơn",
    NAME_2 == "Vân Canh" ~ "Van Canh",
    NAME_2 == "Dầu Tiếng" ~ "Dau Tieng",
    NAME_2 == "Dĩ An" ~ "Di An",
    NAME_2 == "Bù Đốp" ~ "Bu Dop",
    NAME_2 == "Đồng Phú" ~ "Đồng Phù",
    NAME_2 == "Đồng Xoài" ~ "Dong Xoai",
    NAME_2 == "Phú Quí" ~ "Phú Quý",
    NAME_2 == "Tánh Linh" ~ "Tanh Linh",
    NAME_2 == "Bình Thuỷ" ~ "Bình Thủy",
    NAME_2 == "Phục Hoà" ~ "Phục Hòa",
    NAME_2 == "Quảng Uyên" ~ "Quảng Yên",
    NAME_2 == "Hòa Vang" ~ "Hoà Vang",
    NAME_2 == "Buôn Ma Thuột" ~ "Buon Ma Thuot",
    NAME_2 == "Krông A Na" ~ "Krông Ana",
    NAME_2 == "Krông Búk" ~ "Krông Buk",
    NAME_2 == "Krông Pắc" ~ "Krông Pắk",
    NAME_2 == "Lắk" ~ "Lăk",
    NAME_2 == "M'Đrắk" ~ "M'Đrăk",
    NAME_2 == "Đắk Song" ~ "Dak Song",
    NAME_2 == "Điện Biên Phủ" ~ "Điên Biên Phủ",
    NAME_2 == "Biên Hòa" ~ "Bien Hoa",
    NAME_2 == "Đăk Đoa" ~ "Đắk Đoa",
    NAME_2 == "Đăk Pơ" ~ "Đắk Pơ",
    NAME_2 == "KBang" ~ "K'Bang",
    NAME_2 == "Ba Vì" ~ "Ba Vi",
    NAME_2 == "Ứng Hòa" ~ "Ứng Hoà",
    NAME_2 == "Kiến Thuỵ" ~ "Kiến Thụy",
    NAME_2 == "Thuỷ Nguyên" ~ "Thủy Nguyên",
    NAME_2 == "Vạn Ninh" ~ "Van Ninh",
    NAME_2 == "Đắk Glei" ~ "Đăk Glei",
    NAME_2 == "Đắk Hà" ~ "Đăk Hà",
    NAME_2 == "Đắk Tô" ~ "Đăk Tô",
    NAME_2 == "Đắk Mil" ~ "Đăk Mil",
    NAME_2 == "Đắk R'Lấp" ~ "Đăk R'Lấp",
    NAME_2 == "Than Uyên" ~ "Thanh Uyen",
    NAME_2 == "Văn Lãng" ~ "Vãn Lãng",
    NAME_2 == "Tân Thạnh" ~ "Tân Thành",
    NAME_2 == "Thạnh Hóa" ~ "Thanh Hóa",
    NAME_2 == "Thái Hoà" ~ "Thái Hòa",
    NAME_2 == "Phú Hoà" ~ "Phú Hòa",
    NAME_2 == "Tuy Hoà" ~ "Tuy Hoa",
    NAME_2 == "Tuyên Hóa" ~ "Tuyen Hoa",
    NAME_2 == "Tây Giang" ~ "Tay Giang",
    NAME_2 == "Thạnh Trị" ~ "Thanh Trì",
    NAME_2 == "Định Hóa" ~ "Định Hoá",
    NAME_2 == "Bỉm Sơn" ~ "Bim Son",
    NAME_2 == "Ngọc Lặc" ~ "Ngọc Lạc",
    NAME_2 == "Thanh Hóa" ~ "Thanh Hóa City",
    NAME_2 == "Gò Công" ~ "Go Cong",
    NAME_2 == "Chiêm Hóa" ~ "Chiêm Hoá",
    NAME_2 == "Tam Dương" ~ "Tam Đường",
    NAME_2 == "Mù Căng Chải" ~ "Mù Căng Trai",
    NAME_2 == "Tân Sơn" ~ "Thanh Sơn",
    NAME_2 == 'Lạc Thủy' ~ 'Lạc Thuỷ',
    NAME_2 == 'Yên Thủy' ~ 'Yên Thuỷ',
    NAME_2 == 'Tân Uyên' ~ 'Yên Thuỷ',
    NAME_2 == 'Phù Ninh' ~ 'Phú Ninh',
    TRUE ~ NAME_2),
  NAME_2 = ifelse(NAME_2 == "Tân Phú" & NAME_1 == "Hồ Chí Minh city", "Tan Phu", NAME_2),
  NAME_2 = ifelse(NAME_2 == "Bình Tân" & NAME_1 == "Hồ Chí Minh city", "Binh Tan", NAME_2),
  ) %>% 
  distinct()

vnmap2 <- vnmap2 %>% 
  mutate(NAME_2 = NAME_2 %>%
           str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP.)\\s*") %>%
           str_trim(),
         NAME_2 = ifelse(NAME_2 == "Cao Lanh", "Cao Lãnh", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Tam Dao", "Tam Đảo", NAME_2),
         NAME_2 = ifelse(NAME_1 == "Cao Bằng" & NAME_2 == "Hoà An", "Cao Bằng", NAME_2)) %>% 
  group_by(NAME_1, NAME_2) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  group_by(NAME_1, NAME_2) %>% 
  mutate(ID_2 = cur_group_id()) %>% 
  left_join(ruralid) %>% 
  distinct() %>% 
  mutate(
    urban = ifelse(NAME_2 == "Mỏ Cày", 0, urban),
    NAME_1 = case_when(
      NAME_1 == 'Bà Rịa - Vũng Tàu' ~ 'Bà Rịa-Vũng Tàu',
      NAME_1 == "Hồ Chí Minh city" ~ "Tp Hồ Chí Minh",
      NAME_1 == "Thừa Thiên - Huế" ~ "Thừa Thiên-Huế",
      NAME_1 == "Thanh Hóa" ~ "Thanh Hoá",
      NAME_1 == "Khánh Hòa" ~ "Khánh Hoà",
      NAME_1 == "Hòa Bình" ~ "Hoà Bình",
      NAME_1 == "Đăk Nông" ~ "Đắk Nông",
      TRUE ~ NAME_1),
    NAME_2 = case_when(
      NAME_2 == 'Chiêm Hoá' ~ 'Chiêm Hóa',
      NAME_2 == 'Thanh Trì' ~ 'Thạnh Trị',
      NAME_2 == 'Tam Đường' ~ 'Tam Dương',
      NAME_2 == 'Van Ninh' ~ 'Vạn Ninh',
      NAME_2 == 'Tam Dao' ~ 'Tam Đảo',
      NAME_2 == 'Mù Căng Chải' ~ 'Mù Căng Trai',
      NAME_2 == 'Đăk Glei' ~ 'Đắk Glei',
      NAME_2 == 'Đăk Tô' ~ 'Đắk Tô',
      NAME_2 == 'Binh Tan' ~ 'Bình Tân',
      NAME_2 == 'Tan Phu' ~ 'Tân Phú',
      NAME_2 == 'Go Cong' ~ 'Gò Công',
      NAME_2 == 'Đăk Hà' ~ 'Đắk Hà',
      NAME_2 == 'Thanh Uyen' ~ 'Than Uyên',
      NAME_2 == 'Mù Căng Trai' ~ 'Mù Căng Chải',
      NAME_2 == 'Vãn Lãng' ~ 'Văn Lãng',
      NAME_2 == 'Tân Thành' ~ 'Tân Thạnh',
      NAME_2 == 'Tanh Linh' ~ 'Tánh Linh',
      NAME_2 == 'Thanh Hóa' ~ 'Thạnh Hóa',
      NAME_2 == 'Thái Hòa' ~ 'Thái Hoà',
      NAME_2 == 'Tuy Hoa' ~ 'Tuy Hoà',
      NAME_2 == 'Phú Hòa' ~ 'Phú Hoà',
      NAME_2 == 'Tuyen Hoa' ~ 'Tuyên Hóa',
      NAME_2 == 'Tay Giang' ~ 'Tây Giang',
      NAME_2 == 'Định Hoá' ~ 'Định Hóa',
      NAME_2 == 'Bim Son' ~ 'Bỉm Sơn',
      NAME_2 == 'Ngọc Lạc' ~ 'Ngọc Lặc',
      NAME_2 == 'Thanh Hóa City' ~ 'Thanh Hóa',
      NAME_2 == 'Cao Lãnh (Thành phố)' ~ 'Cao Lãnh',
      NAME_2 == 'Na Ri' ~ 'Na Rì',
      NAME_2 == 'Bac Kan' ~ 'Bắc Kạn',
      NAME_2 == 'Ba Ria' ~ 'Bà Rịa',
      NAME_2 == 'Long Xuyen Township' ~ 'Long Xuyên',
      NAME_2 == 'Hoà Bình' ~ 'Hòa Bình',
      NAME_2 == 'Lạc Thuỷ' ~ 'Lạc Thủy',
      NAME_2 == 'Yên Thuỷ' ~ 'Yên Thủy',
      NAME_2 == 'Mỏ Cày' ~ 'Mỏ Cày Nam',
      NAME_2 == 'Van Canh' ~ 'Vân Canh',
      NAME_2 == 'Di An' ~ 'Dĩ An',
      NAME_2 == 'Dau Tieng' ~ 'Dầu Tiếng',
      NAME_2 == 'Bu Dop' ~ 'Bù Đốp',
      NAME_2 == 'Dong Xoai' ~ 'Đồng Xoài',
      NAME_2 == 'Bình Thủy' ~ 'Bình Thuỷ',
      NAME_2 == 'Phục Hòa' ~ 'Phục Hoà',
      NAME_2 == 'Quảng Yên' ~ 'Quảng Uyên',
      NAME_2 == 'Hoà Vang' ~ 'Hòa Vang',
      NAME_2 == 'Buon Ma Thuot' ~ 'Buôn Ma Thuột',
      NAME_2 == 'Krông Pắk' ~ 'Krông Pắc',
      NAME_2 == 'Krông Ana' ~ 'Krông A Na',
      NAME_2 == 'Lăk' ~ 'Lắk',
      NAME_2 == "M'Đrăk" ~ "M'Đrắk",
      NAME_2 == "Điên Biên Phủ" ~ "Điện Biên Phủ",
      NAME_2 == "Bien Hoa" ~ "Biên Hòa",
      NAME_2 == "Đắk Đoa" ~ "Đăk Đoa",
      NAME_2 == "Đắk Pơ" ~ "Đăk Pơ",
      NAME_2 == "K'Bang" ~ "KBang",
      NAME_2 == "Ứng Hoà" ~ "Ứng Hòa",
      NAME_2 == "Ba Vi" ~ "Ba Vì",
      NAME_2 == "Thủy Nguyên" ~ "Thuỷ Nguyên",
      NAME_2 == "Krông Buk" ~ "Krông Búk",
      NAME_2 == "Đăk Mil" ~ "Đắk Mil",
      NAME_2 == "Đăk R'Lấp" ~ "Đắk R'Lấp",
      NAME_2 == "Dak Song" ~ "Đắk Song",
      NAME_2 == "Kiến Thụy" ~ "Kiến Thuỵ",
      NAME_2 == 'Chau Doc' ~ 'Châu Đốc',
      NAME_2 == 'Hồng Ngự (Thị xã)' ~ 'Hồng Ngự',
      NAME_2 == 'Long Mỹ (Thị xã)' ~ 'Long Mỹ',
      NAME_2 == 'Kỳ Anh (Thị xã)' ~ 'Kỳ Anh',
      NAME_2 == 'Cai Lậy (Thị xã)' ~ 'Cai Lậy',
      NAME_2 == 'Duyên Hải (Thị xã)' ~ 'Duyên Hải',
      NAME_2 == "Thành Phố Bắc Kạn" ~ "Bắc Kạn",
      NAME_2 == "Đồng Phú" ~ "Đồng Phù",
      NAME_2 == "Thị Xã Buôn Hồ" ~ "Buôn Hồ",
      NAME_2 == "Thị Xã Mường Lay" ~ "Mường Lay",
      NAME_2 == "Chư Pưh" ~ "Ia Pa",
      NAME_2 == "Nam Từ Liêm" ~ "Từ Liêm",
      NAME_2 == "Bắc Từ Liêm" ~ "Từ Liêm",
      NAME_2 == "Bắc Tân Uyên" ~ "Tân Uyên",
      NAME_2 == "Thành Phố Đồng Hới" ~ "Đồng Hới",
      NAME_2 == "Mù Căng Trai" ~ "Mù Căng Chải",
      TRUE ~ NAME_2)
  ) %>% 
  mutate(NAME_2 = ifelse(NAME_2 == "Tân Thạnh" & NAME_1 == "Bà Rịa-Vũng Tàu", "Tân Thành", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Hòa Bình" & NAME_1 == "Bạc Liêu", "Hoà Bình", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Thạnh Trị" & NAME_1 == "Hà Nội", "Thanh Trì", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Tam Dương" & NAME_1 == "Lai Châu", "Tam Đường", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Phù Ninh" & NAME_1 == "Quảng Nam", "Phú Ninh", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Phú Ninh" & NAME_1 == "Phú Thọ", "Phù Ninh", NAME_2),
         NAME_2 = ifelse(NAME_2 == "Thạnh Hóa" & NAME_1 == "Thanh Hoá", "Thanh Hóa", NAME_2)) %>% 
  filter(!is.na(ID_2)) 

vnmap2_dist <- vnmap2 %>%
  st_drop_geometry() %>%
  select(ID_2, NAME_1, NAME_2) %>%
  distinct() 

distid <- lfs11_distid %>%
  select(provname, distname, tinh, huyen) %>%
  mutate(
    huyen = ifelse(tinh == 87 & huyen == 868, 870, huyen),
    huyen = ifelse(tinh == 87 & huyen == 866, 873, huyen),
    huyen = ifelse(tinh == 4 & huyen == 51, 40, huyen)
  ) %>% 
  rename(NAME_1 = provname,
         NAME_2 = distname) %>% 
  distinct() %>% 
  mutate(
    NAME_2 = NAME_2 %>%
      str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP.)\\s*") %>%
      str_trim()
  ) %>% 
  full_join(vnmap2_dist) %>% 
  filter(!is.na(tinh),
         !is.na(ID_2)) %>% 
  select(-c(NAME_1, NAME_2)) %>% 
  distinct() 

##############################
# DISTRICT-LEVEL 3G COVERAGE #
##############################

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

vnmap2 <- st_transform(vnmap2, crs = 32648) %>% 
  mutate(Shape_Area = as.numeric(st_area(geometry)))

umts <- gdf %>%
  filter(radio == "UMTS" & year_created > 2009) 

umts <- st_join(umts, vnmap2[c("NAME_1", "NAME_2", "ID_2", "urban")]) %>% 
  mutate(range = ifelse(urban == 1, 2000, 12000)) %>% 
  filter(!is.na(urban))

proj_crs <- st_crs(32648)

compute_3G_coverage_dist <- function(umts, vnmap2, year_range, proj_crs) {
  
  dist_filtered <- umts %>%
    filter(year_created >= year_range[1] & year_created <= year_range[2])
  
  dist_filtered <- dist_filtered %>%
    mutate(coverage_area = map2(geometry, range, ~ st_make_valid(st_buffer(.x, dist = as.numeric(.y)))))
  
  coverage_sf <- st_sf(geometry = st_sfc(dist_filtered$coverage_area, crs = proj_crs))
  
  merged_coverage <- coverage_sf %>%
    st_union() %>%
    st_sf(geometry = .) %>%
    st_make_valid()
  
  dist_cov <- st_intersection(vnmap2, merged_coverage) %>%
    mutate(
      coverage_area = st_area(geometry), 
      coverage_area_m2 = as.numeric(coverage_area),  
      share_3G_OCI = coverage_area_m2 / Shape_Area  
    ) %>%
    st_drop_geometry() %>%  
    dplyr::select(ID_2, share_3G_OCI) 
  
  return(dist_cov)
}

dists <- vnmap2 %>% st_drop_geometry() %>% ungroup %>% select(ID_2) %>% distinct() 

dist_cov10 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2010), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2010) 
dist_cov11 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2011), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2011) 
dist_cov12 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2012), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2012) 
dist_cov13 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2013), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2013) 
dist_cov14 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2014), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2014) 
dist_cov15 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2015), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2015) 
dist_cov16 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2016), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2016) 
dist_cov17 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2017), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2017) 
dist_cov18 <- compute_3G_coverage_dist(umts, vnmap2, c(2010, 2018), proj_crs) %>% st_drop_geometry() %>% full_join(dists) %>% mutate(year = 2018) 

mean_coverage16_oci <- dist_cov16 %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  summarise(mean_share_3G_OCI = mean(share_3G_OCI)) %>% 
  pull(mean_share_3G_OCI)
median_coverage16_oci <- dist_cov16 %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  summarise(med_share_3G_OCI = median(share_3G_OCI)) %>% 
  pull(med_share_3G_OCI)

mean_coverage17_oci <- dist_cov17 %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  summarise(mean_share_3G_OCI = mean(share_3G_OCI)) %>% 
  pull(mean_share_3G_OCI)
median_coverage17_oci <- dist_cov17 %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  summarise(med_share_3G_OCI = median(share_3G_OCI)) %>% 
  pull(med_share_3G_OCI)

oci_dist_1016 <- bind_rows(dist_cov10, dist_cov11, dist_cov12, dist_cov13,
                           dist_cov14, dist_cov15, dist_cov16) %>% 
  select(year, ID_2, share_3G_OCI) %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_OCI = ifelse(any(share_3G_OCI >= mean_coverage16_oci), 1, 0),
    med_3G_OCI  = ifelse(any(share_3G_OCI >= median_coverage16_oci), 1, 0),
    year_mean_OCI = ifelse(any(share_3G_OCI >= mean_coverage16_oci),
                           min(year[share_3G_OCI >= mean_coverage16_oci], na.rm = T),
                           NA),
    year_med_OCI = ifelse(any(share_3G_OCI >= median_coverage16_oci),
                          min(year[share_3G_OCI >= median_coverage16_oci], na.rm = T),
                          NA),
    year_OCI = ifelse(any(share_3G_OCI > 0),
                      min(year[share_3G_OCI > 0], na.rm = TRUE),
                      NA)
  )

oci_dist_1017 <- bind_rows(dist_cov10, dist_cov11, dist_cov12, dist_cov13,
                           dist_cov14, dist_cov15, dist_cov16, dist_cov17) %>% 
  select(year, ID_2, share_3G_OCI) %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci), 1, 0),
    med_3G_OCI  = ifelse(any(share_3G_OCI >= median_coverage17_oci), 1, 0),
    year_mean_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci),
                           min(year[share_3G_OCI >= mean_coverage17_oci], na.rm = T),
                           NA),
    year_med_OCI = ifelse(any(share_3G_OCI >= median_coverage17_oci),
                          min(year[share_3G_OCI >= median_coverage17_oci], na.rm = T),
                          NA),
    year_OCI = ifelse(any(share_3G_OCI > 0),
                      min(year[share_3G_OCI > 0], na.rm = TRUE),
                      NA)
  )

oci_dist_1018 <- bind_rows(dist_cov10, dist_cov11, dist_cov12, dist_cov13,
                           dist_cov14, dist_cov15, dist_cov16, dist_cov17, dist_cov18) %>% 
  select(year, ID_2, share_3G_OCI) %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci), 1, 0),
    med_3G_OCI  = ifelse(any(share_3G_OCI >= median_coverage17_oci), 1, 0),
    year_mean_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci),
                           min(year[share_3G_OCI >= mean_coverage17_oci], na.rm = T),
                           NA),
    year_med_OCI = ifelse(any(share_3G_OCI >= median_coverage17_oci),
                          min(year[share_3G_OCI >= median_coverage17_oci], na.rm = T),
                          NA),
    year_OCI = ifelse(any(share_3G_OCI > 0),
                      min(year[share_3G_OCI > 0], na.rm = TRUE),
                      NA)
  )

oci_dist_1018 <- bind_rows(dist_cov10, dist_cov11, dist_cov12, dist_cov13,
                           dist_cov14, dist_cov15, dist_cov16, dist_cov17, dist_cov18) %>% 
  select(year, ID_2, share_3G_OCI) %>% 
  mutate(share_3G_OCI = ifelse(is.na(share_3G_OCI), 0, share_3G_OCI)) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci), 1, 0),
    med_3G_OCI  = ifelse(any(share_3G_OCI >= median_coverage17_oci), 1, 0),
    year_mean_OCI = ifelse(any(share_3G_OCI >= mean_coverage17_oci),
                             min(year[share_3G_OCI >= mean_coverage17_oci], na.rm = T),
                             NA),
    year_med_OCI = ifelse(any(share_3G_OCI >= median_coverage17_oci),
                            min(year[share_3G_OCI >= median_coverage17_oci], na.rm = T),
                            NA),
    year_OCI = ifelse(any(share_3G_OCI > 0),
                      min(year[share_3G_OCI > 0], na.rm = TRUE),
                      NA)
  )

save(oci_dist_1018, file = "Clean data/oci_dist_1018.Rda")
write_dta(oci_dist_1018, "Clean data/oci_dist_1018.dta")

#####################
# DISTRICT-LEVEL CB #
#####################

vnmap2 <- st_transform(vnmap2, crs = crs(cb13))

raster_list <- list(cb12, cb13, cb14, cb15, cb16, cb17, cb18)
names(raster_list) <- paste0("share_3G_", 12:18)

shares <- map_dfc(
  raster_list,
  ~ exact_extract(.x, vnmap2, function(values, coverage_fraction) {
    values_no_na <- ifelse(is.na(values), 0, values)
    sum((values_no_na > 0) * coverage_fraction, na.rm = TRUE) /
      sum(coverage_fraction, na.rm = TRUE)
  })
)

cb_1218_shp <- bind_cols(vnmap2, shares)

cb_dist_1218 <- cb_1218_shp %>%
  st_drop_geometry() %>%
  select(ID_2, starts_with("share_"))

cb.10 <- cb_dist_1218 %>% select(ID_2) %>% mutate(year = 2010, share_3G_CB = 0)
cb.11 <- cb_dist_1218 %>% select(ID_2) %>% mutate(year = 2011, share_3G_CB = 0)
cb.12 <- cb_dist_1218 %>% select(ID_2, share_3G_12) %>% mutate(year = 2012) %>% rename(share_3G_CB = share_3G_12)
cb.13 <- cb_dist_1218 %>% select(ID_2, share_3G_13) %>% mutate(year = 2013) %>% rename(share_3G_CB = share_3G_13)
cb.14 <- cb_dist_1218 %>% select(ID_2, share_3G_14) %>% mutate(year = 2014) %>% rename(share_3G_CB = share_3G_14)
cb.15 <- cb_dist_1218 %>% select(ID_2, share_3G_15) %>% mutate(year = 2015) %>% rename(share_3G_CB = share_3G_15)
cb.16 <- cb_dist_1218 %>% select(ID_2, share_3G_15) %>% mutate(year = 2016) %>% rename(share_3G_CB = share_3G_15)
cb.17 <- cb_dist_1218 %>% select(ID_2, share_3G_15) %>% mutate(year = 2017) %>% rename(share_3G_CB = share_3G_15)
cb.18 <- cb_dist_1218 %>% select(ID_2, share_3G_15) %>% mutate(year = 2018) %>% rename(share_3G_CB = share_3G_15)

mean_coverage16_cb <- mean(cb.16$share_3G_CB, na.rm = T)
median_coverage16_cb <- median(cb.16$share_3G_CB, na.rm = T)
mean_coverage17_cb <- mean(cb.17$share_3G_CB, na.rm = T)
median_coverage17_cb <- median(cb.17$share_3G_CB, na.rm = T)

cb_dist_1016 <- bind_rows(cb.10, cb.11, cb.12, cb.13, cb.14, cb.15, cb.16) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_CB = ifelse(any(share_3G_CB >= mean_coverage16_cb), 1, 0),
    med_3G_CB  = ifelse(any(share_3G_CB >= median_coverage16_cb), 1, 0),
    year_mean_CB = ifelse(any(share_3G_CB >= mean_coverage16_cb),
                          min(year[share_3G_CB >= mean_coverage16_cb], na.rm = T),
                          NA),
    year_med_CB = ifelse(any(share_3G_CB >= median_coverage16_cb),
                         min(year[share_3G_CB >= median_coverage16_cb], na.rm = T),
                         NA),
    share_3G_CB = ifelse(year < 2012, NA, share_3G_CB)
  )

cb_dist_1017 <- bind_rows(cb.10, cb.11, cb.12, cb.13, cb.14, cb.15, cb.16, cb.17) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_CB = ifelse(any(share_3G_CB >= mean_coverage17_cb), 1, 0),
    med_3G_CB  = ifelse(any(share_3G_CB >= median_coverage17_cb), 1, 0),
    year_mean_CB = ifelse(any(share_3G_CB >= mean_coverage17_cb),
                          min(year[share_3G_CB >= mean_coverage17_cb], na.rm = T),
                          NA),
    year_med_CB = ifelse(any(share_3G_CB >= median_coverage17_cb),
                         min(year[share_3G_CB >= median_coverage17_cb], na.rm = T),
                         NA),
    share_3G_CB = ifelse(year < 2012, NA, share_3G_CB)
  )


cb_dist_1018 <- bind_rows(cb.10, cb.11, cb.12, cb.13, cb.14, cb.15, cb.16, cb.17, cb.18) %>% 
  group_by(ID_2) %>% 
  mutate(
    mean_3G_CB = ifelse(any(share_3G_CB >= mean_coverage17_cb), 1, 0),
    med_3G_CB  = ifelse(any(share_3G_CB >= median_coverage17_cb), 1, 0),
    year_mean_CB = ifelse(any(share_3G_CB >= mean_coverage17_cb),
                           min(year[share_3G_CB >= mean_coverage17_cb], na.rm = T),
                           NA),
    year_med_CB = ifelse(any(share_3G_CB >= median_coverage17_cb),
                          min(year[share_3G_CB >= median_coverage17_cb], na.rm = T),
                          NA),
    share_3G_CB = ifelse(year < 2012, NA, share_3G_CB)
  )

save(cb_dist_1018, file = "Clean data/cb_dist_1018.Rda")
write_dta(cb_dist_1018, "Clean data/cb_dist_1018.dta")

options(scipen = 999)
dist_3G <- list(cb_dist_1017, oci_dist_1017, distid) %>% 
  reduce(merge) %>% 
  filter(!is.na(year),
         !is.na(tinh)) %>% 
  distinct() %>% 
  select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB")) 

save(dist_3G, file = "Clean data/dist_3G.Rda")
write_dta(dist_3G, "Clean data/dist_3G.dta")

dist_3G_shp <- left_join(vnmap2, dist_3G) 

dist_3G_shp$year_mean_OCI <- forcats::fct_na_value_to_level(
  as.factor(dist_3G_shp$year_mean_OCI),
  level = "Control"
)

dist_3G_shp$year_med_OCI <- forcats::fct_na_value_to_level(
  as.factor(dist_3G_shp$year_med_OCI),
  level = "Control"
)

sum_year_3G <- dist_3G %>% group_by(year) %>% summarise(median = median(share_3G_OCI*100), mean = mean(share_3G_OCI*100))

ggplot() +
  geom_sf(data = dplyr::filter(dist_3G_shp, !is.na(year_OCI)), aes(fill = as.factor(year_OCI)), color = NA) +  
  scale_fill_manual(
    name = "Year 3G first recorded",
    values = c(
      "2017" = "#f3ff82",  
      "2016" = "#abeb88",  
      "2015" = "#67d294",  
      "2014" = "#17b79c",  
      "2013" = "#009a9c", 
      "2012" = "#007c92", 
      "2011" = "#005e7d",  
      "2010" = "#1f4260"
    ),
    labels = c(
      "2010" = "2010 (N = 20)",
      "2011" = "2011 (N = 51)",
      "2012" = "2012 (N = 146)",
      "2013" = "2013 (N = 254)",
      "2014" = "2014 (N = 49)",
      "2015" = "2015 (N = 107)",
      "2016" = "2016 (N = 16)",
      "2017" = "2017 (N = 2)"
    )
  )+
  geom_sf(data = vnmap2, fill = NA, color = "black", size = 0.2) +
  theme_minimal() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/OCI_map.png", width = 7, height = 14)

ggplot() +
  geom_sf(data = dist_3G_shp, aes(fill = as.factor(year_med_OCI)), color = NA) +  
  scale_fill_manual(
    name = "Year treated",
    values = c(
      "2017" = "#f3ff82",  
      "2016" = "#abeb88",  
      "2015" = "#67d294",  
      "2014" = "#17b79c",  
      "2013" = "#009a9c", 
      "2012" = "#007c92", 
      "2011" = "#005e7d",  
      "2010" = "#1f4260",
      "Control" = "grey80"
    ),
    labels = c(
      "2010" = "2010 (N = 2)",
      "2011" = "2011 (N = 6)",
      "2012" = "2012 (N = 21)",
      "2013" = "2013 (N = 70)",
      "2014" = "2014 (N = 42)",
      "2015" = "2015 (N = 148)",
      "2016" = "2016 (N = 42)",
      "2017" = "2017 (N = 27)",
      "Control" = " Control (N = 304)")
  )+
  geom_sf(data = vnmap2, fill = NA, color = "black", size = 0.2) +
  labs(fill = "Year treated") +
  theme_minimal() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/med_OCI_map.png", width = 7, height = 14)

ggplot() +
  geom_sf(data = dist_3G_shp, aes(fill = as.factor(year_mean_OCI)), color = NA) +  
  scale_fill_manual(
    name = "Year treated",
    values = c(
      "2017" = "#f3ff82",  
      "2016" = "#abeb88",  
      "2015" = "#67d294",  
      "2014" = "#17b79c",  
      "2013" = "#009a9c", 
      "2012" = "#007c92", 
      "2011" = "#005e7d",  
      "2010" = "#1f4260",  
      "Control" = "grey80"
    ),
    labels = c(
      "2010" = "2010 (N = 4)",
      "2011" = "2011 (N = 13)",
      "2012" = "2012 (N = 52)",
      "2013" = "2013 (N = 117)",
      "2014" = "2014 (N = 48)",
      "2015" = "2015 (N = 189)",
      "2016" = "2016 (N = 53)",
      "2017" = "2017 (N = 21)",
      "Control" = " Control (N = 165)")
  )+
  geom_sf(data = vnmap2, fill = NA, color = "black", size = 0.2) +
  labs(fill = "Year treated") +
  theme_minimal() + 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks   = element_blank(),
    axis.line    = element_blank()
  )
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/mean_OCI_map.png", width = 7, height = 14)

ggplot(sum_year_3G, aes(x = year)) +
  geom_line(aes(y = mean, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = median, color = "Median"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = mean, color = "Mean")) +
  geom_point(aes(y = median, color = "Median")) +
  labs(
    y = "District-level 3G coverage (%)",
    color = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(color = "black")
  ) +
  scale_x_continuous(breaks = 2010:2017)
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/cov_yearly_OCI.png", width = 7, height = 7)

ggplot(
  dist_3G, aes(x = share_3G_OCI * 100)
) +
  geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "grey70",
    color = "white"
  ) +
  facet_wrap(~ year, ncol = 1, axes = "all") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    x = "District level 3G Coverage (%)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(color = "black"),
    strip.text = element_text(face = "bold", size = 14)
  )
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/cov_yearly_hist_OCI.png", width = 7, height = 21)
