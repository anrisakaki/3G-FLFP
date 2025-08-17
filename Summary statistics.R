sum_radio_yr <- gdf %>% 
  group_by(radio, year_created) %>% 
  filter(year > 1980) %>% 
  st_drop_geometry() %>% 
  summarise(n = n())

sum_radio_cummulative_yr <- gdf %>%
  group_by(radio, year_created) %>%
  filter(year_created > 1980) %>%
  st_drop_geometry() %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(radio, year) %>%
  group_by(radio) %>%
  mutate(cumulative_n = cumsum(n)) 

# VHLSS # 

###########
# Summary #
###########

sum_vhlss_fn <- function(i){
  i %>% 
    summarise(
      work = weighted.mean(work, hhwt, na.rm = T),
      work2 = weighted.mean(work2, hhwt, na.rm = T),
      informal = weighted.mean(informal, hhwt, na.rm = T),
      agri = weighted.mean(agri, hhwt, na.rm = T),
      manu = weighted.mean(manu, hhwt, na.rm = T),
      service = weighted.mean(service, hhwt, na.rm = T),
      agri_informal = weighted.mean(agri_informal, hhwt, na.rm = T),
      manu_informal = weighted.mean(manu_informal, hhwt, na.rm = T),
      service_informal = weighted.mean(service_informal, hhwt, na.rm = T),
      nonagri_informal = weighted.mean(nonagri_informal, hhwt, na.rm = T)
    )
}

options(scipen = 999)
sum_vhlss_all <- vhlss_all %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() 

female_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & age > 19 & age < 65) %>% 
  group_by(year) %>% 
  sum_vhlss_fn() 

male_sum_vhlss <- vhlss_all %>% 
  filter(female == 0 & age > 19 & age < 65) %>% 
  group_by(year) %>% 
  sum_vhlss_fn()

mothers_u7_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & children_u7 > 0) %>% 
  group_by(year) %>% 
  sum_vhlss_fn()

mothers_sum_vhlss <- vhlss_all %>% 
  filter(female == 1 & nchild > 0) %>% 
  group_by(year) %>% 
  sum_vhlss_fn()

vhlss_prov <- vhlss_all %>% 
  filter(age > 19 & age < 65) %>% 
  group_by(tinh, year,) %>% 
  summarise(
    work = weighted.mean(work, hhwt, na.rm = T),
    informal = weighted.mean(informal, hhwt, na.rm = T),
    agri = weighted.mean(agri, hhwt, na.rm = T),
    manu = weighted.mean(manu, hhwt, na.rm = T),
    service = weighted.mean(service, hhwt, na.rm = T),
    agri_informal = weighted.mean(agri_informal, hhwt, na.rm = T),
    manu_informal = weighted.mean(manu_informal, hhwt, na.rm = T),
    service_informal = weighted.mean(service_informal, hhwt, na.rm = T),
    nonagri_informal = weighted.mean(nonagri_informal, hhwt, na.rm = T)
  )

vhlss_prov_wide <- vhlss_prov %>%
  select(tinh, year, informal, agri_informal2, service_informal2, manu_informal2) %>%
  pivot_wider(
    names_from = year,
    values_from = c(informal, agri_informal2, service_informal2, manu_informal2),
    names_sep = "_"
  ) %>% 
  mutate(
    informal_1018 = informal_2018-informal_2010,
    agri_informal2_1018 = agri_informal2_2018-agri_informal2_2010,
    manu_informal2_1018 = manu_informal2_2018-manu_informal2_2010,
    service_informal2_1018 = service_informal2_2018-service_informal2_2010
  )

vhlss_prov_wide_shp <- vhlss_prov_wide %>% 
  mutate(tinh = as.numeric(tinh),
         NAME_1 = recode(tinh,
                         '89' = 'An Giang',
                         '77' = 'Bà Rịa - Vũng Tàu',
                         '24' = 'Bắc Giang',
                         '6' = 'Bắc Kạn',
                         '95' = 'Bạc Liêu',
                         '27' = 'Bắc Ninh',
                         '83' = 'Bến Tre',
                         '52' = 'Bình Định',
                         '74' = 'Bình Dương',
                         '70' = 'Bình Phước',
                         '60' = 'Bình Thuận',
                         '96' = 'Cà Mau',
                         '92' = 'Cần Thơ',
                         '4' = 'Cao Bằng',
                         '48' = 'Đà Nẵng',
                         '66' = 'Đắk Lắk',
                         '67' = 'Đắk Nông',
                         '11' = 'Điện Biên',
                         '75' = 'Đồng Nai',
                         '87' = 'Đồng Tháp',
                         '64' = 'Gia Lai',
                         '2' = 'Hà Giang',
                         '35' = 'Hà Nam',
                         '1' = 'Hà Nội',
                         '42' = 'Hà Tĩnh',
                         '30' = 'Hải Dương',
                         '31' = 'Hải Phòng',
                         '93' = 'Hậu Giang',
                         '79' = 'Hồ Chí Minh',
                         '17' = 'Hoà Bình',
                         '33' = 'Hưng Yên',
                         '56' = 'Khánh Hòa',
                         '91' = 'Kiên Giang',
                         '62' = 'Kon Tum',
                         '12' = 'Lai Châu',
                         '68' = 'Lâm Đồng',
                         '20' = 'Lạng Sơn',
                         '10' = 'Lào Cai',
                         '80' = 'Long An',
                         '36' = 'Nam Định',
                         '40' = 'Nghệ An',
                         '37' = 'Ninh Bình',
                         '58' = 'Ninh Thuận',
                         '25' = 'Phú Thọ',
                         '54' = 'Phú Yên',
                         '44' = 'Quảng Bình',
                         '49' = 'Quảng Nam',
                         '51' = 'Quảng Ngãi',
                         '22' = 'Quảng Ninh',
                         '45' = 'Quảng Trị',
                         '94' = 'Sóc Trăng',
                         '14' = 'Sơn La',
                         '72' = 'Tây Ninh',
                         '34' = 'Thái Bình',
                         '19' = 'Thái Nguyên',
                         '38' = 'Thanh Hóa',
                         '46' = 'Thừa Thiên Huế',
                         '82' = 'Tiền Giang',
                         '84' = 'Trà Vinh',
                         '8' = 'Tuyên Quang',
                         '86' = 'Vĩnh Long',
                         '26' = 'Vĩnh Phúc',
                         '15' = 'Yên Bái',
                         .default = NA_character_)) %>% 
  left_join(vnmap1) %>% 
  st_as_sf()

hhbus_sum <- hhbus_all %>% 
  group_by(year) %>% 
  summarise(erc = weighted.mean(erc, hhwt, na.rm = T))