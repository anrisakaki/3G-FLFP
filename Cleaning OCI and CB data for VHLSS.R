dist_3G_vhlss <- dist_3G %>% 
  filter(year == 2010 | year == 2012 | year == 2014 | year == 2016) %>% 
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

dist1016 <- dist9919 %>% 
  select(ends_with("10"), ends_with("12"), ends_with("14"), ends_with("16")) %>% 
  select(-c(starts_with("ward"), starts_with("explanation"), starts_with("change"))) %>% 
  distinct() %>% 
  filter(if_all(everything(), ~ !is.na(.))) 
  
dist.consistent <- dist9919 %>%
  select(ends_with("10")) %>%
  select(-c(starts_with("ward"), starts_with("explanation"), starts_with("change"))) %>%
  distinct() %>%
  mutate(
    NAME_1 = provname2010 %>%
      str_remove("^(Thành phố|Tỉnh|)\\s*") %>%
      str_trim(),
    NAME_2 = distname2010 %>%
      str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP.)\\s*") %>%
      str_trim(),
    NAME_1 = ifelse(NAME_1 == "Hồ Chí Minh", "Tp Hồ Chí Minh", NAME_1),
    NAME_1 = ifelse(NAME_1 == "Bà Rịa - Vũng Tàu", "Bà Rịa-Vũng Tàu", NAME_1),
    NAME_1 = ifelse(NAME_1 == "Khánh Hòa", "Khánh Hoà", NAME_1),
    NAME_1 = ifelse(NAME_1 == "Thanh Hóa", "Thanh Hoá", NAME_1),
    NAME_1 = ifelse(NAME_1 == "Thừa Thiên Huế", "Thừa Thiên-Huế", NAME_1),
    NAME_2 = ifelse(NAME_2 == "Mỏ Cày", "Mỏ Cày Nam", NAME_2),
    NAME_2 = ifelse(NAME_2 == "Bình Phước", "Đồng Phù", NAME_2)
  ) %>%
  full_join(distid) %>%
  full_join(dist1016) %>%
  select(tinh, huyen, ID_2, NAME_1, NAME_2, ends_with("10"), ends_with("12"), ends_with("14"), ends_with("16")) %>%
  select(-c(starts_with("provname"), starts_with("distname"))) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  distinct()

cov.10.vhlss <- dist.consistent %>% 
  select(tinh, huyen, ends_with("10")) %>%
  full_join(dist_3G_vhlss) %>% 
  distinct() %>% 
  select(-c(tinh, huyen)) %>% 
  rename(tinh = prov2010,
         huyen = dist2010)

cov.12.vhlss <- dist.consistent %>% 
  select(tinh, huyen, ends_with("12")) %>%
  full_join(dist_3G_vhlss) %>% 
  distinct() %>% 
  select(-c(tinh, huyen)) %>% 
  rename(tinh = prov2012,
         huyen = dist2012)

cov.14.vhlss <- dist.consistent %>% 
  select(tinh, huyen, ends_with("14")) %>%
  full_join(dist_3G_vhlss) %>% 
  distinct() %>% 
  select(-c(tinh, huyen)) %>% 
  rename(tinh = prov2014,
         huyen = dist2014)
