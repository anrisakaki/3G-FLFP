################################
# DISTRICT - LEVEL 3G COVERAGE #
################################

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

# Collins Bartholomew data 

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

# Population coverage by district

# Get population raster for a given year
pop_for_year <- function(yr) {
  pop_list[[paste0("pop", substr(as.character(yr), 3, 4))]]
}

# Total population per district from raster
dist_total_pop_fn <- function(pop_raster) {
  pop_proj <- terra::project(pop_raster, terra::crs(vnmap2))
  pop_vals <- terra::extract(pop_proj, terra::vect(vnmap2), fun = sum, na.rm = TRUE)
  vnmap2 %>%
    st_drop_geometry() %>%
    dplyr::select(GID_2) %>%
    mutate(total_pop = pop_vals[, 2])
}

# Population within 3G coverage area per district
cb_pop_cov_fn <- function(cb_int, pop_raster) {
  pop_proj <- terra::project(pop_raster, terra::crs(cb_int))
  pop_vals <- terra::extract(pop_proj, terra::vect(cb_int), fun = sum, na.rm = TRUE)
  cb_int %>%
    st_drop_geometry() %>%
    dplyr::select(GID_2) %>%
    mutate(pop_covered = pop_vals[, 2]) %>%
    group_by(GID_2) %>%
    summarise(pop_covered = sum(pop_covered, na.rm = TRUE), .groups = "drop")
}

cb_pop_years <- list(
  list(cb_int = cb13_int, year = 2013),
  list(cb_int = cb14_int, year = 2014),
  list(cb_int = cb15_int, year = 2015),
  list(cb_int = cb16_int, year = 2016),
  list(cb_int = cb17_int, year = 2017)
)

pop_cov_dist <- map_dfr(cb_pop_years, function(x) {
  pop_raster  <- pop_for_year(x$year)
  total_pop   <- dist_total_pop_fn(pop_raster)
  covered_pop <- cb_pop_cov_fn(x$cb_int, pop_raster)
  total_pop %>%
    left_join(covered_pop, by = "GID_2") %>%
    mutate(
      pop_covered   = replace_na(pop_covered, 0),
      pop_cov_share = pop_covered / total_pop,
      year = x$year
    ) %>%
    dplyr::select(GID_2, year, pop_cov_share)
})

cb_3G_1320 <- bind_rows(cb13_cov_dist, cb14_cov_dist, cb15_cov_dist, cb16_cov_dist,
                        cb17_cov_dist, cb18_cov_dist, cb19_cov_dist, cb20_cov_dist) %>%
  dplyr::select(-c(prov2018, dist2018, tinh, huyen, dist_cov, dist_area)) %>%
  left_join(pop_cov_dist, by = c("GID_2", "year"))

cb_3G_vhlss <- bind_rows(cb14_cov_dist, cb16_cov_dist, cb18_cov_dist, cb20_cov_dist) %>%
  left_join(pop_cov_dist, by = c("GID_2", "year")) %>%
  group_by(GID_2) %>%
  mutate(distid = cur_group_id()) %>%
  ungroup() %>%
  dplyr::select(-c(prov2018, dist2018, GID_2, dist_cov, dist_area))

save(cb_3G_1320, file = "Clean data/cb_3G_1320.Rda")
write_dta(cb_3G_1320, "Clean data/cb_3G_1320.dta")
save(cb_3G_vhlss, file = "Clean data/cb_3G_vhlss.Rda")
write_dta(cb_3G_vhlss, "Clean data/cb_3G_vhlss.dta")

########
# MAPS #
########

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
