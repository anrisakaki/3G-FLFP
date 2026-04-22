ruralid <- wards_rural %>% 
  mutate(
    type = word(wardname, 1),
    wardname = wardname %>%
      str_remove("^(Phường|Thị|Xã\\.)\\s*") %>%
      str_trim(),
    urban = ifelse(type == "Xã", 0, 1),
    distname = distname %>%
      str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP\\.)\\s*") %>%
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
    NAME_2 = case_when(
    NAME_2 == "Quy Nhơn" ~ "Qui Nhơn",
    TRUE ~ NAME_2)
  ) %>% 
  distinct()

vnmap2 <- vnmap2 %>% 
  mutate(NAME_2 = NAME_2 %>%
           str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP\\.)\\s*") %>%
           str_remove(" \\(Thành phố\\)$") %>%
           str_remove(" \\(Thị xã\\)$") %>%
           str_trim(),
         NAME_2 = case_when(
           NAME_2 == 'Bắc Từ Liêm' ~ 'Từ Liêm',
           NAME_2 == 'Nam Từ Liêm' ~ 'Từ Liêm',
           NAME_2 == 'Mỏ Cày Bắc' ~ 'Mỏ Cày Nam',
           NAME_2 == 'Bắc Tân Uyên' ~ 'Tân Uyên',
           TRUE ~ NAME_2
         ),
         NAME_2 = ifelse(NAME_2 == "Hoà An" & NAME_1 == "Cao Bằng", "Cao Bằng", NAME_2)
         ) %>% 
  group_by(NAME_1, NAME_2) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  group_by(NAME_1, NAME_2) %>% 
  mutate(ID_2 = cur_group_id()) %>% 
  merge(ruralid) %>% 
  distinct() %>% 
  filter(!is.na(ID_2)) 

vnmap2_dist <- vnmap2 %>%
  st_drop_geometry() %>%
  select(ID_2, NAME_1, NAME_2) %>%
  distinct() %>% 
  mutate(
    NAME_1 = case_when(
      NAME_1 == 'Bà Rịa - Vũng Tàu'~ 'Bà Rịa-Vũng Tàu',
      NAME_1 == 'Khánh Hòa'~ 'Khánh Hoà',
      NAME_1 == 'Thanh Hóa'~ 'Thanh Hoá',
      NAME_1 == 'Thừa Thiên Huế'~ 'Thừa Thiên-Huế',
      NAME_1 == 'Hồ Chí Minh'~ 'Tp Hồ Chí Minh',
      TRUE ~ NAME_1
    ),
    NAME_2 = case_when(
      NAME_2 == 'Đồng Phú'~ "Đồng Phù",
      TRUE ~ NAME_2
    )
  )

distid <- lfs11_distid %>%
  select(provname, distname, tinh, huyen) %>%
  mutate(
    huyen = ifelse(tinh == 87 & huyen == 868, 870, huyen),
    huyen = ifelse(tinh == 87 & huyen == 866, 873, huyen),
    huyen = ifelse(tinh == 4 & huyen == 51, 40, huyen)
  ) %>% 
  distinct() %>% 
  rename(NAME_1 = provname,
         NAME_2 = distname) %>% 
  mutate(
    NAME_2 = NAME_2 %>%
      str_remove("^(Quận|Huyện|Thành phố|Thị xã|TP|Thị Xã|Thành Phố|TP\\.)\\s*") %>%
      str_trim()
  ) %>% 
  full_join(vnmap2_dist) %>% 
  distinct() %>% 
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

compute_3G_coverage_dist <- function(umts, vnmap2, year_range, proj_crs, pop_raster = NULL) {

  dist_filtered <- umts %>%
    filter(year_created >= year_range[1] & year_created <= year_range[2])

  dist_filtered <- dist_filtered %>%
    mutate(coverage_area = map2(geometry, range, ~ st_make_valid(st_buffer(.x, dist = as.numeric(.y)))))

  coverage_sf <- st_sf(geometry = st_sfc(dist_filtered$coverage_area, crs = proj_crs))

  merged_coverage <- coverage_sf %>%
    st_union() %>%
    st_sf(geometry = .) %>%
    st_make_valid()

  dist_cov_sf <- st_intersection(vnmap2, merged_coverage) %>% st_make_valid()

  dist_cov <- dist_cov_sf %>%
    mutate(
      coverage_area = st_area(geometry),
      coverage_area_m2 = as.numeric(coverage_area),
      share_3G_OCI = coverage_area_m2 / Shape_Area
    ) %>%
    st_drop_geometry() %>%
    dplyr::select(ID_2, share_3G_OCI)

  if (!is.null(pop_raster)) {
    pop_proj <- terra::project(pop_raster, terra::crs(dist_cov_sf))
    pop_proj <- terra::aggregate(pop_proj, fact = 3, fun = "sum", na.rm = TRUE)

    pop_covered_vals <- exact_extract(pop_proj, dist_cov_sf, fun = "sum")
    pop_covered <- dist_cov_sf %>%
      st_drop_geometry() %>%
      ungroup() %>%
      dplyr::select(ID_2) %>%
      mutate(pop_covered = pop_covered_vals) %>%
      group_by(ID_2) %>%
      summarise(pop_covered = sum(pop_covered, na.rm = TRUE), .groups = "drop")

    pop_total_vals <- exact_extract(pop_proj, vnmap2, fun = "sum")
    pop_total <- vnmap2 %>%
      st_drop_geometry() %>%
      ungroup() %>%
      dplyr::select(ID_2) %>%
      mutate(total_pop = pop_total_vals)

    dist_cov <- dist_cov %>%
      left_join(pop_covered, by = "ID_2") %>%
      left_join(pop_total, by = "ID_2") %>%
      mutate(
        pop_covered = replace_na(pop_covered, 0),
        ppn_3G_OCI = pop_covered / total_pop
      ) %>%
      dplyr::select(ID_2, share_3G_OCI, ppn_3G_OCI)
  }

  return(dist_cov)
}

dists <- vnmap2 %>% st_drop_geometry() %>% ungroup() %>% select(ID_2) %>% distinct()

build_dist_cov_year <- function(year_end, umts, vnmap2, dists, proj_crs) {
  pop_raster <- if (year_end <= 2017) pop_list[[paste0("pop", substr(as.character(year_end), 3, 4))]] else NULL
  compute_3G_coverage_dist(umts, vnmap2, c(2010, year_end), proj_crs, pop_raster = pop_raster) %>%
    full_join(dists, by = "ID_2") %>%
    mutate(year = year_end)
}

oci_dist_1017 <- map_dfr(2008:2017, build_dist_cov_year, umts = umts, vnmap2 = vnmap2, dists = dists, proj_crs = proj_crs)

save(oci_dist_1017, file = "Clean data/oci_dist_1017.Rda")
write_dta(oci_dist_1017, "Clean data/oci_dist_1017.dta")

#####################
# DISTRICT-LEVEL CB #
#####################

vnmap2 <- st_transform(vnmap2, crs = crs(cb13))

raster_list <- list(cb12, cb13, cb14, cb15, cb16, cb17)
names(raster_list) <- paste0("share_3G_", 12:17)

shares <- map_dfc(
  raster_list,
  ~ exact_extract(.x, vnmap2, function(values, coverage_fraction) {
    values_no_na <- ifelse(is.na(values), 0, values)
    sum((values_no_na > 0) * coverage_fraction, na.rm = TRUE) /
      sum(coverage_fraction, na.rm = TRUE)
  })
)

cb_1217_shp <- bind_cols(vnmap2, shares)

cb_dist_1217 <- cb_1217_shp %>%
  st_drop_geometry() %>%
  select(ID_2, starts_with("share_"))

build_cb_year <- function(year_value, cb_dist_1217, vnmap2) {
  if (year_value < 2012) {
    return(cb_dist_1217 %>% select(ID_2) %>% mutate(year = year_value, share_3G_CB = 0, ppn_3G_CB = 0))
  }

  source_col <- paste0("share_3G_", substr(year_value, 3, 4))
  result <- cb_dist_1217 %>%
    select(ID_2, all_of(source_col)) %>%
    mutate(year = year_value) %>%
    rename(share_3G_CB = all_of(source_col))

  cb_ras   <- raster_list[[paste0("share_3G_", substr(as.character(year_value), 3, 4))]]
  pop_proj <- terra::resample(
    terra::project(pop_list[[paste0("pop", substr(as.character(year_value), 3, 4))]], terra::crs(cb_ras)),
    cb_ras,
    method = "sum"
  )
  vals <- exact_extract(cb_ras, vnmap2,
    function(values, coverage_fraction, weights) {
      covered <- ifelse(is.na(values), 0, as.numeric(values > 0))
      pop_cov <- sum(covered * weights * coverage_fraction, na.rm = TRUE)
      pop_tot <- sum(weights * coverage_fraction, na.rm = TRUE)
      ifelse(pop_tot > 0, pop_cov / pop_tot, 0)
    }, weights = pop_proj)

  pop_df <- vnmap2 %>%
    st_drop_geometry() %>%
    ungroup() %>%
    dplyr::select(ID_2) %>%
    mutate(ppn_3G_CB = vals)

  result %>% left_join(pop_df, by = "ID_2")
}

cb_dist_1017 <- map_dfr(2008:2017, build_cb_year, cb_dist_1217 = cb_dist_1217, vnmap2 = vnmap2) %>%
  group_by(ID_2) %>%
  arrange(year) %>%
  mutate(share_3G_CB = cummax(replace_na(share_3G_CB, 0)),
         ppn_3G_CB   = cummax(replace_na(ppn_3G_CB, 0))) %>%
  ungroup()

save(cb_dist_1017, file = "Clean data/cb_dist_1017.Rda")
write_dta(cb_dist_1017, "Clean data/cb_dist_1017.dta")

options(scipen = 999)
dist_3G <- list(cb_dist_1017, oci_dist_1017, distid) %>% 
  reduce(merge) %>% 
  filter(!is.na(year),
         !is.na(tinh)) %>% 
  distinct() %>% 
  select(year, ID_2, tinh, huyen, ends_with("_OCI"), ends_with("_CB")) 

median_coverage17_oci <- dist_3G %>%
  filter(year == 2017) %>%
  mutate(ppn_3G_OCI = coalesce(ppn_3G_OCI, 0)) %>%
  summarise(ppn_3G_OCI = median(ppn_3G_OCI)) %>%
  pull(ppn_3G_OCI)

mean_coverage17_oci <- dist_3G %>%
  filter(year == 2017) %>%
  mutate(ppn_3G_OCI = coalesce(ppn_3G_OCI, 0)) %>%
  summarise(ppn_3G_OCI = mean(ppn_3G_OCI)) %>%
  pull(ppn_3G_OCI)

median_coverage17_cb <- dist_3G %>%
  filter(year == 2017) %>%
  mutate(ppn_3G_CB = coalesce(ppn_3G_CB, 0)) %>%
  summarise(ppn_3G_CB = median(ppn_3G_CB)) %>%
  pull(ppn_3G_CB)

mean_coverage17_cb <- dist_3G %>%
  filter(year == 2017) %>%
  mutate(ppn_3G_CB = coalesce(ppn_3G_CB, 0)) %>%
  summarise(ppn_3G_CB = mean(ppn_3G_CB)) %>%
  pull(ppn_3G_CB)

dist_3G <- dist_3G %>% 
  mutate(ppn_3G_OCI   = coalesce(ppn_3G_OCI, 0),
         share_3G_OCI = coalesce(share_3G_OCI, 0)) %>%
  group_by(ID_2) %>%
  mutate(
    mean_3G_OCI   = ifelse(any(ppn_3G_OCI >= mean_coverage17_oci), 1, 0),
    med_3G_OCI    = ifelse(any(ppn_3G_OCI >= median_coverage17_oci), 1, 0),
    year_mean_OCI = ifelse(any(ppn_3G_OCI >= mean_coverage17_oci),
                           min(year[ppn_3G_OCI >= mean_coverage17_oci], na.rm = TRUE), NA),
    year_med_OCI  = ifelse(any(ppn_3G_OCI >= median_coverage17_oci),
                           min(year[ppn_3G_OCI >= median_coverage17_oci], na.rm = TRUE), NA),
    year_OCI      = ifelse(any(ppn_3G_OCI > 0),
                           min(year[ppn_3G_OCI > 0], na.rm = TRUE), NA),
    
    mean_3G_CB   = ifelse(any(ppn_3G_CB >= mean_coverage17_cb), 1, 0),
    med_3G_CB    = ifelse(any(ppn_3G_CB >= median_coverage17_cb), 1, 0),
    year_mean_CB = ifelse(any(ppn_3G_CB >= mean_coverage17_cb),
                           min(year[ppn_3G_CB >= mean_coverage17_cb], na.rm = TRUE), NA),
    year_med_CB  = ifelse(any(ppn_3G_CB >= median_coverage17_cb),
                           min(year[ppn_3G_CB >= median_coverage17_cb], na.rm = TRUE), NA),
    year_CB      = ifelse(any(ppn_3G_CB > 0),
                           min(year[ppn_3G_CB > 0], na.rm = TRUE), NA)
  ) %>%
  ungroup()

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

sum_year_3G <- dist_3G %>% group_by(year) %>% summarise(median = median(ppn_3G_OCI*100), mean = mean(ppn_3G_OCI*100))

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
      "2010" = "2010 (N = 30)",
      "2011" = "2011 (N = 56)",
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
      "2010" = "2010 (N = 2)",
      "2011" = "2011 (N = 11)",
      "2012" = "2012 (N = 51)",
      "2013" = "2013 (N = 122)",
      "2014" = "2014 (N = 53)",
      "2015" = "2015 (N = 215)",
      "2016" = "2016 (N = 60)",
      "2017" = "2017 (N = 25)",
      "Control" = " Control (N = 135)")
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
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/ppn_mean_OCI_map.png", width = 7, height = 14)

ggplot(sum_year_3G, aes(x = year)) +
  geom_line(aes(y = mean, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = median, color = "Median"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = mean, color = "Mean")) +
  geom_point(aes(y = median, color = "Median")) +
  labs(
    y = "Share of district population covered by 3G coverage (%)",
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
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/cov_ppn_yearly_OCI.png", width = 7, height = 7)

ggplot(
  dist_3G, aes(x = ppn_3G_OCI * 100)
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
    x = "Share of district population covered by 3G (%)",
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
ggsave("C:/Users/Anri Sakakibara/Dropbox/Apps/Overleaf/3G in Vietnam/Figures/Descriptive Stats/ppn_cov_yearly_hist_OCI.png", width = 7, height = 21)
