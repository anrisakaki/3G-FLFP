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

vnmap1 <- st_transform(vnmap1, crs = 32648) %>% st_make_valid()

# Calculating share of province that is covered by 3G
prov_umts <- gdf %>% filter(radio == "UMTS")

compute_3G_coverage <- function(prov_umts, vnmap1, year_range, proj_crs) {
  
  # Filter by year range
  prov_filtered <- prov_umts %>%
    filter(year_created <= year_range[1] & year_lastseen >= year_range[2])
  
  # Apply buffer using individual range per tower
  prov_filtered <- prov_filtered %>%
    mutate(coverage_area = map2(geometry, range, ~ st_make_valid(st_buffer(.x, dist = as.numeric(.y)))))
  
  # Convert list to sf and merge overlapping coverage areas
  coverage_sf <- st_sf(geometry = st_sfc(prov_filtered$coverage_area, crs = proj_crs))
  
  merged_coverage <- coverage_sf %>%
    st_union() %>%
    st_sf(geometry = .) %>%
    st_make_valid()
  
  # Intersect with province boundaries and calculate coverage area
  prov_cov <- st_intersection(vnmap1, merged_coverage) %>%
    mutate(coverage_m2 = st_area(geometry)) %>% 
    st_drop_geometry() %>% 
    select(VARNAME_1, coverage_m2)
  
  return(prov_cov)
}

prov_cov10 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2010), proj_crs) %>% st_drop_geometry()
prov_cov11 <- compute_3G_coverage(prov_umts, vnmap1, c(2011, 2011), proj_crs) %>% st_drop_geometry()
prov_cov12 <- compute_3G_coverage(prov_umts, vnmap1, c(2012, 2012), proj_crs) %>% st_drop_geometry()
prov_cov13 <- compute_3G_coverage(prov_umts, vnmap1, c(2013, 2013), proj_crs) %>% st_drop_geometry()
prov_cov14 <- compute_3G_coverage(prov_umts, vnmap1, c(2014, 2014), proj_crs) %>% st_drop_geometry()
prov_cov15 <- compute_3G_coverage(prov_umts, vnmap1, c(2015, 2015), proj_crs) %>% st_drop_geometry()
prov_cov16 <- compute_3G_coverage(prov_umts, vnmap1, c(2016, 2016), proj_crs) %>% st_drop_geometry()
prov_cov17 <- compute_3G_coverage(prov_umts, vnmap1, c(2017, 2017), proj_crs) %>% st_drop_geometry()
prov_cov18 <- compute_3G_coverage(prov_umts, vnmap1, c(2018, 2018), proj_crs) %>% st_drop_geometry()
prov_cov19 <- compute_3G_coverage(prov_umts, vnmap1, c(2019, 2019), proj_crs) %>% st_drop_geometry()
prov_cov20 <- compute_3G_coverage(prov_umts, vnmap1, c(2020, 2020), proj_crs) %>% st_drop_geometry()

prov_area <- vnmap1 %>%
  mutate(prov_area = st_area(geometry)) %>%
  st_drop_geometry() %>% 
  select(VARNAME_1, prov_area)

prov_covfn <- function(i){
  i %>% 
    right_join(prov_area, by = "VARNAME_1") %>% 
    mutate(
      coverage_m2 = as.numeric(set_units(coverage_m2, "m^2", mode = "standard")),
      prov_area = as.numeric(set_units(prov_area, "m^2", mode = "standard")),
      coverage_share = coverage_m2 / prov_area
    ) %>% 
    select(VARNAME_1, coverage_share) %>% 
    left_join(vnmap1, by = "VARNAME_1") %>% 
    st_as_sf()
}

prov_cov10 <- prov_cov10 %>% prov_covfn()
prov_cov11 <- prov_cov11 %>% prov_covfn()
prov_cov12 <- prov_cov12 %>% prov_covfn()
prov_cov13 <- prov_cov13 %>% prov_covfn()
prov_cov14 <- prov_cov14 %>% prov_covfn()
prov_cov15 <- prov_cov15 %>% prov_covfn()
prov_cov16 <- prov_cov16 %>% prov_covfn()
prov_cov17 <- prov_cov17 %>% prov_covfn()
prov_cov18 <- prov_cov18 %>% prov_covfn()
prov_cov19 <- prov_cov19 %>% prov_covfn()
prov_cov20 <- prov_cov20 %>% prov_covfn()
