# 2G - GSM
# 3G - UMTS
# 4G - LTE

proj_crs <- st_crs(vnmap1)

gdf <- st_as_sf(opencell, coords = c("lon", "lat"), crs = 4326)
gdf$lon <- st_coordinates(gdf)[, "X"]
gdf$lat <- st_coordinates(gdf)[, "Y"]

gdf$created_recode <- as.POSIXct(gdf$created, origin = "1970-01-01", tz = "UTC")

gdf$year <- year(gdf$created_recode)
gdf$month <- month(gdf$created_recode)
gdf$day <- day(gdf$created_recode)

gdf <- st_transform(gdf, proj_crs)

gdf <- st_make_valid(gdf) 

# Calculating share of province that is covered by 3G
prov_umts <- gdf %>% filter(radio == "UMTS")
prov_umts <- st_transform(prov_umts, proj_crs)

compute_3G_coverage <- function(prov_umts, vnmap1, year_range, proj_crs) {

  prov_filtered <- prov_umts %>%
    filter(year >= year_range[1] & year <= year_range[2])
  
  prov_filtered <- prov_filtered %>%
    mutate(coverage_area = st_make_valid(st_buffer(st_geometry(prov_filtered), dist = range)))
  
  merged_coverage <- st_union(prov_filtered$coverage_area)
  
  merged_coverage <- st_sf(geometry = merged_coverage, crs = proj_crs) %>%
    st_make_valid()
  
  vnmap1 <- st_make_valid(vnmap1)
  
  prov_cov <- st_intersection(vnmap1, merged_coverage) %>%
    mutate(coverage_m2 = st_area(geometry)) %>% 
    st_drop_geometry() %>% 
    select(VARNAME_1, coverage_m2)
  
  return(prov_cov)
}

prov_cov10 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2010), proj_crs)
save(prov_cov10, file = "prov_cov10.Rda")

prov_cov11 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2011), proj_crs) %>% st_drop_geometry()
save(prov_cov11, file = "prov_cov11.Rda")

prov_cov12 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2012), proj_crs) %>% st_drop_geometry()
save(prov_cov12, file = "prov_cov12.Rda")

prov_cov13 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2013), proj_crs) %>% st_drop_geometry()
save(prov_cov13, file = "prov_cov13.Rda")

prov_cov14 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2014), proj_crs) %>% st_drop_geometry()
save(prov_cov14, file = "prov_cov14.Rda")

prov_cov15 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2015), proj_crs) %>% st_drop_geometry()
save(prov_cov15, file = "prov_cov15.Rda")

prov_cov16 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2016), proj_crs) %>% st_drop_geometry()
save(prov_cov16, file = "prov_cov16.Rda")

prov_cov17 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2017), proj_crs) %>% st_drop_geometry()
save(prov_cov17, file = "prov_cov17.Rda")

prov_cov18 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2018), proj_crs) %>% st_drop_geometry()
save(prov_cov18, file = "prov_cov18.Rda")

prov_cov19 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2019), proj_crs) %>% st_drop_geometry()
save(prov_cov19, file = "prov_cov19.Rda")

prov_cov20 <- compute_3G_coverage(prov_umts, vnmap1, c(2010, 2020), proj_crs) %>% st_drop_geometry()
save(prov_cov20, file = "prov_cov20.Rda")

prov3g <- c("prov_cov10.Rda", "prov_cov11.Rda", "prov_cov12.Rda", "prov_cov13.Rda", "prov_cov14.Rda", "prov_cov15.Rda",
            "prov_cov16.Rda", "prov_cov17.Rda", "prov_cov18.Rda", "prov_cov19.Rda", "prov_cov20.Rda")

for (i in prov3g) {
  load(i)
}

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
    right_join(vnmap1, by = "VARNAME_1")
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
