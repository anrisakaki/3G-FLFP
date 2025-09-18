vn_nat <- vect(vnmap_nat)
vn_dist <- vect(vnmap_dist)

vn_nat <- st_transform(vn_nat, crs(cb12))
vnmap2 <- st_transform(vnmap2, crs(cb12_vn))

cb_function <- function(i){
  i %>% 
    filter(!is.na(NAME_2)) %>%
    st_drop_geometry() %>%
    select(TYPE_2, NAME_2, NAME_1) %>%
    distinct()
}

cb12_vn <- crop(cb12, vn_nat)
cb12_vn <- mask(cb12_vn, vn_nat)
district_cov_12 <- terra::extract(
  cb12_vn,
  vect(vnmap2),
  fun = function(x) {
    covered <- sum(x == 1, na.rm = TRUE)   
    total   <- length(x[!is.na(x)])        
    if (total == 0) return(NA)
    return(covered / total)                
  }
)

cb12_vn_sf <- as.data.frame(cb12_vn, xy = T, na.rm = T) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2, join = st_intersects)
cb12 <- cb12_vn_sf %>% cb_function()

cb13_vn <- crop(cb13, vn_nat)
cb13_vn <- mask(cb13_vn, vn_nat)
cb13_vn_sf <- as.data.frame(cb13_vn, xy = T, na.rm = T) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2, join = st_intersects) 
cb13 <- cb13_vn_sf %>% cb_function()

cb14_vn <- crop(cb14, vn_nat)
cb14_vn <- mask(cb14_vn, vn_nat)
cb14_vn_sf <- as.data.frame(cb14_vn, xy = T) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2) 
cb14 <- cb14_vn_sf %>% cb_function()

cb15_vn <- crop(cb15, vn_nat)
cb15_vn <- mask(cb15_vn, vn_nat)
cb15_vn_sf <- as.data.frame(cb15_vn, xy = T) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2)
cb15 <- cb15_vn_sf %>% cb_function()

cb16_vn <- crop(cb16, vn_nat)
cb16_vn <- mask(cb16_vn, vn_nat)
cb16_vn_sf <- as.data.frame(cb16_vn, xy = T) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2)
cb16 <- cb16_vn_sf %>% cb_function()

cb17_vn <- crop(cb17, vn_nat)
cb17_vn <- mask(cb17_vn, vn_nat)
cb17_vn_sf <- as.data.frame(cb17_vn, xy = T) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2)
cb17 <- cb17_vn_sf %>% cb_function()

cb18_vn <- crop(cb18, vn_nat)
cb18_vn <- mask(cb18_vn, vn_nat)
cb18_vn_sf <- as.data.frame(cb18_vn, xy = T) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_join(vnmap2)
cb18 <- cb18_vn_sf %>% cb_function()
