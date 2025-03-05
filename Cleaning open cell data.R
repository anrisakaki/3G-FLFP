# 2G - GSM
# 3G - UMTS
# 4G - LTE

gdf <- st_as_sf(opencell, coords = c("lon", "lat"), crs = 4362)
gdf$lon <- st_coordinates(gdf)[, "X"]
gdf$lat <- st_coordinates(gdf)[, "Y"]

gdf$created_recode <- as.POSIXct(gdf$created, origin = "1970-01-01", tz = "UTC")

gdf$year <- year(gdf$created_recode)
gdf$month <- month(gdf$created_recode)
gdf$day <- day(gdf$created_recode)

gdf <- st_make_valid(gdf) 

sum_radio_yr <- gdf %>% 
  group_by(radio, year) %>% 
  summarise(n = n())
