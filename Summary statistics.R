sum_radio_yr <- gdf %>% 
  group_by(radio, year) %>% 
  filter(year > 1980) %>% 
  st_drop_geometry() %>% 
  summarise(n = n())

sum_radio_cummulative_yr <- gdf %>%
  group_by(radio, year) %>%
  filter(year > 1980) %>%
  st_drop_geometry() %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(radio, year) %>%
  group_by(radio) %>%
  mutate(cumulative_n = cumsum(n)) 

