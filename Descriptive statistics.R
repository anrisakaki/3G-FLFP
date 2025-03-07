
ggplot(dplyr::filter(sum_radio_cummulative_yr, radio == "GSM" | radio == "UMTS" | radio == "LTE"), aes(x = year, y = cumulative_n, color = radio, group = radio)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_radio_yr$year), max(sum_radio_yr$year), by = 1)) +
  labs(title = "",
       x = "Year",
       y = "Cumulative No. of Towers",
       color = "") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        text = element_text(size=10))
ggsave("radio_by_year.png", width = 7, height = 7)