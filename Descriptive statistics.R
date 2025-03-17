
ggplot(dplyr::filter(sum_radio_cummulative_yr, radio == "GSM" | radio == "UMTS" | radio == "LTE"), aes(x = year_created, y = cumulative_n, color = radio, group = radio)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_radio_yr$year_created), max(sum_radio_yr$year_created), by = 1)) +
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
ggsave("Figures/radio_by_year.jpeg", width = 7, height = 7)

ggplot(prov_cov20_shp) + 
  geom_sf(aes(fill = coverage_share*100)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradient(name = "3G coverage (%)", low = "lightblue", high = "darkblue", na.value = "grey") +
  ggtitle(" ")
ggsave("Figures/prov_cov20.jpeg", width = 7, height = 7)

# By year 
ggplot(sum_vhlss, aes(x = year, y = flfp * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "FLFP (%)"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )

ggplot(sum_vhlss, aes(x = year, y = fwork2_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women with a second job (%)"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )

ggplot(sum_vhlss, aes(x = year, y = retail_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women in retail (%)"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )

# By age group 

ggplot(sum_age_vhlss, aes(x = year, y = fwork2_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women with second job (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/fwork2_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = retail_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women in retail (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/retailshare_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = retail2_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of women with second job in retail (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/retail2share_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = wholesale_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women in wholesale (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/wholesale_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = wholesale2_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of women with second job in wholesale (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/wholesale2_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = (retail_perc+wholesale_perc) * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of women in retail or wholesale (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/retail_wholesaleshare_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = flfp * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "FLFP (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/flfp_agegroup.jpeg", width = 7, height = 7)

ggplot(sum_age_vhlss, aes(x = year, y = agrihh_perc * 100, color = factor(age_group))) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_age_vhlss$year), max(sum_age_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women in household agriculture (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = 'black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    text = element_text(size = 10)
  )
ggsave("Figures/agrihh_agegroup.jpeg", width = 7, height = 7)