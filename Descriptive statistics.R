
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

ggplot(dist_cov10_shp) + 
  geom_sf(aes(fill = coverage_share*100)) +
  geom_sf(data = vnmap1, fill = NA, color = "black", size = 80) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradient(name = "3G coverage (%)", low = "lightyellow", high = "darkblue", na.value = "grey") +
  ggtitle(" ")
ggsave("Figures/dist_cov10.jpeg", width = 7, height = 14)

# Devices 
ggplot(devices_long, aes(x = year, y = share*100, colour = device)) +
  geom_line(size = 1.2) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(devices_sum$year), max(devices_sum$year), by = 2)) +
  labs(
    x = "Year",
    y = "Share of household with at least 1 device (%)",
    color = "Device"
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
ggsave("Figures/devices.jpeg", width = 7, height = 7)

# Labour market outcomes  
ggplot(female_sum_vhlss, aes(x = year, y = work * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
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
ggsave("Figures/flfp.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = work * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "MLFP (%)"
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
ggsave("Figures/mlfp.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = informal * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers working informally (%)"
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
ggsave("Figures/f_informal.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = informal * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers working informally (%)"
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
ggsave("Figures/m_informal.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = nonagri_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers working informally \n(exc. agriculture) (%)"
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
ggsave("Figures/f_nonagrihh.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = nonagri_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers working informally \n(exc. agriculture) (%)"
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
ggsave("Figures/m_nonagrihh.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = agri_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers in informal agriculture (%)"
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
ggsave("Figures/f_agrihh.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = agri_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers in informal agriculture (%)"
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
ggsave("Figures/m_agrihh.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = manu_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers in informal manufacturing (%)"
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
ggsave("Figures/f_manuhh.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = manu_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers in informal manufacturing (%)"
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
ggsave("Figures/m_manuhh.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = service_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers in informal services (%)"
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
ggsave("Figures/f_servicehh.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = service_informal2 * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers in informal services (%)"
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
ggsave("Figures/m_servicehh.jpeg", width = 7, height = 7)
