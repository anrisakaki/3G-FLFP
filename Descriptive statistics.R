
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

ggplot(female_sum_vhlss, aes(x = year, y = flfp * 100)) +
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

ggplot(mothers_sum_vhlss, aes(x = year, y = flfp * 100)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(mothers_sum_vhlss$year), max(mothers_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of mothers in the labour force (%)"
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
ggsave("Figures/mothers_lfp.jpeg", width = 7, height = 7)

ggplot(mothers_u7_sum_vhlss, aes(x = year, y = flfp * 100)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(mothers_u7_sum_vhlss$year), max(mothers_u7_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of mothers with children \nunder 7 in the labour force (%)"
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
ggsave("Figures/mothersu7_lfp.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = mlfp * 100)) +
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

ggplot(female_sum_vhlss, aes(x = year, y = informal_perc * 100)) +
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
ggsave("Figures/f_informal_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = informal_perc * 100)) +
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
ggsave("Figures/m_informal_perc.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = nonagri_informal_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(female_sum_vhlss$year), max(female_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers working informally (exc. agriculture) (%)"
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
ggsave("Figures/f_nonagrihh_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = nonagri_informal_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(male_sum_vhlss$year), max(male_sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers working informally (exc. agriculture) (%)"
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
ggsave("Figures/m_nonagrihh_perc.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = agri_informal_perc * 100)) +
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
ggsave("Figures/f_agrihh_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = agri_informal_perc * 100)) +
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
ggsave("Figures/m_agrihh_perc.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = manu_informal_perc * 100)) +
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
ggsave("Figures/f_manuhh_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = manu_informal_perc * 100)) +
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
ggsave("Figures/m_manuhh_perc.jpeg", width = 7, height = 7)

ggplot(female_sum_vhlss, aes(x = year, y = service_informal_perc * 100)) +
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
ggsave("Figures/f_servicehh_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = service_informal_perc * 100)) +
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
ggsave("Figures/m_servicehh_perc.jpeg", width = 7, height = 7)
