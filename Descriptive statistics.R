
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
ggsave("Figures/flfp.jpeg", width = 7, height = 7)

ggplot(sum_vhlss, aes(x = year, y = informal_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of female workers in informal sector (exc. agri) (%)"
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
ggsave("Figures/informal_perc.jpeg", width = 7, height = 7)

ggplot(male_sum_vhlss, aes(x = year, y = informal_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of male workers in informal sector (exc. agri) (%)"
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
ggsave("Figures/male_informal_perc.jpeg", width = 7, height = 7)

ggplot(sum_vhlss, aes(x = year, y = agrihh_perc * 100)) +
  geom_line(size = 1) + 
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(min(sum_vhlss$year), max(sum_vhlss$year), by = 2)) +
  labs(
    title = "",
    x = "Year",
    y = "Share of working women in informal agriculture (%)"
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
ggsave("Figures/agrihh_perc.jpeg", width = 7, height = 7)

# By year and age group 

ggplot(fsum_age_vhlss, aes(x = year, y = informal_perc * 100, color = factor(age_group))) +
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