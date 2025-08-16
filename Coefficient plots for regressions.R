plot_did <- function(data, file, point_size = 3, errorbar_width = 0.3, errorbar_thickness = 1.2, line_thickness = 1.2) {
  p <- ggplot(dplyr::filter(data, event_time > -12), aes(x = event_time, y = att, colour = model)) +
    geom_point(position = position_dodge(width = -0.4), size = point_size) +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
                  width = errorbar_width,
                  size = errorbar_thickness,
                  position = position_dodge(width = -0.4)) +
    scale_x_continuous(breaks = seq(min(data$event_time), max(data$event_time), by = 2)) +
    geom_vline(xintercept = -2, linetype = "dashed", size = line_thickness) +
    geom_hline(yintercept = 0, color = "black", size = line_thickness) +
    labs(title = "",
         x = "Time to treatment",
         y = "ATT",
         color = "Model") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    )
  
  ggsave(file, plot = p, width = 20, height = 20)
}

plot_did(work_did, "Results/work_did.jpeg")
plot_did(informal_did, "Results/informal_did.jpeg")
plot_did(agri_did, "Results/agri_did.jpeg")
plot_did(manu_did, "Results/manu_formal_did.jpeg")
plot_did(service_did, "Results/service_formal_did.jpeg")
