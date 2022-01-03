#' seagrass_plot_height 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_seagrass_height <- function(data_filtered, sel_geom) {
  ## TODO implement trends
  data_aggreg <- aggregate_data(data_filtered, "avg_height_cm")
  
  if (sel_geom == "Bar plots") {
    data_summary <- summarySE(data_aggreg, 'avg_height_cm')
    ggplot2::ggplot(data = data_summary,
                    aes(x = location_status, y = avg_height_cm),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_bar(
        aes(fill = location_status),
        position = position_dodge(),
        color = "black",
        stat = 'identity',
        width = 0.6,
        show.legend = FALSE) +
      geom_errorbar(
        aes(ymin = avg_height_cm - SE, ymax = avg_height_cm + SE),
        position = position_dodge(),
        width = 0.2,
        na.rm = TRUE) +
      ggtitle("Average seagrass height") +
      xlab("Location status") + 
      ylab("Height (cm)") +
      theme_rare() +
      theme(
        panel.grid.major.y = element_line(),
        panel.grid.major.x = element_blank()
      ) +
      scale_fill_manual(values = c(RARE_COLORS$lightblue, RARE_COLORS$lightgreen))
  } else {
    ggplot2::ggplot(data = data_filtered,
                    aes(x = avg_height_cm, fill = location_status),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_histogram(
        position = "identity",
        alpha = 0.7,
        color = "black") +
      ggtitle("Average seagrass height distribution") +
      ylab("Count") +
      xlab("Height (cm)") +
      labs(fill = "") +
      theme_rare() +
      theme(
        panel.grid.major.y = element_line()
      ) +
      scale_fill_manual(values = c(RARE_COLORS$blue, RARE_COLORS$red))
  }
}