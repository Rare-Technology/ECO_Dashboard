#' benthic_plot_cover 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_cover <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, 'percentage')
    data_summary <- data_aggreg
    
    ggplot2::ggplot(data = data_summary,
                    aes(x = category, y = location_status, fill = percentage)) +
      facet_wrap("ma_name") +
      geom_tile() +
      geom_text(aes(label = round(percentage,1), color = round(percentage,1) >= 50),
                show.legend = c(TRUE, FALSE)) +
      scale_fill_viridis_c(option = "magma") +
      scale_color_manual(values = c("white", "black")) +
      labs(title = "Benthic area cover composition (%)") +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank()
      )
}