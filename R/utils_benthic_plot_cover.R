#' benthic_plot_cover 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_cover <- function(data_filtered, sel_geom, facet_maa) {
  ## TODO figure out how to represent trends here
  ## TODO figure out how to aggregate across maa's. currently, just removing the
  # facet_wrap line in this file doesn't work; the number labels overlap on top of each other
  ## TODO update `out` when you do the above
  
  data_aggreg <- aggregate_data(data_filtered, 'percentage')
  data_summary <- data_aggreg
  
  data_summary$percentage_ma[data_summary$location_status == 'Managed Access'] <- data_summary %>% 
    dplyr::filter(location_status == 'Managed Access') %>% 
    dplyr::pull(percentage)
  
  out <- list(data = data_summary)
  
  
  ### WIP cleveland plots, restrict to one MA at a time
  # p <- ggplot2::ggplot(data = data_summary) +
  #   geom_segment(aes(
  #     x = category, xend = category, y = percentage_ma, yend = percentage_r
  #   )) +
  #   geom_point(aes(
  #     x = category, y = percentage_ma
  #   ), color = 'red') +
  #   geom_point(aes(
  #     x = category, y = percentage_r
  #   ), color = 'blue') +
  #   coord_flip() +
  #   facet_wrap("year") +
  #   labs(title = "Benthic area cover composition (%)") +
  #   theme_rare() +
  #   theme(
  #     # axis.text.x = element_text(angle = 30, hjust = 1),
  #     panel.grid.major.x = element_blank(),
  #     axis.title.y = element_blank()
  #   )
    
  p <- ggplot2::ggplot(data = data_summary,
                  aes(x = category, y = location_status, fill = percentage)) +
    facet_wrap("ma_name") +
    geom_tile() +
    geom_text(aes(label = round(percentage,1), color = round(percentage,1) >= 50),
              show.legend = c(TRUE, FALSE)) +
    scale_fill_viridis_c(option = "magma") +
    scale_color_manual(values = c("white", "black")) +
    labs(title = "Benthic area cover composition (%)") +
    theme_rare() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.grid.major.x = element_blank(),
      axis.title.y = element_blank()
    )
  
  out$plot <- p
  out
}