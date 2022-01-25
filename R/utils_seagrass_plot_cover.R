#' seagrass_plot_cover 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import stringr
#' @import ggplot2
plot_seagrass_cover <- function(data_filtered, sel_geom, facet_maa) {
  ## TODO implement trends
  ## TODO same problem as benthic cover: make the maa aggregating work properly
  data_aggreg <- aggregate_data(data_filtered, 'cover')
  data_summary <- data_aggreg
  
  ggplot2::ggplot(data = data_summary,
                aes(x = seagrass_species,
                    y = stringr::str_wrap(location_status, 5),
                    fill = cover)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(cover,1), color = round(cover,1) >= 50),
              show.legend = c(TRUE, FALSE)) +
    scale_fill_viridis_c(option = "magma") +
    scale_color_manual(values = c("white", "black")) +
    labs(
      title = "Seagrass area cover (%)",
      fill = "Cover"
    ) +
    xlab("Seagrass species") +
    theme_rare() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      axis.title.y = element_blank()
    )
}