#' benthic_plot_diversity 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_diversity <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, 'attribute') # nested up to location_name
  data_summary <- summarySE(data_aggreg, 'attribute', facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "attribute",
      fill = "location_status",
      title = "Benthic diversity,",
      year = years,
      y_label = "Number of benthic attributes"
    )
    if (sel_geom == "Distribution plots") {
      if (facet_maa) {
        data_local <- data_aggreg
      } else {
        data_local <- summarySE(data_aggreg, 'attribute', !facet_maa)
      }
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "attribute",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "attribute",
      fill = "location_status",
      title = "Benthic diversity trends",
      x_label = "Year",
      y_label = "Number of benthic attributes",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      if (facet_maa) {
        data_local <- data_aggreg
      } else {
        data_local <- summarySE(data_aggreg, 'attribute', !facet_maa)
      }
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "attribute",
        fill = "location_status"
      )
    }
  }
  
  out$plot <- p
  out
}