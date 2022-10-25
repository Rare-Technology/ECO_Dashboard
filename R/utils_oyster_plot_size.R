#' oyster_plot_size 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_oyster_size <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, "length_mm")
  data_summary <- summarySE(data_aggreg, "length_mm", facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "length_mm",
      fill = "location_status",
      title = "Oyster size,",
      year = years,
      y_label = expression("Length (mm)")
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'length_mm', facet_maa)
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "length_mm",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "length_mm",
      fill = "location_status",
      title = "Oyster size",
      x_label = "Year",
      y_label = "Length (mm)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "length_mm", facet_maa)
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "length_mm",
        fill = "location_status"
      )
    }
  }
  
  out$plot <- p
  out
}