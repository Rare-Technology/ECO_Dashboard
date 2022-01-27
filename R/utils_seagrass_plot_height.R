#' seagrass_plot_height 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_seagrass_height <- function(data_filtered, sel_geom, facet_maa) {
  ## TODO once data is available for more than one year, double check that the
  # trend plots work
  data_aggreg <- aggregate_data(data_filtered, "avg_height_cm")
  data_summary <- summarySE(data_aggreg, 'avg_height_cm', facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "avg_height_cm",
      fill = "location_status",
      title = "Average seagrass height,",
      year = years,
      y_label = "Height (cm)"
    )
    
    if (sel_geom == "Distribution plots") {
      out$data <- data_filtered
      
      p <- plot_histogram(
        data_full =  data_filtered,
        data_summary = data_summary,
        x = "avg_height_cm",
        year = years,
        title = "Distribution of seagrass heights,",
        x_label = "Height (cm)"
      )
    }
  } else {
    if (sel_geom == "Bar plots") {
      p <- plot_trend(
        data = data_summary,
        x = "year",
        y = "avg_height_cm",
        title = "Average seagrass height trends",
        y_label = "Height (cm)",
        years = years
      )
    } else if (sel_geom == "Distribution plots") {
      data_filtered$year <- as.character(data_filtered$year)
      years <- as.character(years)
      data_filtered$year <- factor(data_filtered$year, levels = years)
      
      out$data <- data_filtered
      
      p <- plot_histogram_trend(
        data = data_filtered,
        x = "avg_height_cm",
        y = "year",
        title = "Distribution of seagrass height trends",
        x_label = "Height (cm)",
        y_label = "Year"
      )
    }
  }

  out$plot <- p
  out
}