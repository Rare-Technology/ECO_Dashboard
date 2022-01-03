#' mangrove_plot_size 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_tree_size <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, "dbh_cm")
  data_summary <- summarySE(data_aggreg, "dbh_cm")
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "dbh_cm",
      fill = "location_status",
      title = "Tree diameter,",
      year = years,
      y_label = "Diameter (cm)"
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'dbh_cm')
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "dbh_cm",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "dbh_cm",
      fill = "location_status",
      title = "Tree diameter trends",
      x_label = "Year",
      y_label = "Diameter (cm)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "dbh_cm")
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "dbh_cm",
        fill = "location_status"
      )
    }
  }
  
  p
}