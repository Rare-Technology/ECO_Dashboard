#' mangrove_plot_density 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_sapling_tree_density <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, "sapling_tree_density_ind_m2")
  data_summary <- summarySE(data_aggreg, "sapling_tree_density_ind_m2")
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "sapling_tree_density_ind_m2",
      fill = "location_status",
      title = "Sapling number density,",
      year = years,
      y_label = expression("Number density (individuals/m"^2*")")
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'sapling_tree_density_ind_m2')
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "sapling_tree_density_ind_m2",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "sapling_tree_density_ind_m2",
      fill = "location_status",
      title = "Sapling number density trends",
      x_label = "Year",
      y_label = expression("Number density (individuals/m"^2*")"),
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "sapling_tree_density_ind_m2")
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "sapling_tree_density_ind_m2",
        fill = "location_status"
      )
    }
  }
  
  p
}