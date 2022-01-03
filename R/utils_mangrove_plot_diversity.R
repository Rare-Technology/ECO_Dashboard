#' mangrove_plot_diversity 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_tree_diversity <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, 'tree_species')
  data_summary <- summarySE(data_aggreg, 'tree_species')
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "tree_species",
      fill = "location_status",
      title = "Mangrove diversity,",
      year = years,
      y_label = "Number of unique tree species"
    )
    if (sel_geom == "Distribution plots") {
      data_local <- data_aggreg
      
      p <- p + plot_samples(
        data = data_local,
        x = "location_status",
        y = "tree_species",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "tree_species",
      fill = "location_status",
      title = "Mangrove diversity trends",
      x_label = "Year",
      y_label = "Number of unique tree species",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- data_aggreg
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "tree_species",
        fill = "location_status"
      )
    }
  }
  
  p
}