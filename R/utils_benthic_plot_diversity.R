#' benthic_plot_diversity 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_diversity <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, 'family') # nested up to location_name
  data_summary <- summarySE(data_aggreg, 'family', facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  p <- plot_bar(
    data = data_summary,
    x = "year",
    y = "family",
    fill = "location_status",
    title = "Coral reef diversity",
    years = years,
    y_label = "Number of unique benthic families"
  )
  if (sel_geom == "Distribution plots") {
    if (facet_maa) {
      data_local <- data_aggreg
    } else {
      data_local <- summarySE(data_aggreg, 'family', !facet_maa)
    }
    
    out$data <- data_local
    
    p <- p + plot_samples(
      data = data_local,
      x = "year",
      y = "family",
      fill = "location_status"
    )
  }
  
  out$plot <- p
  out
}