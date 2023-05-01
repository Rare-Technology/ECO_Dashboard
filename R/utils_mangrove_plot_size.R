#' mangrove_plot_size 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_mangrove_size <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, "dbh_cm")
  data_summary <- summarySE(data_aggreg, "dbh_cm", facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  p <- plot_bar(
    data = data_summary,
    x = "year",
    y = "dbh_cm",
    fill = "location_status",
    title = "Tree diameter",
    years = years,
    y_label = "Diameter (cm)"
  )
  if (sel_geom == "Distribution plots") {
    data_local <- get_local_data(data_aggreg, "dbh_cm", facet_maa)
    out$data <- data_local
    
    p <- p + plot_samples(
      data = data_local,
      x = "year",
      y = "dbh_cm",
      fill = "location_status"
    )
  }
  
  out$plot <- p
  out
}