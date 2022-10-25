#' crab_plot_density 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_crab_density <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, "crab_density_ind_ha")
  data_summary <- summarySE(data_aggreg, "crab_density_ind_ha", facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "crab_density_ind_ha",
      fill = "location_status",
      title = "Crab density,",
      year = years,
      y_label = expression("Density (individuals/ha)")
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'crab_density_ind_ha', facet_maa)
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "crab_density_ind_ha",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "crab_density_ind_ha",
      fill = "location_status",
      title = "Crab density",
      x_label = "Year",
      y_label = "Density (individuals/ha)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "crab_density_ind_ha", facet_maa)
      out$data <- data_local
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "year",
        y = "crab_density_ind_ha",
        fill = "location_status"
      )
    }
  }
  
  out$plot <- p
  out
}