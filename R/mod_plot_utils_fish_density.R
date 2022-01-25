plot_fish_density <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, 'density_ind_ha')
  data_summary <- summarySE(data_aggreg, "density_ind_ha", facet_maa)
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "density_ind_ha",
      fill = "location_status",
      title = "Fish number density,",
      year = years,
      y_label = "Number density (individuals/ha)"
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "density_ind_ha", facet_maa)
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "location_status",
        y = "density_ind_ha",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "density_ind_ha",
      fill = "location_status",
      title = "Fish number density trend",
      x_label = "Year",
      y_label = "Number density (individuals/ha)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "density_ind_ha", facet_maa)
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "year",
        y = "density_ind_ha",
        fill = "location_status"
      )
    }
  }
  
  p
}