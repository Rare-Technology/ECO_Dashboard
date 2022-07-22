plot_fish_biomass <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, 'biomass_kg_ha')
  data_summary <- summarySE(data_aggreg, 'biomass_kg_ha', facet_maa)
  years <- sort(unique(data_summary$year))
  
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "biomass_kg_ha",
      fill = "location_status",
      title = "Fish biomass,",
      year = years,
      y_label = "Biomass (kg/ha)"
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'biomass_kg_ha', facet_maa)
      out$data <- data_local
      
      p <- p + plot_samples(
        data =  data_local,
        x = "location_status",
        y = "biomass_kg_ha",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
    
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "biomass_kg_ha",
      fill = "location_status",
      title = "Fish biomass",
      x_label = "Year",
      y_label = "Biomass (kg/ha)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "biomass_kg_ha", facet_maa)
      out$data <- data_local
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "biomass_kg_ha",
        fill = "location_status"
      )
    }
  }
  
  out$plot <- p
  out
}