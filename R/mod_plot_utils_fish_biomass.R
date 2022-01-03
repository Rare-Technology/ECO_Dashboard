plot_fish_biomass <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, 'biomass_kg_ha')
  data_summary <- summarySE(data_aggreg, 'biomass_kg_ha')
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "biomass_kg_ha",
      fill = "location_status",
      title = "Biomass density,",
      year = years,
      y_label = "Biomass density (kg/ha)"
    )
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, 'biomass_kg_ha')
      
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
      title = "Biomass density trends",
      x_label = "Year",
      y_label = "Biomass density (kg/ha)",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- get_local_data(data_aggreg, "biomass_kg_ha")
      
      p <- p + plot_samples(
        data = data_local,
        x = "year",
        y = "biomass_kg_ha",
        fill = "location_status"
      )
    }
  }
  
  p
}