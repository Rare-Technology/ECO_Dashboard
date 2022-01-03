plot_fish_diversity <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, 'species')
  data_summary <- summarySE(data_aggreg, 'species')
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "species",
      fill = "location_status",
      title = "Number of unique species,",
      year = years,
      y_label = "Species count"
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- data_aggreg # diversity is a special case for this
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "location_status",
        y = "species",
        fill = "location_status",
        shape = 16,
        point_size = 4
      )
    }
  } else {
    p <- plot_trend(
      data = data_summary,
      x = "year",
      y = "species",
      fill = "location_status",
      title = "Number of unique species trends",
      x_label = "Year",
      y_label = "Species count",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      data_local <- data_aggreg
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "year",
        y = "species",
        fill = "location_status"
      )
    }
  }
  
  p
}