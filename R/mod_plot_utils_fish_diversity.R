plot_fish_diversity <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, 'species')
  data_summary <- summarySE(data_aggreg, 'species', facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (length(years) == 1) {
    p <- plot_bar(
      data = data_summary,
      x = "location_status",
      y = "species",
      fill = "location_status",
      title = "Fish diversity,",
      year = years,
      y_label = "Number of unique species"
    )
    
    if (sel_geom == "Distribution plots") {
      if (facet_maa) {
        data_local <- data_aggreg # diversity is a special case for this
      } else {
        # this feels a bit sneaky but it should work: pretend we're faceting maa so
        # calculate data_summary in that case BUT call it data_local
        data_local <- summarySE(data_aggreg, 'species', !facet_maa) # note the !
      }
      
      out$data <- data_local
      
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
      title = "Fish diversity",
      x_label = "Year",
      y_label = "Number of unique fish species",
      years = years
    )
    
    if (sel_geom == "Distribution plots") {
      if (facet_maa) {
        data_local <- data_aggreg
      } else {
        data_local <- summarySE(data_aggreg, 'species', !facet_maa)
      }
      
      out$data <- data_local
      
      p <- p + plot_samples(
        data_local = data_local,
        x = "year",
        y = "species",
        fill = "location_status"
      )
    }
  }
  
  out$plot <- p
  out
}