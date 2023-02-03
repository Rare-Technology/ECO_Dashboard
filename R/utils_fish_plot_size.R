plot_fish_size <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, "length")
  data_summary <- summarySE(data_aggreg, "length", facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  if (sel_geom == "Bar plots") {
    p <- plot_bar(
      data = data_summary,
      x = "year",
      y = "length",
      fill = "location_status",
      title = "Average Fish length",
      years = years,
      y_label = "Length (cm)"
    )
  } else if (sel_geom == "Distribution plots") {
    data_filtered$year <- as.character(data_filtered$year)
    years <- as.character(years)
    data_filtered$year <- factor(data_filtered$year, levels = years)
    
    out$data <- data_filtered
    
    p <- plot_density(
      data = data_filtered,
      x = "length",
      y = "year",
      title = "Distribution of fish length",
      x_label = "Length (cm)",
      y_label = "Year"
    )
  }
  
  out$plot <- p
  out
}