plot_fish_size <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, "length")
  data_summary <- summarySE(data_aggreg, "length", for.size = TRUE)
  years <- sort(unique(data_summary$year))
  
  if (length(years) == 1) {
    if (sel_geom == "Bar plots") {
      p <- plot_bar(
        data = data_summary,
        x = "location_status",
        y = "length",
        fill = "location_status",
        title = "Average Fish length,",
        year = years,
        y_label = "Length (cm)"
      )
    } else if (sel_geom == "Distribution plots") {
      p <- plot_histogram(
        data_full =  data_filtered,
        data_summary = data_summary,
        x = "length",
        year = years,
        title = "Distribution of fish lengths,",
        x_label = "Length (cm)"
      )
    }
  } else {
    if (sel_geom == "Bar plots") {
      p <- plot_trend(
        data = data_summary,
        x = "year",
        y = "length",
        title = "Average fish length trends",
        y_label = "Length (cm)",
        years = years
      )
    } else if (sel_geom == "Distribution plots") {
        data_filtered$year <- as.character(data_filtered$year)
        years <- as.character(years)
        data_filtered$year <- factor(data_filtered$year, levels = years)
              
        p <- plot_histogram_trend(
          data = data_filtered,
          x = "length",
          y = "year",
          title = "Distribution of fish length trends",
          x_label = "Length (cm)",
          y_label = "Year"
        )
    }
  }
  
  p
}