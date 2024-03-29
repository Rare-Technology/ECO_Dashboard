#' crab_plot_size 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_crab_size <- function(data_filtered, sel_geom, facet_maa) {
  data_aggreg <- aggregate_data(data_filtered, "carapace_length_cm")
  data_summary <- summarySE(data_aggreg, "carapace_length_cm", facet_maa)
  years <- sort(unique(data_summary$year))
  out <- list(data = data_summary)
  
  p <- plot_bar(
    data = data_summary,
    x = "year",
    y = "carapace_length_cm",
    fill = "location_status",
    title = "Crab size",
    years = years,
    y_label = expression("Length (cm)")
  )
  if (sel_geom == "Distribution plots") {
    data_local <- get_local_data(data_aggreg, "carapace_length_cm", facet_maa)
    out$data <- data_local
    
    p <- p + plot_samples(
      data = data_local,
      x = "year",
      y = "carapace_length_cm",
      fill = "location_status"
    )
  }
  
  out$plot <- p
  out
}