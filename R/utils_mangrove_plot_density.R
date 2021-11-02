#' mangrove_plot_density 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_sapling_tree_density <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, "sapling_tree_density_ind_m2")
  
  if (sel_geom == "Bar plots") {
    data_summary <- summarySE(data_aggreg, "sapling_tree_density_ind_m2")
    
    ggplot2::ggplot(data = data_summary,
                    aes(location_status, sapling_tree_density_ind_m2),
                    na.rm = TRUE) +
      facet_wrap("ma_name") +
      geom_bar(aes(fill = location_status), position = position_dodge(),
               stat = "identity") +
      geom_errorbar(aes(ymin = sapling_tree_density_ind_m2 - SE,
                        ymax = sapling_tree_density_ind_m2 + SE),
                    position = position_dodge(), width = 0.2, na.rm = TRUE) +
      ggtitle("Mean Sapling Number Density")
  }
}