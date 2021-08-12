plot_density <- function(data_filtered, sel_family) {
  data_aggreg <- data_filtered %>% 
                  dplyr::filter(family %in% sel_family) %>% 
                  aggregate_data(., 'density_ind_ha')
  data_summary <- summarySE(data_aggreg, 'density_ind_ha')
 
  ggplot2::ggplot(data=data_summary,
                  aes(location_status, density_ind_ha),
                  na.rm = TRUE) +
    facet_wrap('ma_name') +
    geom_bar(aes(fill = location_status), position=position_dodge(),
             stat = 'identity') +
    geom_errorbar(aes(ymin=density_ind_ha - SE, ymax=density_ind_ha + SE),
                  position=position_dodge(), width=0.2, na.rm=TRUE) +
    ggtitle("Mean Fish Density")
}