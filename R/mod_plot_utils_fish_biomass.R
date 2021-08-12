plot_biomass <- function(data_filtered, sel_family) {
  data_aggreg <- data_filtered %>% 
                  dplyr::filter(family %in% sel_family) %>% 
                  aggregate_data(., 'biomass_kg_ha')
  data_summary <- summarySE(data_aggreg, 'biomass_kg_ha')

  ggplot2::ggplot(data=data_summary,
                  aes(location_status, biomass_kg_ha),
                  na.rm = TRUE) +
    facet_wrap('ma_name') +
    geom_bar(aes(fill = location_status), position=position_dodge(),
             stat = 'identity') +
    geom_errorbar(aes(ymin=biomass_kg_ha - SE, ymax=biomass_kg_ha + SE),
                  position=position_dodge(), width=0.2, na.rm=TRUE) +
    ggtitle("Mean Fish Biomass")
}