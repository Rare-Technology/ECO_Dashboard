plot_biomass <- function(data_filtered, sel_year, sel_family, sel_geom) {
  data_aggreg <- data_filtered %>% 
    dplyr::filter(family %in% sel_family) %>% 
    dplyr::filter(year == sel_year) %>% 
    aggregate_data(., 'biomass_kg_ha')

  if (sel_geom == 'Bar plots') {
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
  } else if (sel_geom == "Distribution plots") {
    data_local <- get_local_data(data_aggreg, 'biomass_kg_ha')
    ggplot2::ggplot(data=data_local,
                    aes(location_status, biomass_kg_ha),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_jitter(aes(fill=location_status), width=0.1, height=0, alpha=0.5, size=2) +
      stat_summary(aes(col=location_status), na.rm=TRUE, fun.data = "mean_se",
                   geom = "pointrange", size = .4, position=position_dodge(width=1)) +
      ggtitle("Mean Fish Biomass")
  }
}