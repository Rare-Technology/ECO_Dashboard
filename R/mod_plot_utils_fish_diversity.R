plot_diversity <- function(data_filtered, sel_family) {
  data_aggreg <- data_filtered %>% 
    dplyr::filter(family %in% sel_family) %>% 
    aggregate_data(., 'species')
  data_summary <- summarySE(data_aggreg, 'species')
  
  ggplot2::ggplot(data=data_summary,
                  aes(location_status, species),
                  na.rm = TRUE) +
    facet_wrap('ma_name') +
    geom_bar(aes(fill = location_status), position=position_dodge(),
             stat = 'identity') +
    geom_errorbar(aes(ymin=species - SE, ymax=species + SE),
                  position=position_dodge(), width=0.2, na.rm=TRUE) +
    ggtitle("Mean Number of Species")
}