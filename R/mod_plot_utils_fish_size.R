plot_size <- function(data_filtered, sel_family) {
  data_aggreg <- data_filtered %>% 
    dplyr::filter(family %in% sel_family) %>% 
    aggregate_data(., 'sizeclass')
  data_summary <- summarySE(data_aggreg, 'density_ind_ha', for.size=TRUE)
  
  ggplot2::ggplot(data=data_summary,
                  aes(sizeclass, density_ind_ha),
                  na.rm = TRUE) +
    facet_wrap('ma_name') +
    geom_bar(aes(fill = location_status), position=position_dodge(),
             stat = 'identity', width=0.6) +
    ggtitle("Size class distribution")
}