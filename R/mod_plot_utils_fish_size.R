plot_size <- function(data_filtered, sel_family, sel_geom) {
  
  
  if (sel_geom == "Bar plots") {
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
  } else {
    data_local <- get_map_data(data_filtered, with.coords=FALSE)
    ggplot2::ggplot(data=data_local,
                    aes(location_status, species),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_jitter(aes(fill=location_status), width=0.05, alpha=0.5, size=2) +
      stat_summary(aes(col=location_status), na.rm=TRUE, fun.data = "mean_se",
                   geom = "pointrange", size = .4, position=position_dodge(width=1)) +
      ggtitle("Mean Number of Species")
  }
}