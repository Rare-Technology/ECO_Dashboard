plot_size <- function(data_filtered, sel_family, sel_geom) {
  
  data_aggreg <- data_filtered %>% 
    dplyr::filter(family %in% sel_family) %>% 
    aggregate_data(., 'sizeclass')

  if (sel_geom == "Bar plots") {
    data_summary <- summarySE(data_aggreg, 'density_ind_ha', for.size=TRUE)
    ggplot2::ggplot(data=data_summary,
                    aes(sizeclass, density_ind_ha),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_bar(aes(fill = location_status), position=position_dodge(),
            stat = 'identity', width=0.6) +
      ggtitle("Size class distribution")
  } else {
    data_local <- get_local_data(data_aggreg, 'density_ind_ha', for.size=TRUE)
    ggplot2::ggplot(data=data_local,
                    aes(sizeclass, group=location_status),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_density(aes(fill=location_status), size=0.3, alpha=0.5) +
      ggtitle(ggtitle("Size class distribution"))
  }
}