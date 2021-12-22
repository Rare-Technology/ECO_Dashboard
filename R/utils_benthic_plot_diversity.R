#' benthic_plot_diversity 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_diversity <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, 'attribute')
  if (sel_geom == 'Bar plots') {
    data_summary <- summarySE(data_aggreg, 'attribute')
    
    ggplot2::ggplot(data=data_summary,
                    aes(location_status, attribute),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_bar(aes(fill = location_status), position=position_dodge(),
               stat = 'identity') +
      geom_errorbar(aes(ymin=attribute - SE, ymax=attribute + SE),
                    position=position_dodge(), width=0.2, na.rm=TRUE) +
      ggtitle("Mean number of benthic attributes")
  } else {
    data_local <- data_aggreg # ONLY for diversity !
    ggplot2::ggplot(data = data_local,
                    aes(location_status, attribute),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_jitter(aes(fill=location_status), width=0.1, height=0, alpha=0.5, size=2) +
      stat_summary(aes(col=location_status), na.rm=TRUE, fun.data = "mean_se",
                   geom = "pointrange", size = .4, position=position_dodge(width=1)) +
      ggtitle("Mean number of benthic attribute")
  }
}