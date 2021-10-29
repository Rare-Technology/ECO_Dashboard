#' mangrove_plot_size 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_tree_size <- function(data_filtered, sel_geom) {
  data_aggreg <- aggregate_data(data_filtered, "dbh_cm")
  
  if (sel_geom == "Bar plots") {
    data_summary <- summarySE(data_aggreg, "dbh_cm")
    ggplot2::ggplot(data = data_summary,
                    aes(location_status, dbh_cm),
                    na.rm = TRUE) +
      facet_wrap("ma_name") +
      geom_bar(aes(fill = location_status), position = position_dodge(),
               stat = "identity") +
      geom_errorbar(aes(ymin = dbh_cm - SE, ymax = dbh_cm + SE),
                    position = position_dodge(), width = 0.2, na.rm = TRUE) +
      ggtitle("Mean tree diameter (cm)")
  } else if (sel_geom == "Distribution plots") {
    data_local <- get_local_data(data_aggreg, 'dbh_cm')
    ggplot2::ggplot(data=data_local,
                    aes(location_status, dbh_cm),
                    na.rm = TRUE) +
      facet_wrap('ma_name') +
      geom_jitter(aes(fill=location_status), width=0.1, height=0, alpha=0.5, size=2) +
      stat_summary(aes(col=location_status), na.rm=TRUE, fun.data = "mean_se",
                   geom = "pointrange", size = .4, position=position_dodge(width=1)) +
      ggtitle("Mean Tree Diameter (cm)")
  }
}