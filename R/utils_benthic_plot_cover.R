#' benthic_plot_cover 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_cover <- function(data_filtered, sel_geom, facet_maa) {
  ### Plot coral reef categories in a bar plot, using maa AND MA/R status as facet variables
  df <- aggregate_data(data_filtered, "percentage")
  
  # To make the plot work, we need to make sure there's a value recorded for every #category in df;
  # the way the original dataset works is that if there was a certain category that had no coverage,
  # it would simply not appear rather than explicitly saying 0%.
  # Here, we will add 0%'s as necessary
  all_categories <- unique(df$category)
  df <- df %>% 
    tidyr::pivot_wider(names_from = category, values_from = percentage)
  df[is.na(df)] <- 0
  df <- df %>% 
    tidyr::pivot_longer(cols = all_categories, names_to = "category", values_to = "percentage") %>% 
    summarySE(., "percentage", facet_maa)
  
  out <- list(data = df)
  
  # Convert year to character so it is treated as a discrete variable
  df$year <- as.character(df$year)
  
  grad <- colorRampPalette(c(RARE_COLORS$lightblue, RARE_COLORS$red), bias=0.5, space="rgb")
  grad <- grad(length(unique(df$year)))
  
  p <- ggplot(df) +
    geom_col(
      aes(y=percentage, x=category, fill=year),
      color="black",
      position="dodge"
    ) +
    geom_errorbar(
      aes(
        x = category,
        fill = year,
        ymin = ymin,
        ymax = ymax
      ),
      position = position_dodge(0.9),
      width = 0.2,
      na.rm = TRUE
    ) +
    scale_x_discrete(limits = rev) +
    ggtitle("Coral reef coverage") +
    labs(
      x="",
      y="Cover (%)",
      fill=""
    ) +
    theme_rare() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_line(),
      panel.grid.major.y = element_line(),
      axis.text.x = element_text(hjust=1, angle=30)
    ) +
    scale_fill_manual(values = grad)

  
  out$plot <- p
  out
}