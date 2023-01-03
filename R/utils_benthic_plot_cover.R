#' benthic_plot_cover 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_reef_cover <- function(data_filtered, sel_geom, facet_maa) {
  ### Plot coral reef categories similar to a population pyramid.
  # The left side is MA covers, the right side is reserve covers.
  # Additionally, different years are shown as adjacent bars.
  # Thus, you can read the year-to-year trend on one side of the plot, or
  # look at both sides to compare MA vs R.
  ## TODO figure out how to aggregate across maa's. currently, just removing the
  # facet_wrap line in this file doesn't work; the number labels overlap on top of each other
  ## TODO update `out` when you do the above
  df <- aggregate_data(data_filtered, "percentage")
  
  # To make the plot work, we need to make sure there's a value recorded for every
  # category in df; the way the original dataset works is that if there was a certain
  # category that had no coverage, it would simply not appear rather than explicitly
  # saying 0%.
  # Here, we will add 0%'s as necessary
  all_categories <- unique(df$category)
  df <- df %>% 
    tidyr::pivot_wider(names_from = category, values_from = percentage)
  df[is.na(df)] <- 0
  df <- df %>% 
    tidyr::pivot_longer(cols = all_categories, names_to = "category", values_to = "percentage")
  
  out <- list(data = df)
  
  # Convert MA numbers to negative, a hack to make the pyramid work
  df <- summarySE(df, "percentage", TRUE) %>% 
    dplyr::mutate(
      year = as.character(year),
      percentage = dplyr::case_when(
        location_status == "Managed Access" ~ -percentage,
        TRUE ~ percentage
      ),
      ymin = dplyr::case_when(
        location_status == "Managed Access" ~ -ymin,
        TRUE ~ ymin
      ),
      ymax = dplyr::case_when(
        location_status == "Managed Access" ~ -ymax,
        TRUE ~ ymax
      )
    )
  
  xmax <- max(abs(df$ymax))
  
  p <- ggplot(df) +
    geom_col(
      aes(x=percentage, y=category, fill=year),
      color="black",
      position="dodge"
    ) +
    geom_errorbar(
      aes(
        y = category,
        fill = year,
        xmin = ymin,
        xmax = ymax
      ),
      position = position_dodge(0.9),
      width = 0.2,
      na.rm = TRUE
    ) +
    geom_vline(xintercept=0) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(
      labels = abs,
      limits = xmax*c(-1.1, 1.1)
    )
  
  xbreaks <- ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  loc_break <- xbreaks[length(xbreaks)] / 2
  rm(p)
  grad <- colorRampPalette(c(RARE_COLORS$lightblue, RARE_COLORS$red), bias=0.5, space="rgb")
  grad <- grad(length(unique(df$year)))
  
  # Remake p to add the ticks for the location status; this doesn't work:
  # p <- p + ggplot2::scale_x_continuous(...)
  p <- ggplot(df) +
    geom_col(
      aes(x=percentage, y=category, fill=year),
      color="black",
      position="dodge"
    ) +
    geom_errorbar(
      aes(
        y = category,
        fill = year,
        xmin = ymin,
        xmax = ymax
      ),
      position = position_dodge(0.9),
      width = 0.2,
      na.rm = TRUE
    ) +
    geom_vline(xintercept=0) +
    scale_y_discrete(limits= rev) +
    scale_x_continuous(
      labels = abs,
      limits = xmax*c(-1.1, 1.1),
      sec.axis = ggplot2::sec_axis(
        ~ .,
        labels = c("Managed Access", "Reserve"),
        breaks = c(-loc_break, loc_break)
      )
    ) +
    ggtitle("Coral reef composition") +
    xlab("Cover (%)") +
    labs(
      fill = ""
    ) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.minor.x = element_line(),
      panel.grid.major.x = element_line()
    ) +
    scale_fill_manual(values = grad)
  
  out$plot <- p
  out
}