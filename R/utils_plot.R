#' plot 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import ggplot2
#' @import ggridges
#' @import hrbrthemes
#' @import scales
theme_rare <- function(rotate_x = FALSE, subtitle_color = "black") {
  hrbrthemes::theme_ipsum_rc(
    axis_title_size = 16,
    axis_title_just = "cc",
    plot_margin = margin(5, 5, 5, 5),
    axis_text_size = 14
  ) +
  theme(
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 15, colour = subtitle_color),
    panel.grid.major = element_line(color = "grey10"),
    panel.grid.minor = element_line(color = "grey60"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 16, face = "bold")    
  )
}

RARE_COLORS <- list(
  "blue" = "#005BBB",
  "green" = "#008542",
  "grey" = "#5E6A71",
  "red" = "#AA1948",
  "lightblue" = "#00AFD8",
  "orange" = "#F58233",
  "lightgreen" = "#7AB800",
  "yellow" = "#EEAF00"
)

plot_trend <- function(data, x = "year", y = NULL, ymin = "ymin", ymax = "ymax",
  fill = "location_status", title = NULL, x_label = "Year", y_label = NULL,
  years = NULL) {
  
  p <- ggplot2::ggplot() +
    geom_errorbar(
      data = data,
      aes_string(
        x = x,
        fill = fill,
        ymin = ymin,
        ymax = ymax),
      position = position_dodge(width = 0.5),
      width = 0.2,
      na.rm = TRUE
    ) +
    geom_line(
      data = data,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      position = position_dodge(width = 0.5)
    ) +
    geom_point(
      data = data,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      shape = 21,
      size = 6,
      position = position_dodge(width = 0.5)
    ) +
    scale_x_continuous(breaks = years) +
    scale_fill_manual(values = c(RARE_COLORS$lightblue, RARE_COLORS$lightgreen)) +
    labs(
      title = title,
      fill = ""
    ) +
    xlab(x_label) + 
    ylab(y_label) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

plot_bar <- function(data, x = "year", y = NULL, ymin = "ymin", ymax = "ymax",
  fill = "location_status", title = NULL, years = NULL, x_label = "", y_label = NULL) {
  
  one_year <- ifelse(length(unique(data[[x]])) == 1, TRUE, FALSE)
  if (one_year) {
    data[[x]] <- as.factor(data[[x]])
  }
  ggplot2::ggplot() + list(
    geom_bar(
      data = data,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      position = position_dodge(0.6),
      color = "black",
      stat = 'identity',
      width = 0.6
    ),
    geom_errorbar(
      data = data,
      aes_string(
        x = x,
        fill = fill,
        ymin = ymin,
        ymax = ymax
      ),
      position = position_dodge(0.6),
      width = 0.3,
      na.rm = TRUE
    ),
    if (!one_year) {scale_x_continuous(breaks = years)},
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = "Location status" # look out for any fill != location_status
    ), 
    theme_rare(),
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_blank(),
    ),
    scale_y_continuous(labels = scales::comma),
    scale_fill_manual(values = c(RARE_COLORS$lightblue, RARE_COLORS$lightgreen))
  )
}

plot_samples <- function(data_local, x = NULL, y = NULL, fill = NULL,
  shape = 21, point_size = 2) {
  one_year <- ifelse(length(unique(data_local[[x]])) == 1, TRUE, FALSE)
  if (one_year) {
    data_local[[x]] <- as.factor(data_local[[x]])
  }
  
  geom_point(
      data = data_local,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      position = position_jitterdodge(
        jitter.width = 0.1,
        dodge.width = 0.6
      ),
      shape = shape,
      alpha = 1,
      size = point_size,
      show.legend = FALSE
  )
}

plot_histogram_deprecated <- function(data_full, data_summary, x = NULL, fill = "location_status",
  year = year, title = NULL, x_label = NULL) {
  ggplot2::ggplot() +
    geom_histogram(
      data = data_full,
      aes_string(
        x = x,
        fill = fill
      ),
      position = position_dodge(),
      binwidth = 1,
      alpha = 1
    ) +
    geom_vline(
      data = data_summary,
      aes_string(
        xintercept = x,
        color = fill,
        linetype = fill
      ),
      size = 0.5
    ) +
    ggtitle(paste(title, year)) +
    xlab(x_label) +
    labs(
      fill = ""
    ) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.minor.x = element_line(),
      panel.grid.major.x = element_blank()
    ) +
    scale_x_continuous(
      breaks = seq(0, max(data_full[[x]], na.rm = TRUE), by = 10),
      labels = seq(0, max(data_full[[x]], na.rm = TRUE), by = 10)
    ) +
    # guides(
    #   color = guide_legend(title = "Mean")
    # ) +
    scale_color_manual(values = c(RARE_COLORS$blue, RARE_COLORS$green), name = "Mean") +
    scale_linetype_manual(values = c("dashed", "dotted"), name = "Mean") +
    scale_fill_manual(values = c(RARE_COLORS$lightblue, RARE_COLORS$lightgreen))
}

plot_density <- function(data, x = NULL, y = NULL, fill = "location_status",
  title = NULL, x_label = NULL, y_label = NULL) {
  ggplot2::ggplot() +
    ggridges::geom_density_ridges(
      data = data,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      alpha = 0.7
    ) +
    ggtitle(title) +
    xlab(x_label) +
    ylab(y_label) +
    labs(fill = "") +
    theme_rare() +
    scale_fill_manual(values = c(RARE_COLORS$blue, RARE_COLORS$red))
}


display_filters <- function(rv) {
  out <- c(
    tr(rv, "Country"), paste(rv$sel_country), '\n',
    tr(rv, "Subnational unit"), paste(rv$sel_subnational, collapse=', '), '\n',
    tr(rv, "Local government unit"), paste(rv$sel_local, collapse=', '), '\n',
    tr(rv, "Managed access area"), paste(rv$sel_maa, collapse=', '), '\n'
  )
  
  if (rv$sel_dataset == 'Fish') { # add other cases as more dataset-specific filters are added
    out <- c(out, tr(rv, "Family"), paste(rv$sel_family), '\n')
  }
  
  out <- paste(out, collapse='\n')
  
  out
}

get_plot_height <- function(num_groups) {
  nrows <- ceiling(num_groups / 2)
  height <- 400 + (nrows - 1)*200
  return(height)
}

PLOT_FUN <- function(metric, ...) {
  switch(
    metric,
    "Fish biomass" = plot_fish_biomass(...),
    "Fish density" = plot_fish_density(...),
    "Fish diversity" = plot_fish_diversity(...),
    "Fish size" = plot_fish_size(...),
    "Mangrove sapling density" = plot_mangrove_sapling_density(...),
    "Mangrove diversity" = plot_mangrove_diversity(...),
    "Mangrove size" = plot_mangrove_size(...),
    "Coral reef cover" = plot_reef_cover(...),
    "Coral reef diversity" = plot_reef_diversity(...),
    "Seagrass cover" = plot_seagrass_cover(...),
    "Seagrass diversity" = plot_seagrass_diversity(...),
    "Seagrass height" = plot_seagrass_height(...),
    "Oyster density" = plot_oyster_density(...),
    "Oyster size" = plot_oyster_size(...),
    "Crab density" = plot_crab_density(...),
    "Crab size" = plot_crab_size(...)
  )
}

clean_plot <- function(p, facet_maa, y_scale, sel_metric, sel_maa) {
  if (facet_maa) {
    if (sel_metric == "Coral reef cover") {
      p$plot <- p$plot + facet_grid(rows = vars(ma_name), cols = vars(location_status))
      plot_height <- get_plot_height(2*length(sel_maa))
    } else {
      p$plot <- p$plot + facet_wrap('ma_name', ncol = 2)
      plot_height <- get_plot_height(length(sel_maa))
    }
  } else {
    if (sel_metric == "Coral reef cover") {
      p$plot <- p$plot + facet_wrap("location_status")
    }
    plot_height <- 'auto'
  }
  p$plot$facet$params$free$y <- !y_scale
  p$plot <- p$plot + ggplot2::labs(caption = WATERMARK_LABEL)
  
  out <- list(
    p=p,
    plot_height=plot_height
  )
  return(out)
}