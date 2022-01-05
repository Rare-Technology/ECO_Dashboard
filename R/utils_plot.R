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
    panel.grid.major = element_line(color = "grey75"),
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
  
  ggplot2::ggplot() +
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
    facet_wrap("ma_name") +
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
      panel.grid.minor.x = element_blank()
    )
}

plot_bar <- function(data, x = "location_status", y = NULL, ymin = "ymin", ymax = "ymax",
  fill = "location_status", title = NULL, year = NULL, x_label = "", y_label = NULL) {
  
  ggplot2::ggplot() +
    geom_bar(
      data = data,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      position = position_dodge(),
      color = "black",
      stat = 'identity',
      width = 0.6,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data = data,
      aes_string(
        x = x,
        fill = fill,
        ymin = ymin,
        ymax = ymax
      ),
      position = position_dodge(),
      width = 0.2,
      na.rm = TRUE
    ) +
    ggtitle(paste(title, year)) +
    facet_wrap('ma_name') +
    xlab(x_label) + 
    ylab(y_label) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_blank()
    ) +
    scale_fill_manual(values = c(RARE_COLORS$lightblue, RARE_COLORS$lightgreen))
}

plot_samples <- function(data_local, x = NULL, y = NULL, fill = NULL,
  shape = 21, point_size = 2) {
  geom_point(
      data = data_local,
      aes_string(
        x = x,
        y = y,
        fill = fill
      ),
      position = position_jitterdodge(
        jitter.width = 0.1,
        dodge.width = 0.5
      ),
      shape = shape,
      alpha = 0.5,
      size = point_size,
      show.legend = FALSE
    )
}

plot_histogram <- function(data_full, data_summary, x = NULL, fill = "location_status",
  year = year, title = NULL, x_label = NULL) {
  ggplot2::ggplot() +
    geom_histogram(
      data = data_full,
      aes_string(
        x = x,
        fill = fill
      ),
      position = "identity",
      binwidth = 1,
      color = "black",
      alpha = 0.8
    ) +
    geom_vline(
      data = data_summary,
      aes_string(
        xintercept = x,
        color = fill
      ),
      size = 1,
      linetype = "dashed"
    ) +
    facet_wrap("ma_name") +
    ggtitle(paste(title, year)) +
    xlab(x_label) +
    labs(
      fill = "",
      color = "Mean"
    ) +
    theme_rare() +
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.minor.x = element_line()
    ) +
    scale_x_continuous(
      breaks = seq(0, max(data_full[[x]], na.rm = TRUE), by = 10),
      labels = seq(0, max(data_full[[x]], na.rm = TRUE), by = 10)
    ) +
    # guides(
    #   color = guide_legend(title = "Mean")
    # ) +
    scale_color_manual(values = c(RARE_COLORS$red, RARE_COLORS$yellow)) +
    scale_fill_manual(values = c(RARE_COLORS$red, RARE_COLORS$yellow))
}

plot_histogram_trend <- function(data, x = NULL, y = NULL, fill = "location_status",
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
    facet_wrap("ma_name") +
    ggtitle(title) +
    xlab(x_label) +
    ylab(y_label) +
    labs(fill = "") +
    theme_rare() +
    scale_fill_manual(values = c(RARE_COLORS$red, RARE_COLORS$yellow))
}