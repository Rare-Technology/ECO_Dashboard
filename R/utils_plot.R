#' plot 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
theme_rare <- function(rotate_x = FALSE, subtitle_color = "black") {
  hrbrthemes::theme_ipsum_rc(
    axis_title_size = 14,
    axis_title_just = "cc",
    plot_margin = margin(5, 5, 5, 5),
    axis_text_size = 10
  ) +
  theme(
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 15, colour = subtitle_color),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 14, face = "bold")    
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