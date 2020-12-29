#' @title Custom Color Palette for 2-4 Groups
#' @description Applies a custom color palette for 2-4 groups to be used
#'     in figures in the final report for the Open Water Mercury studies.
#'     The palette is based on the Viridis palette. Function is to be used
#'     on ggplot objects. Can be used to represent any categorical grouping
#'     variable as long as it has the same number of groups that is specified
#'     in \code{num_colors}.
#'
#' @param num_colors An integer representing the number of colors for the
#'     palette. Must be between 2-4.
#' @param aes_type The aesthetic type to apply the palette to. Must be
#'     either \code{"fill"} or \code{"color"}.
#' @param legend_title An optional argument used to provide a title for the
#'     legend of the color palette. Default is \code{NULL}.
#'
#' @return A ggplot object with a custom color palette applied to it
#'     with the specified number of colors and aesthetic type.
#' @export
#' @importFrom colorspace sequential_hcl
#' @import ggplot2
add_gen_color_pal <- function(num_colors = c(2:4),
                              aes_type = c("fill", "color"),
                              legend_title = NULL) {
  # evaluate choices for num_colors
  if (!(as.integer(num_colors) %in% 2L:4L)) stop("numb_colors must be between 2 and 4")

  # evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # define colors for each palette
  pal_two <- c("#F69422", "#00588B")
  pal_three <- rev(sequential_hcl(3, palette = "Viridis"))
  pal_four <- rev(sequential_hcl(4, palette = "Viridis"))

  # apply palette based on num_colors and aes_type
  if (aes_type == "fill") {
    if (num_colors == 2) {
      scale_fill_manual(name = legend_title, values = pal_two)
    } else if (num_colors == 3) {
      scale_fill_manual(name = legend_title, values = pal_three)
    } else {
      scale_fill_manual(name = legend_title, values = pal_four)
    }
  } else {
    if (num_colors == 2) {
      scale_color_manual(name = legend_title, values = pal_two)
    } else if (num_colors == 3) {
      scale_color_manual(name = legend_title, values = pal_three)
    } else {
      scale_color_manual(name = legend_title, values = pal_four)
    }
  }
}


#' @title Custom Color Palette for Inlet Stations
#' @description Applies a custom color palette for the five inlet stations
#'     to be used in figures in the final report for the Open Water Mercury
#'     studies. The palette is based on the Viridis palette. Function is to
#'     be used on ggplot objects. Each observation under the inlet grouping
#'     variable used in the ggplot must have one of the following values:
#' \itemize{
#' \item KLRC
#' \item CCSB
#' \item Putah Creek
#' \item Sacramento Weir
#' \item Fremont Weir
#' }
#' This function works best if the inlet grouping variable is a factor in
#' above order.
#'
#' @param aes_type The aesthetic type to apply the palette to. Must be
#'     either \code{"fill"} or \code{"color"}.
#' @param legend_title An optional argument used to provide a title for the
#'     legend of the color palette. Default is \code{NULL}.
#'
#' @return A ggplot object with the custom color palette for the inlets applied
#'     to it.
#' @export
#' @import ggplot2
add_inlet_color_pal <- function(aes_type = c("fill", "color"), legend_title = NULL) {
  # evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # define color palette for inlets
  pal_inlet <- c(
    "KLRC" = "#FDE333",
    "CCSB" = "#53CC67",
    "Putah Creek" = "#009B95",
    "Sacramento Weir" = "#00588B",
    "Fremont Weir" = "#4B0055"
  )

  # apply palette based on aes_type
  if (aes_type == "fill") {
    scale_fill_manual(name = legend_title, values = pal_inlet)
  } else {
    scale_color_manual(name = legend_title, values = pal_inlet)
  }
}

#' @title Open Water Hg Report Plot Theme
#' @description This custom theme is the standardized theme used in plots
#'     within the final report for the Open Water Mercury studies. It builds
#'     off of the \code{theme_light()} ggplot theme and changes the facet
#'     label text color to black.
#'
#' @param x_axis_v Makes the tick labels for the x-axis vertical if
#'     \code{x_axis_v = TRUE}. Default is \code{FALSE}.
#'
#' @return A ggplot layer that applies a custom theme as described above.
#' @import ggplot2
#' @export
theme_owhg <- function(x_axis_v = FALSE) {
  t <- list(
    theme_light(),
    # change facet label text color to black
    theme(strip.text = element_text(color = "black"))
  )

  # make x axis tick labels vertical if x_axis_v = TRUE
  if (x_axis_v == TRUE) {
    t <- append(t, list(theme(axis.text.x = element_text(angle = 90, hjust = 1))))
  }

  return(t)
}

