# This file is part of colorplaner. Code in this file is derived from modifying
# continuous_scale in ggplot2 (copyright RStudio 2016), 2016-09-18.
#
# colorplaner is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# colorplaner is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with colorplaner.  If not, see <http://www.gnu.org/licenses/>.

#' @include colorplane.R other_package_compatibility.R
#'
NULL

#' Color Plane Scale ggproto Object
#'
#' This ggproto object inherits from \code{\link[ggplot2]{ScaleContinuous}} and
#' implements methods and default values needed for color plane scale instances.
#' See \code{\link{scale_color_colorplane}} for usage.
#' @export
#' @keywords internal
ScaleColorPlane <- ggplot2::ggproto("ScaleColorPlane", ggplot2::ScaleContinuous,
  limits_y = NULL,
  breaks_y = ggplot2::waiver(),
  labels_y = ggplot2::waiver(),
  axis_title = ggplot2::waiver(),
  axis_title_y = ggplot2::waiver(),
  range = ggplot2::ggproto(NULL, RangeContinuous),
  range_y = ggplot2::ggproto(NULL, RangeContinuous),
  na.color = NULL,
  projection_function = NULL,
  projection_function_args = list(),
  # tracks if error has been displayed to avoid repeated messages
  has_error = FALSE,
  map_df = function(self, df, i = NULL) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()
    if(length(self$aesthetics) != 2 || self$is_empty()) {
      if(!self$has_error) {
        message("No valid colorplane mapping found. ",
                ifelse("colour" %in% self$aesthetics,
                       "scale_color_colorplane requires both color and color2",
                       "scale_fill_colorplane requires both fill and fill2"),
              " aesthetics to be mapped.")
        self$has_error <- TRUE
      }
      return()
    }
    aes_check <- intersect(self$aesthetics, names(df))
    if (length(aes_check) != 2) {
      # silently skip layers that don't have colorplane mapping
      return()
    }

    aesthetics <- self$aesthetics
    df <- df[ , aesthetics]

    xlim <- self$get_limits(dir = "horizontal")
    x <-  self$oob(df[[aesthetics[1]]], xlim)
    x <- self$rescaler(x, c(0,1), xlim)

    ylim <- self$get_limits(dir = "vertical")
    y <- self$oob(df[[aesthetics[2]]], ylim)
    y <- self$rescaler(y, c(0,1), ylim)

    # prefill with the NA color so we can do the projection as an inset, process
    # the NA color to ensure proper #xxxxxx form regardless of unput type
    df[[aesthetics[1]]] <- grDevices::rgb(
      t(grDevices::col2rgb(self$na.color[1])),
      maxColorValue = 255)
    whichOK <- !is.na(x) & !is.na(y)
    # TODO: support extra argument pass_through
    df[whichOK, aesthetics[1]] <- do.call(self$projection_function,
                                          c(list(x = x[whichOK],
                                               y = y[whichOK]),
                                            self$projection_function_args))

    # This handling for optional paramter i is in the default method for Scale
    # proto, but the method is only ever called from ggplot_build without it

    #    if (is.null(i)) {
    #      lapply(aesthetics, function(j) self$map(df[[j]]))
    #    } else {
    #      lapply(aesthetics, function(j) self$map(df[[j]][i]))
    #    }
    df
  },
  train_df = function(self, df) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()
    aesthetics <- intersect(self$aesthetics, names(df))
    aesthetics <- aesthetics[c(grep("[[:digit:]]", aesthetics, invert = TRUE),
                               grep("[[:digit:]]", aesthetics))]
    names(aesthetics) <- aesthetics
    if (length(aesthetics) != 2) {
      return()
    }
    self$aesthetics <- aesthetics
    # default axis titles: cannot find any other way to access the original
    # variable names in the plot data, so grabbing 'plot' object from
    # ggplot_build in the call stack with dynGet. This can be avoided by
    # specifying the axis titles
    if(is.waive(self$axis_title) ||
       is.waive(self$axis_title_y)) {
      p <- dynGet("plot", ifnotfound = NULL)
      if(is.waive(self$axis_title)) {
        if(!is.null(p$labels) && !is.null(p$labels[[aesthetics[1]]])) {
          self$axis_title <- p$labels[[aesthetics[1]]]
        }
      }
      if(is.waive(self$axis_title_y)) {
        if(!is.null(p$labels) && !is.null(p$labels[[aesthetics[2]]])) {
          self$axis_title_y <- p$labels[[aesthetics[2]]]
        }
      }
    }
    self$range$train(df[[aesthetics[1]]])
    self$range_y$train(df[[aesthetics[2]]])
  },
  get_range = function(self, dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if(dir == "horizontal") self$range$range else self$range_y$range
  },
  get_limits = function(self, dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (self$is_empty()) return(c(0, 1))

    if(dir == "horizontal") {
      if (!is.null(self$limits)) {
        ifelse(!is.na(self$limits), self$limits, self$get_range(dir))
      } else {
        self$get_range(dir)
      }
    } else {
      if (!is.null(self$limits_y)) {
        ifelse(!is.na(self$limits_y), self$limits_y, self$get_range(dir))
      } else {
        self$get_range(dir)
      }
    }
  },
  get_breaks = function(self, limits = self$get_limits(dir),
                        dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (self$is_empty()) return(numeric())
    if(dir == "horizontal") breaks <- self$breaks else breaks <- self$breaks_y
    # Limits in transformed space need to be converted back to data space
    limits <- self$trans$inverse(limits)

    if (is.null(breaks)) {
      return(NULL)
    } else if (identical(breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA")
    } else if (scales::zero_range(as.numeric(limits))) {
      breaks <- limits[1]
    } else if (is.waive(breaks)) {
      breaks <- self$trans$breaks(limits)
    } else if (is.function(breaks)) {
      breaks <- breaks(limits)
    }

    # Breaks in data space need to be converted back to transformed space
    # And any breaks outside the dimensions need to be flagged as missing
    breaks <- scales::censor(self$trans$transform(breaks),
                             self$trans$transform(limits),
                             only.finite = FALSE)
    if (length(breaks) == 0) {
      stop("Zero breaks in scale for ", paste(self$aesthetics, collapse = "/"),
           call. = FALSE)
    }
    breaks
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(),
                              limits = self$get_limits()) {
    # minor breaks not implemented
    return()
  },
  get_labels = function(self, breaks = self$get_breaks(dir),
                        dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (is.null(breaks)) return(NULL)

    breaks <- self$trans$inverse(breaks)

    if (dir == "horizontal") labels <- self$labels else labels <- self$labels_y

    if (is.null(labels)) {
      return(NULL)
    } else if (identical(labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(labels)) {
      labels <- self$trans$format(breaks)
    } else if (is.function(labels)) {
      labels <- labels(breaks)
    }
    if (length(labels) != length(breaks)) {
      stop("Breaks and labels are different lengths")
    }
    labels
  },
  map = function(self, x, limits = self$get_limits(dir),
                 dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    x <- self$oob(self$rescaler(x, from = limits))

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  }
)

#' Bivariate Color Space Projection Scale
#'
#' Maps two continuous variables into a single display color, using either the
#' \code{color} and \code{color2} aesthetics (\code{scale_color_colorplane}) or
#' the \code{fill} and \code{fill2} aesthetics (\code{scale_fill_colorplane}).
#' Variables mapped to \code{color} or \code{fill} are be mapped to the
#' horizontal component of the colorplane scale and \code{color}/\code{fill2}
#' are mapped to the vertical component.
#'
#' Variable values are projected into color space to create a bivariate
#' gradient. The default projection maps values to the U and V components of YUV
#' color space. In the YUV color space, the full spectrum of chrominance (color
#' difference) is encoded into the U and V components and luminosity
#' (brightness) is encoded in the Y component. For a fixed value of Y, the
#' remaining U-V color space is a plane of all possible colors at that
#' brightness. Therefore, mapping data to this projection utilizes the full
#' color spectrum to provide visual discrimination between differing values.
#'
#' The YUV projection colorplane scale is visually divided into four quadrants:
#' green when both values are small, fuchsia when both are large, orange when
#' the horizontal variable is small and the vertical is large, and blue when the
#' horizontal variable is large and the vertical is small. Values closer to the
#' extremes are saturated and the center of the scale, representing the
#' mid-point of the ranges for both variables, is grey.
#'
#' Alternative color projections can be used, but may not be as interpretable.
#' See \code{\link{color_projections}} for information on specifying or creating
#' other color projections.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams guide_colorplane
#' @param breaks_y As \code{breaks}, but for vertical axis (i.e. \code{color2}
#'   or \code{fill2})
#' @param labels_y As \code{labels}, but for vertical axis (i.e. \code{color2}
#'   or \code{fill2})
#' @param limits_y As \code{limits}, but for vertical axis (i.e. \code{color2}
#'   or \code{fill2})
#' @param name Character string or expression to be used as guide title.
#'   Defaults to "Color Key" or "Fill Color Key" to match the scale function
#'   used.
#' @param na.color Character string containing a valid R color to use when
#'   plotting missing data or data outside the limits.
#' @param guide Name of guide object, or object itself. Defaults to
#'   \code{\link{guide_colorplane}} designed for this scale. Behavior of other
#'   guides with this scale is not defined.
#' @param color_projection Projection mapping to use. Either the name of an
#'   included projection or a function that performs the projection. See
#'   \code{\link{color_projections}}.
#' @param ... Additional arguments to pass on to \code{color_projection}
#'   function.
#' @param axis_title,axis_title_y Character strings or expressions indicating
#'   the horizontal and vertical axis titles in the guide, respectively. If
#'   \code{NULL}, the title is not shown. By default (\link[ggplot2]{waiver}),
#'   the name of the scale or the name of the variable mapped to the aesthetic.
#' @examples
#' library(ggplot2)
#' if(requireNamespace("mapproj")) {
#'   crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'   states_map <- map_data("state")
#'   ggplot(crimes,
#'          aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
#'     geom_map(map = states_map) +
#'     scale_fill_colorplane() +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     coord_map()
#'  }
#' # setting upper limit for qsec causes points for higher values to plot
#' # as na.color (black)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
#'   geom_point(size = 4) +
#'   scale_color_colorplane(limits = c(NA, 18.9))
#' @export
#' @aliases scale_colour_colourplane scale_fill_colourplane
#' @references Based on \code{\link[ggplot2]{continuous_scale}}, modified 2016.
scale_color_colorplane <- function(name = waiver(),
                                   axis_title = waiver(),
                                   axis_title_y = waiver(),
                                   breaks = waiver(),
                                   breaks_y = waiver(),
                                   labels = waiver(),
                                   labels_y = waiver(),
                                   limits = NULL,
                                   limits_y = NULL,
                                   color_projection = "YUV",
                                   rescaler = rescale,
                                   oob = censor,
                                   trans = "identity",
                                   na.color = "black",
                                   na.value = NA_real_,
                                   guide = "colorplane",
                                   ...) {

  check_breaks_labels(breaks, labels)

  if (is.null(breaks) && guide != "none") {
    guide <- "none"
  }
  color_projection <- get_projection(color_projection)
  # using local version of as.trans to avoid namespace issues
  trans <- as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }
  if(!is.null(limits_y)) {
    limits_y <- trans$transform(limits_y)
  }

  # Handle waived names, ggplot would insert the horizontal axis name by
  # default, which does not make sense in this context
  if(is.waive(name)) name <- "Color Key"

  ggproto(NULL, ScaleColorPlane,
          call = match.call(),

          aesthetics = c("colour", "colour2", "color2"),
          scale_name = "colorplane",
          projection_function = color_projection,
          projection_function_args = list(...),
          palette = scales::identity_pal(),
          range = ggproto(NULL, RangeContinuous),
          range_y = ggproto(NULL, RangeContinuous),

          limits = limits,
          limits_y = limits_y,
          trans = trans,
          na.color = na.color,
          na.value = na.value,
          expand = function(range, ...) {range},
          rescaler = rescaler,  # Used by diverging and n colour gradients
          oob = oob,

          name = name,
          axis_title = axis_title,
          axis_title_y = axis_title_y,

          breaks = breaks,
          breaks_y = breaks_y,

          labels = labels,
          labels_y = labels_y,
          guide = guide
  )
}

#' @export
#' @rdname scale_color_colorplane
scale_fill_colorplane <- function(name = waiver(),
                                  axis_title = waiver(),
                                  axis_title_y = waiver(),
                                  breaks = waiver(),
                                  breaks_y = waiver(),
                                  labels = waiver(),
                                  labels_y = waiver(),
                                  limits = NULL,
                                  limits_y = NULL,
                                  color_projection = "YUV",
                                  rescaler = rescale,
                                  oob = censor,
                                  trans = "identity",
                                  na.color = "black",
                                  na.value = NA_real_,
                                  guide = "colorplane",
                                  ...) {

  check_breaks_labels(breaks, labels)

  if (is.null(breaks) && guide != "none") {
    guide <- "none"
  }
  color_projection <- get_projection(color_projection)
  # using local version of as.trans to avoid namespace issues
  trans <- as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }
  if (!is.null(limits_y)) {
    limits_y <- trans$transform(limits_y)
  }

  # Handle waived names, ggplot would insert the horizontal axis name by
  # default, which does not make sense in this context
  if(is.waive(name)) name <- "Fill Color Key"

  ggproto(NULL, ScaleColorPlane,
          call = match.call(),

          aesthetics = c("fill", "fill2"),
          scale_name = "fillplane",
          projection_function = color_projection,
          projection_function_args = list(...),
          palette = scales::identity_pal(),
          range = ggproto(NULL, RangeContinuous),
          range_y = ggproto(NULL, RangeContinuous),

          limits = limits,
          limits_y = limits_y,
          trans = trans,
          na.color = na.color,
          na.value = na.value,
          expand = function(range, ...) {range},
          rescaler = rescaler,  # Used by diverging and n colour gradients
          oob = oob,

          name = name,
          axis_title = axis_title,
          axis_title_y = axis_title_y,

          breaks = breaks,
          breaks_y = breaks_y,

          labels = labels,
          labels_y = labels_y,
          guide = guide
  )
}
#' @export
scale_colour_colourplane <- scale_color_colorplane
#' @export
scale_fill_colourplane <- scale_fill_colorplane
