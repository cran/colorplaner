## ----setup, include=FALSE------------------------------------------------
# This file is part of colorplaner
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
library(knitr)
require(maps)
opts_chunk$set(fig.width = 6)

## ----redblue-------------------------------------------------------------
library(ggplot2)
library(colorplaner)

if(require(maps)) {
 crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  states_map <- map_data("state")
  ggplot(crimes,
         aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
    geom_map(map = states_map) +
    scale_fill_colorplane(color_projection = "red_blue") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() 
}

## ----interpolate---------------------------------------------------------
if(require(maps)) {
 crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  states_map <- map_data("state")
  ggplot(crimes,
         aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
    geom_map(map = states_map) +
    scale_fill_colorplane(color_projection = "interpolate",
                          zero_color = "darkorange2",
                          horizontal_color = "mediumspringgreen",
                          vertical_color = "#CD00CD") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map()
}

## ---- custom-------------------------------------------------------------
# Define a custom projection function that uses HSV color space by
# mapping the data to hue and saturation with a fixed level for value
hsv_projection <- function(x, y, v) {
  # Convert y value from a position to a hue angle
  h <- atan(scales::rescale(y, from = c(0,1), to = c(-1, 1)) / 
              scales::rescale(x, from =  c(0, 1), to = c(-1, 1)))
  # There are no missing values in the input, but atan can create some
  h <- ifelse(is.na(h), 0, h)
  h <- scales::rescale(h, from = c(-pi / 2, pi / 2), to = c(0, 1))
  # hsv takes inputs on a scale of [0, 1] and returns an RGB color string
  grDevices::hsv(h, x, v)
}

if(require(maps)) {
 crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  states_map <- map_data("state")
  ggplot(crimes,
         aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
    geom_map(map = states_map) +
    # The v argument will be passed on to hsv_projection
    scale_fill_colorplane(color_projection = hsv_projection, v = 0.75) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() 
}

