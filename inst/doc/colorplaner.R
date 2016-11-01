## ---- include = FALSE----------------------------------------------------
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
library(ggplot2)
library(colorplaner)

## ----intro, echo = FALSE, fig.cap="Example use of colorplane with the iris dataset."----
data(iris)

iris_plot <- ggplot(iris, 
                aes(x = Petal.Length, y = Petal.Width, shape = Species,
                    color = Sepal.Length, color2 = Sepal.Width)) +
  geom_point(position = "jitter") +
  scale_color_colorplane(limits = range(iris$Sepal.Length),
                         limits_y = range(iris$Sepal.Width),
                         breaks_y = function(x)seq.int(x[1], x[2]),
                         guide = guide_colorplane(
                           nbin = 10, 
                           planewidth = 2.5, 
                           planeheight = 2.5,
                           axis_title.theme = element_text(size = 8),
                           axis_title_y.theme = element_text(size = 8))) +
  theme(legend.text = element_text(size = 8))

iris_plot


## ----colorplane-concept, echo = FALSE, warning = FALSE, message =FALSE, fig.cap="With Y held at 30%, this U-V color plane can be visually divided into 4 color quadrants."----
ggplot(expand.grid(a = seq(0, 1, length.out = 75), 
                   b = seq(1, 0, length.out = 75)), 
       aes(x = a, y = b, color = a, color2 = b)) +
  geom_point(size = 3, alpha = .75) +
  scale_color_colorplane(guide = F) +
  scale_x_continuous(label = scales::percent) +
  scale_y_continuous(label = scales::percent) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "U Component", y = "V Component") +
  annotate("text",  x = c(rep(.15, 2), rep(.85, 2)),
           y = rep(c(.9, .1), 2), size = 4,
           label = c("orange", "green", "magenta", "blue")) +
  geom_vline(xintercept = .5) +
  geom_hline(yintercept = .5) +
  annotate("text", x = .51, y = setdiff(seq(0, 1, by = .1), .5), size = 2.5,
           label = scales::percent(setdiff(seq(0, 1, by = .1), .5)), hjust = 0) +
  annotate("text", y = .49, x = setdiff(seq(0, 1, by = .1), .5), size = 2.5,
           label = scales::percent(setdiff(seq(0, 1, by = .1), .5)), vjust = 1)

## ----colorplane-interpretation, echo = FALSE, warning = FALSE, message =FALSE,  fig.show='hold'----
data(iris)
set.seed(123)
num_lab <- 15
samp <- sample(row.names(iris), num_lab)
iris$labeled <- F
iris[samp, "labeled"] <- T
iris[samp, "let"] <- letters[seq_len(num_lab)]
sepal_grid <- expand.grid(
  Sepal.Length = seq(min(iris$Sepal.Length), max(iris$Sepal.Length), 
                     length.out = 50),
  Sepal.Width = seq(min(iris$Sepal.Width), max(iris$Sepal.Width), 
                    length.out = 50))

iris_plot %+% iris[iris$labeled, ] + 
  geom_text(aes(label = let),  nudge_x = .2, nudge_y = -.05) +
  guides(color = FALSE) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), 
        legend.key.width = grid::unit(3, "mm"))

ggplot(sepal_grid, aes(x = Sepal.Length, y = Sepal.Width, 
                       color2 = Sepal.Width)) +
  geom_point(aes(color = Sepal.Length), size = 5) +
  geom_point(aes(shape = Species), data = iris[iris$labeled, ], size = 1.5) +
  geom_text(aes(label = let), data = iris[iris$labeled, ], 
            nudge_x = .14, nudge_y = -.04) +
  scale_color_colorplane(guide = "none") +
  scale_shape_manual(values = c("setosa" = 1, "versicolor" = 2, "virginica" = 0),
                     guide = "none") +
  theme(panel.background = element_blank())


## ----correlations--------------------------------------------------------
cor(as.matrix(iris[ , 1:4]), method = "pearson") 

## ----variance------------------------------------------------------------
var(iris$Sepal.Width[iris$Sepal.Length <= mean(range(iris$Sepal.Length))])
var(iris$Sepal.Width[iris$Sepal.Length > mean(range(iris$Sepal.Length))])

## ----colorplane-basics, fig.width=5,fig.height=3.5, message=FALSE, warning=FALSE, fig.cap=""----
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, shape = Species,
                 color = Sepal.Length, color2 = Sepal.Width)) +
  geom_point() +
  scale_color_colorplane()

if(require(mapproj)) {
  crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  states_map <- map_data("state")
  ggplot(crimes, aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
    geom_map(map = states_map) +
    scale_fill_colorplane() +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() 
}

## ----colorplane-advanced, fig.height=5, fig.width=4, fig.cap=""----------
if(require("maps")) {
  crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  states_map <- map_data("state")
  ggplot(crimes,
         aes(map_id = state, fill = Murder, fill2 = UrbanPop/100)) +
    geom_map(map = states_map) +
    scale_fill_colorplane(labels_y = scales::percent,
                          axis_title = "Murder arrests\nper 100,000 people",
                          axis_title_y = "Percent Urban\nPopulation",
                          limits_y = c(0,1)) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() +
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "", title = paste("1973 U.S. Arrest Data:\nMurder Rate",
                                       "v. Urban Population by State")) +
    guides(fill = guide_colorplane(
      title = "State Color Key",
      title.theme = element_text(size = 13),
      label.theme = theme_gray(),
      label.y.theme = theme_gray(),
      axis.title.position = "top",
      label.position = c("top", "bottom"),
      label.y.position = c("left", "right")))
}

