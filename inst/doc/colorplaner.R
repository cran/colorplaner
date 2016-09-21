## ---- include = FALSE----------------------------------------------------
library(ggplot2)
library(colorplaner)

## ----colorplane-concept, echo = FALSE, warning = FALSE, message =FALSE, fig.cap="Holding Y constant at 35% yields this U-V color plane"----
ggplot(expand.grid(a = seq(0, 1, length.out = 50), 
                   b = seq(1, 0, length.out = 50)), 
       aes(x = a, y = b, color = a, color2 = b)) +
  geom_point(size = 5) +
  scale_color_colorplane(guide = F) +
  scale_x_continuous(label = scales::percent) +
  scale_y_continuous(label = scales::percent) +
  theme(panel.background = element_blank()) +
  labs(x = "U Component", y = "V Component")


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
p_key <- ggplot(sepal_grid, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Sepal.Length, color2 = Sepal.Width), size = 5) +
  geom_point(aes(shape = Species), data = iris[iris$labeled, ], size = 1.5) +
  geom_text(aes(label = let), data = iris[iris$labeled, ], nudge_x = .14, nudge_y = -.04) +
  scale_color_colorplane(guide = "none") +
  scale_shape_manual(values = c("setosa" = 1, "versicolor" = 2, "virginica" = 0),
                     guide = "none") +
  theme(panel.background = element_blank())
p_dat <- ggplot(iris[iris$labeled, ], 
                aes(x = Petal.Length, y = Petal.Width, shape = Species)) +
  geom_point(aes(color = Sepal.Length, color2 = Sepal.Width)) +
  geom_text(aes(label = let), nudge_x = .2, nudge_y = -.05) +
  scale_color_colorplane(limits = range(iris$Sepal.Length),
                         limits_y = range(iris$Sepal.Width),
                         guide = "none") +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), 
        legend.key.width = grid::unit(3, "mm"))
print(p_key)
print(p_dat)


## ----colorplane-basics, fig.width=6, message=FALSE, warning=FALSE, fig.cap=""----
data(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg, color = disp, color2 = hp)) +
  geom_point(size = 5) +
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
if(require(mapproj)) {
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
      label.theme = theme_grey(),
      label_y.theme = theme_grey(),
      axis_title.position = "top",
      label.position = c("top", "bottom"),
      label_y.position = c("left", "right")))
}

