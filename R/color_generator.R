
#' Generate colors from labels
#'
#' This function allows you to generate colors starting from a vector of labels.
#' @param labels A vector of labels convertible to a factor
#' @param preset_col There are 6 default color palettes
#' @param levels_colors An optional vector of colors, one for each level of `labels`
#' @param visualize Show or not a pie chart of the output colors
#'
#' @returns
#' * `colors`: A vector of colors for each value in `labels`
#' * `levels_colors`: A vector of unique colors for each level of `labels`
#' @export
#'
#' @examples
#' data(iris)
#' labels = iris$Species
#' levels = levels(labels)
#' colors_obj = color_generator(labels, preset_col = "6")
#' colors = colors_obj$colors
#' levels_colors = colors_obj$levels_colors
#' plot(
#'   iris[, 1:2],
#'   col = colors,
#'   pch = 19,
#'   xlab = "Sepal length",
#'   ylab = "Sepal width",
#'   main = "Iris dataset"
#' )
#' legend(
#'   "bottomright",
#'   legend = levels,
#'   fill = levels_colors,
#'   bty = "n",
#'   cex = 0.8
#' )
color_generator = function(labels,
                           preset_col = "1",
                           levels_colors = NULL,
                           visualize = F)
{
  if (!is.factor(labels))
    labels = factor(labels)
  levels = levels(labels)
  levels_number = length(levels)
  n = length(labels)
  
  library(randomcoloR)
  library(viridis)
  if (is.null(levels_colors))
    levels_colors = switch(
      preset_col,
      "1" = rainbow(levels_number),
      "2" = distinctColorPalette(levels_number),
      "3" = viridis_pal(option = "A")(levels_number),
      "4" = viridis_pal(option = "B")(levels_number),
      "5" = viridis_pal(option = "C")(levels_number),
      "6" = viridis_pal(option = "D")(levels_number)
    )
  colors = rep(NA, n)
  for (i in 1:n)
  {
    logical_vector = labels[i] == levels
    indexes = which(logical_vector)
    colors[i] = levels_colors[indexes]
  }
  if (visualize)
    pie(rep(1, levels_number), col = levels_colors)
  list(colors = colors, levels_colors = levels_colors)
}

