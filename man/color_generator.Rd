% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_generator.R
\name{color_generator}
\alias{color_generator}
\title{Generate colors from labels}
\usage{
color_generator(labels, preset_col = "1", levels_colors = NULL, visualize = F)
}
\arguments{
\item{labels}{A vector of labels convertible to a factor}

\item{preset_col}{There are 6 default color palettes}

\item{levels_colors}{An optional vector of colors, one for each level of \code{labels}}

\item{visualize}{Show or not a pie chart of the output colors}
}
\value{
\itemize{
\item \code{colors}: A vector of colors for each value in \code{labels}
\item \code{levels_colors}: A vector of unique colors for each level of \code{labels}
}
}
\description{
This function allows you to generate colors starting from a vector of labels.
}
\examples{
data(iris)
labels = iris$Species
levels = levels(labels)
colors_obj = color_generator(labels, preset_col = "6")
colors = colors_obj$colors
levels_colors = colors_obj$levels_colors
plot(
  iris[, 1:2],
  col = colors,
  pch = 19,
  xlab = "Sepal length",
  ylab = "Sepal width",
  main = "Iris dataset"
)
legend(
  "bottomright",
  legend = levels,
  fill = levels_colors,
  bty = "n",
  cex = 0.8
)
}
