#' Save plot in .png
#'
#' A function that allows you to save standard R plots 
#' or ggplot plots in .png format.
#' 
#' @param plot A variable of class `recordedplot` or `ggplot`, see examples.
#' @param name The name of the plot without extensions. Default is `"test"`.
#' @param folder The path of the folder where to save the plot. Default is the current folder.
#' @param type `"standard"` if plot is a standard R plot or `"ggplot"` if plot is a ggplot.
#' @param width the width of the device.
#' @param height the height of the device.
#' @param pointsize the default pointsize of plotted text.
#'
#' @export
#'
#' @examples
#' data = data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
#' 
#' ### ggplot example
#' library(ggplot2)
#' plot1 = ggplot(data = data, aes(x = x, y = y)) + geom_point()
#' save_png_plot(
#'   plot1,
#'   name = "ggplot_test",
#'   type = "ggplot"
#' )
#' 
#' ### standard plot example
#' plot(data$x,data$y, xlab="x", ylab="y",pch=19)
#' points(3,5,col="red",pch=19)
#' plot2 = recordPlot()
#' save_png_plot(
#'   plot2,
#'   name = "standard_plot_test",
#'   type = "standard"
#' )
save_png_plot = function(plot,
                     name = "test",
                     folder = "./",
                     type = "standard",
                     format = "svg",
                     width = 480,
                     height = 480,
                     pointsize = 12)
{
  final_name = paste(folder, name, ".png", sep = "")
  if (type == "standard")
  {
    png(final_name, width = width, height = height,pointsize=pointsize)
    replayPlot(plot)
    dev.off()
  } else if (type == "ggplot")
  {
    #ggsave(nome_finale, plot, width = width, height = height)
    png(final_name, width = width, height = height,pointsize=pointsize)
    print(plot)
    dev.off()
  }
}
