#' Plot the value of a factor into a map of the North Sea
#'
#' \code{plot_factor_in_map} generates a map of the North Sea and plots the
#' value of a factor for each rectangle, either as point size or colour. The
#' function uses ICES rectangles to locate the data, conversion to coordinates
#' is provided.
#'
#' @param data Dataset to use for the plotting.
#' @param parameter A character string holding the name of the parameters
#' that are to be plot.
#' @param ices_rectangles A character string with the name of the column
#' where location as ICES rectangles are stored in the dataset.
#' @param visual A character string defining how you want the value of the
#' parameter to be represented. Use \code{visual = 'circles'} for point
#' size \code{cex} to represent values, use \code{visual = 'colour'} for
#' colour to represent values. Default is \code{visual = 'colour'}.

plot_factor_in_map <- function(data, parameter, ices_rectangles, visuals = 'colour') {

  # create a base map
  maps::map(database = 'worldHires', xlim = c(-6,13), ylim = c(48,62), fill = T, col = grey(0.7))
  graphics::box(which = 'plot', lty = 'solid')

  # convert the names of ICES rectangles into coordinates (of the midpoint of the rectangle)
  coordinates <- geo::ir2d(ir = data[,ices_rectangles], useI = F)
  data$lat  <- coordinates$lat
  data$lon  <- coordinates$lon
  rm(coordinates)

  # plot parameter values
  if(visuals == 'circles') {
    points(x = data$lon, y = data$lat, cex = data[,parameter])
  }
  if(visuals == 'colour') {
    symbol_colour <- data[,parameter] / max(data[,parameter])
    points(x = data$lon, y = data$lat, col = symbol_colour)
  }

}
