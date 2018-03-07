#' GGPlot Wrapper
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' ggwrapper(d)
ggwrapper<-function(x){
  library(ggplot2)
  library(magrittr)
  x %>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' dplyr Wrapper
#'
#' This is a wrapper for dyplr
#'
#' @param x A data-frame
#'
#' @export
DWrapper <- function(x) {
  xdf <- dplyr::data_frame(x)
  dplyr::count(xdf, x)
}
