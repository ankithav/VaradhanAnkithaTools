#' Computation of a Matrix and Vector
#'
#' Computes (x)(matrix)(x^T)
#'
#' @param mat A Matrix
#' @param vec A Vector
#' @return Scalar
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' matx(a, x)
#'
matx <- function(mat, vec){
  stopifnot(is.numeric(mat))
  stopifnot(is.numeric(vec))
  stopifnot(ncol(mat) == nrow(mat))
  stopifnot(ncol(mat) == length(vec))
  stopifnot(is.finite(vec))
  stopifnot(is.finite(mat))

  inv <- solve(mat)
  return(t(vec) %*% inv %*% vec)

}


#' Standardize Matrix
#'
#' Column standarization of a matrix
#'
#' @param dat A 2-D Matrix with numerical values
#' @return A 2-D Matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' std(a)
#'
std <- function(dat){
  stopifnot(nrow(dat) > 1)
  stopifnot(is.numeric(dat))
  stopifnot(is.finite(dat))

  count = 1
  rows <- nrow(dat)
  cols <- ncol(dat)
  while (count <= cols){
    singleColumn <- dat[,count]
    columnMean <- mean(singleColumn)
    columnSd <- sd(singleColumn)
    dat[,count] = ((singleColumn-columnMean)/columnSd)
    count = count + 1
  }
  return(dat)
}
