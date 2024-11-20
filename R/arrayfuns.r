#' @title Drop slices that sum to zero in specified dimension
#' 
#' @description 
#' Drop slices of an array that sum to zero in specified dimension (only test on
#' 2D and 3D arrays)
#' 
#' @param x An array whose elements can be summed
#' @param dropdim Dimension for which zero-sum slices should be dropped
#' 
#' @return array of the same dimension as x
#' 
#' @export
dim_drop0 <- function(x, dropdim) {
  sums <- apply(x, MARGIN = dropdim, sum)
  # return e.g. for dropdim=1:  x[sums > 0, , ]
  return(eval(parse(text=paste0("x[", paste0(rep(",",dropdim-1), collapse=""), "sums>0", paste0(rep(",", length(dim(x))-dropdim),collapse=""), "]", collapse=""))))
}


#' @title Convert array to binomial
#' 
#' @description 
#' Convert an integer array to binomial. Also works with a data frame or tibble.
#' 
#' @param x An integer array
#' 
#' @return binomial array of the same dimension and size as x
#' 
#' @export
array2binom <- function(x) {
  x[x>1] <- 1
  return(x)
}