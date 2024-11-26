#' Performs a t.test to calculate the confidence interval for the mean from a random sample
#'
#'
#' @param x is the value of samples
#'
#'
#' @example myci(x=c(9,9,1,9,9,9))
#'
#' @export
myci=function(x){
  t.test(x, conf.level = 0.95)
}
