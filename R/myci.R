#' Performs a t.test to calculate the confidence interval for the mean from a random sample
#'
#'
#' @param x is the value of samples
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis barplot curve hist layout lines points polygon segments text
#' @importFrom stats dnorm dpois pbinom pnorm quantile rpois t.test var
#' @example myci(x=c(9,9,1,9,9,9))
#'
#' @export
myci=function(x){
  t.test(x, conf.level = 0.95)
}
