#' Function to plot a normal curve and region while giving the area of the region
#'
#'
#'
#' @param mu the mean of the distribution
#' @param sigma the standard deviation of the distribution
#' @param a the value of x you want to evaluate at
#' @param x is the vector of values
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis barplot curve hist layout lines points polygon segments text
#' @importFrom stats dnorm dpois pbinom pnorm quantile rpois t.test var
#' @example myncurve(2, 4 ,1)
#'
#' @export
myncurve = function(x,mu, sigma, a=1){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  list(mu = mu, sigma = sigma)
  neginf = -10000000
  xreg = seq(mu - 3 * sigma, a, length=1000)
  yreg= dnorm(xreg, mean=mu, sd=sigma)
  polygon(c(mu - 3 * sigma,xreg,a),c(0,yreg,0),col="blue")
  area = round((pnorm(a, mu, sigma)- pnorm(mu - 3 * sigma, mu, sigma)),4)
  message(area)
}

