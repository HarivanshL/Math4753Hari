#' Function to grab the p binomial
#'
#'
#'
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis barplot curve hist layout lines points polygon segments text
#' @importFrom stats dnorm dpois pbinom pnorm quantile rpois t.test var
#' @return p_binom(success, trials, prob)
#'
#' @example check_p_binom(8, 15, 0.4)
#'
#' @export
check_p_binom <- function(success, trials, prob){
  return(pbinom(success, trials, prob))
}
