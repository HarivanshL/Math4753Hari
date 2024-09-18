#' Function to grab the p binomial
#'
#'
#'
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#'
#' @return p_binom(success, trials, prob)
#'
#' @example check_p_ binom(8, 15, 0.4)
#'
#' @export
check_p_binom <- function(success, trials, prob){
  return(pbinom(success, trials, prob))
}
