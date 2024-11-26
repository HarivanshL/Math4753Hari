#' Calculates the number of tickets sold  when there are N seats available,
#' p is the probability a person will show, and gamma is the probability
#' a plane will be truly overbooked
#'
#'
#'
#' @param N total number of seats available
#' @param gamma the probability that a plane will be overbooked
#' @param p is the probability that a person will show
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis barplot curve hist layout lines points polygon segments text
#' @importFrom stats dnorm dpois pbinom pnorm quantile rpois t.test var
#' @example ntickets(100, gamma, p)
#'
#' @export
ntickets <- function(N, gamma, p){


  #Discrete Distribution
  n <- seq(N, floor(N + N/10), by=1)
  #Objective function
  obj_disc <- 1 - gamma - pbinom(q=N, size= n, prob = p)

  #Finding index of minimum value
  ind_min_disc <- which.min(abs(obj_disc))

  #Plotting Discrete Distribution
  title1 <- paste('Objective Vs n to find optimal tickets sold (', n[ind_min_disc], ') gamma = ', gamma, 'N = ', N, '(Discrete)')
  plot(n, obj_disc, ylab='Objective', xlab='n', main=title1,cex.main=0.7)
  lines(n,obj_disc)
  #Adding vertical and horizontal lines
  abline(h=0, v=n[ind_min_disc], col='red')



  #Continuous Distribution
  n1<- seq(N, floor(N +(N/10)), by=0.01)
  mean_normal <- n1*p
  sd_normal <- sqrt(mean_normal * (1-p))
  #Objective Function for normal distribution
  obj_cont <- 1 - gamma - pnorm(N +0.5,mean=mean_normal,sd=sd_normal)

  #Finding index of minimum value
  ind_min_cont <- which.min(abs(obj_cont))

  #Plotting Continuous Distribution
  title2 <- paste('Objective Vs n to find optimal tickets sold (', n1[ind_min_cont], ') gamma = ', gamma, 'N = ', N, '(Continuous)')
  plot(n1, obj_cont, ylab='Objective', xlab='n', main=title2, col='blue',cex.main=0.7)
  #Adding vertical and horizontal lines
  abline(h=0, v=n1[ind_min_cont], col='blue')

  list(nd=n[ind_min_disc], nc=n1[ind_min_cont], N=N, p=p, gamma=gamma)
}
