#' plot_bandit_posterior
#'
#' Generates a plot that shows the bandit posterior values as they are sequentially updated 
#' by the provided win / loss data.
#'
#' @param data data frame containing win loss data
#' @param prior prior vector containing the probabilites of Machine 1 and Machine 2 being good, defaults to 50-50.
#' @param win_probs vector containing the probabilities of winning on the good and bad machine respectively.
#' @examples
#' data = data.frame(machine = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), 
#'                   outcome = c("W", "L", "W", "L", "L", "W", "L", "L", "L", "W"))
#' plot_bandit_posterior(data)
#'
#' @export


plot_bandit_posterior = function(data, prior = c(m1_good=0.5,m2_good=0.5), win_probs = c(good=1/2,bad=1/3))
{
  r = matrix(NA, ncol=2, nrow=nrow(data)+1)
  r[1,] = prior
  for(i in 1:nrow(data))
  {
    r[i+1,]  = bandit_posterior(data[1:i,], prior, win_probs)
  }

  plot(0,0,type='n',xlab="Play #",ylab="Posterior Prob.",xlim=c(0,nrow(data)),ylim=c(0,1))
  lines(0:nrow(data),r[,1],lwd=2,col="blue")
  lines(0:nrow(data),r[,2],lwd=2,col="orange")
  legend("topleft",c("P(M1|Data)","P(M2|Data)"),lwd=2,col=c("blue","orange"))
}