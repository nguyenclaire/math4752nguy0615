#' @title Maximum Log-Likelihood Estimation
#'
#' Description:
#' This function estimates the maximum log-likelihood for a given likelihood function
#' and a given dataset. It also produces a plot of the log-likelihood function with
#' respect to the parameter values.
#'
#' Usage:
#' mymaxlik(lfun, x, param, ...)
#'
#' Arguments:
#' @param lfun   A likelihood function that takes two arguments: a data point and a parameter value.
#'               The function should return the log-likelihood for the given data point and parameter value.
#' @param x      A vector of data points for which the maximum log-likelihood should be estimated.
#' @param param  A vector of parameter values at which the likelihood function should be evaluated.
#' @param ...    Additional graphical parameters to be passed to the plot function.
#'
#' @importFrom graphics axis points
#'
#' @return
#' A list containing the following elements:
#' - i:      The index of the maximum log-likelihood in the 'y' vector.
#' - parami: The parameter value that corresponds to the maximum log-likelihood.
#' - yi:     The maximum log-likelihood value.
#' - slope:  A vector of slopes around the maximum log-likelihood point. This can be used
#'           as a diagnostic tool to check if the slope changes sign around the maximum point.
#'           If the slope changes sign from positive to negative, this indicates a maximum.
#' @export
#'
#' @examples
#'
#' logbin=function(x,param) log(dbinom(x,prob=param,size=20))
#' y <- c(3,3,4,3,4,5,5,4)
#' mymaxlik(lfun = logbin, x = y, param = seq(0, 1, length = 1000), main = "Binomial, n = 20")
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
