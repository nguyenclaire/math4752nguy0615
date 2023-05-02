#' @title myhyper
#' myhyper function from lab 5.
#' This function forms a barplot of the trials and successes
#'
#' @param iter iterations
#' @param N size
#' @param r amount of a certain choice
#' @param n amount of times you'll choose
#'
#' @return results as a barplot
#' @export
#'
#' @examples
#' myhyper(100, 20, 12, 5)
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  graphics::barplot(succ.tab/(iter), col=grDevices::rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}
