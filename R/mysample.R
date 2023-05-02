#' My Sample Function
#'
#' This function generates bar plots of random samples taken from a discrete
#' distribution with integer values ranging from 1 to 10. The function iteratively
#' creates samples, converts them to factors, and then plots the relative
#' frequencies of each value in the form of a bar plot.
#'
#' @title My Sample Function
#'
#' @param n An integer representing the sample size for each iteration.
#' @param iter An integer representing the number of iterations to perform.
#' @param time A numeric value representing the time in seconds between each iteration.
#'
#' @return This function does not return any value. It generates bar plots for each iteration.
#' @export
#'
#' @examples
#' mysample(1, 10, 0.5)
mysample=function(n=1, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    graphics::barplot(table(sf)/n,beside=TRUE,col=grDevices::rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    #release the table
    Sys.sleep(time)
  }
}
