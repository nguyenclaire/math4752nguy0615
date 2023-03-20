#' mysample function from lab 5.
#' This is my function.
#' This is what it does.
#' @title mysample
#'
#' @param n desc
#' @param iter desc
#' @param time desc
#'
#' @return desc
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
