#' @title myncurve
#'
#' @param a Upper bound
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return plotted curve with area shaded in
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' myncurve(a=1, mu=2, sigma=3)
myncurve = function(a, mu, sigma){
  x <- seq(mu - 3*sigma, mu + 3*sigma, length = 1000)
  curve(dnorm(x, mean = mu,sd = sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(xcurve,a),c(ycurve,0),col="Purple")
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(-Inf,mean=mu,sd=sigma)
  probr=round(prob,4)

  text(x = mu, y = 0.5*dnorm(mu, mu, sigma), paste("Area = ", probr))
}
