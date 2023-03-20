#' Title Find the minimum amount of tickets needed
#'
#' @param N number of seats
#' @param gamma probability the plane will truly be overbooked
#' @param p probability of a "show"
#'
#' @return a continuous and discrete graph and a list containing the discrete distribution
#' and normal approximation
#' @importFrom stats pbinom
#' @importFrom graphics abline
#' @export
#'
#' @examples ntickets(400, 0.02, 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  n <- seq(N, floor(N + N/10), by = 1)
  nd <- 1 - gamma - pbinom(q = N, size = n, prob = p) # discrete distribution
  nc <- 1 - gamma - pnorm(N + 0.5,  n * p, sqrt(n * p * (1 - p))) # continuous distribution

  x <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  cat("LIST:\n")
  print(x)

  ind <- which.min(abs(nd))
  ind2 <- which.min(abs(nc))

  plot(n, nd, type = 'b', main=paste("Objective Vs n to find optimal tickets sold\n", "(", n[ind], ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective", pch = 16, col = "blue")
  abline(h = 0, v = n[ind], lwd = 2, col = "red")

  plot(n, nc, type = 'l', main=paste("Objective Vs n to find optimal tickets sold\n", "(", n[ind2], ")", "gamma = ", gamma, "N = ", N, "continuous"), ylab = "Objective", col = "red")
  abline(h = 0, v = n[ind2], col = "blue", lwd = 2)

}
