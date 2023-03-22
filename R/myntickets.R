#' Title Find the minimum amount of tickets needed
#'
#' @param N number of seats
#' @param gamma probability the plane will truly be overbooked
#' @param p probability of a "show"
#'
#' @return a continuous and discrete graph and a list containing the discrete distribution
#' and normal approximation
#' @importFrom stats pbinom optimize
#' @importFrom graphics abline
#' @export
#'
#' @examples ntickets(400, 0.02, 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {

  n <- seq(N, floor(N + N/10), by = 1)
  nd <- 1 - gamma - pbinom(q = N, size = n, prob = p) # discrete distribution
  nc <- 1 - gamma - pnorm(N + 0.5,  n * p, sqrt(n * p * (1 - p))) # continuous distribution

  ind <- which.min(abs(nd)) # discrete

  # Define the objective function for the normal approximation
  f <- function(n) {
    abs(1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))))
  }

  # Find the value of n that minimizes the objective function for the normal approximation
  ind2 <- optimize(f, interval = c(N, floor(N + N/10)))$minimum

  x <- list(nd = n[ind], nc = ind2, N = N, p = p, gamma = gamma)
  print(x)

  plot(n, nd, type = 'b', main=paste("Objective Vs n to find optimal tickets sold\n", "(", n[ind], ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective", pch = 16, col = "blue")
  abline(h = 0, v = n[ind], lwd = 2, col = "red")

  plot(n, nc, type = 'l', main=paste("Objective Vs n to find optimal tickets sold\n", "(", ind2, ")", "gamma = ", gamma, "N = ", N, "continuous"), ylab = "Objective", col = "red")
  abline(h = 0, v = ind2, col = "blue", lwd = 2)
}
