makedata <- function(n, name) {
  X <- sort(runif(n))
  if (name == "HeaviSine") {
    sig <- 4 * sin(4 * pi * X)
    sig <- sig - sign(X - 0.3) - sign(0.72 - X)
  }
  if (name == "Ramp") {
    sig <- X - (X >= 0.37)
  }
  if (name == "Bumps") {
    pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
    hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
    wth <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)
    sig <- 2 * rep(0, length(t))
    for (j in 1:length(pos)) {
      sig <- sig + hgt[j]/((1 + (abs(X - pos[j])/wth[j]))^4)
    }
  }
  V <- rnorm(n)
  U <- runif(n, -1, 1)
  sigma_v <- 0.01
  Y <- U * sig + sigma_v * V
  return(list(X = X, Y = Y, f = sig, sigma_v = sigma_v))
}
