# fig1 - produces Figure 1. of the paper:
#
# Linear wavelet estimation in regression
# with additive and multiplicative noise.
#
#Copyright (c) 2018 C. Chesneau, J. Kou and F. Navarro

rm(list = ls())
library(rwavelet)
source("makedata.R")

n <- 4096
signal_names <- c("HeaviSine", "Ramp", "Bumps")
for (i in 1:length(signal_names)) {
  res <- makedata(n, signal_names[i])
  plot(res$X, res$f^2, type = "l", col = "blue",
       lwd = 2, axes = FALSE, ann = FALSE, lty = 1)
  axis(1, at = seq(0, 1, length.out = 6))
  axis(2, las = 0)
  box()
  title(xlab = "X")
  title(ylab = "f^2")
  plot(res$X, res$Y^2, col = "gray", axes = FALSE, ann = FALSE, lty = 1)
  axis(1, at = seq(0, 1, length.out = 6))
  axis(2, las = 0)
  box()
  matlines(res$X, res$f^2, col = "blue", lwd = 2, lty = 2)
  title(xlab = "X")
  title(ylab = "Y^2")
}
