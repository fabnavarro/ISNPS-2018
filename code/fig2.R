# fig2 - produces Figure 2. of the paper:
#
# Linear wavelet estimation in regression
# with additive and multiplicative noise.
#
#Copyright (c) 2018 C. Chesneau, J. Kou and F. Navarro

rm(list = ls())
library(rwavelet)
source("makedata.R")
source("rescale.R")
source("twoFCVlinear.R")

Pn <- function(x0, x) {
  apply((x - x0)^2, 1, mean)
}

n <- 4096
signal_names <- c("HeaviSine", "Ramp", "Bumps")
J <- log2(n)
j0 <- 0
D <- 2^(1:(J - 1))
filter <- "Daubechies"
qmf <- MakeONFilter(filter, 8)

for (i in 1:length(signal_names)) {
  res <- makedata(n, signal_names[i])
  Y <- res$Y^2 * 3
  f <- res$f^2
  wc <- FWT_PO(Y, j0, qmf)

  cv <- twoFCVlinear(n, Y, j0, qmf, D, wc, res$sigma_v)
  hat_m_2FCV <- which.min(cv$CritCV)
  hat_s_m_2FCV <- cv$hat_s_m[hat_m_2FCV, ]

  err <- Pn(cv$hat_s_m, repmat(f, length(D), 1))
  m_star <- which.min(err)
  hat_s_m_star <- cv$hat_s_m[m_star, ]

  plot(res$X, res$f^2, type = "l", col = "blue",
       lwd = 2, axes = FALSE, ann = FALSE, lty = 1)
  axis(1, at = seq(0, 1, length.out = 6))
  axis(2, las = 0)
  box()
  matlines(res$X, t(cv$hat_s_m[c(9, 5, 1), ]), type = "l",
           lwd = 2, lty = 1, col = c("gray88", "gray50", "gray10"))
  matlines(res$X, res$f^2, col = "blue", lwd = 2, lty = 2)
  title(xlab = "X")
  title(ylab = "f^2")
  plot_colors <- c("blue", "gray88", "gray50", "gray10")
  text <- c("f^2", "r_{9,n}", "r_{5,n}", "r_{1,n}")
  legend("topright", legend = text, cex = 1,
         fill = plot_colors, ncol = 1, xpd = NA)

  plot(res$X, res$f^2, type = "l", col = "blue",
       lwd = 2, axes = FALSE, ann = FALSE, lty = 1)
  axis(1, at = seq(0, 1, length.out = 6))
  axis(2, las = 0)
  box()
  matlines(res$X, hat_s_m_2FCV, type = "l", lwd = 2, lty = 1, col = "red")
  matlines(res$X, res$f^2, col = "blue", lwd = 2, lty = 2)
  title(xlab = "X")
  title(ylab = "f^2")
  plot_colors <- c("blue", "red")
  text <- c("r", "r_{j_0,n}")
  legend("topright", legend = text, cex = 1,
         fill = plot_colors, ncol = 1, xpd = NA)

  rCrit_CV <- rescale(cv$CritCV, min(err), max(err))
  plot(1:11, err, type = "o", col = "blue",
       lwd = 2, axes = FALSE, ann = FALSE, lty = 1)
  axis(1, at = 1:11)
  axis(2, las = 0)
  box()
  matlines(1:11, rCrit_CV, type = "o", lwd = 2,
           lty = 1, pch = 22, col = c("red"))
  title(xlab = "j_0")
  title(ylab = "MSE(r,r_{j_0,n})", cex.lab = 1.2)
  plot_colors <- c("blue", "red")
  text <- c("MSE(r,r_{j_0,n})", "2FCV(j_0)")
  legend("topright", legend = text, cex = 1, ncol = 1,
         pch = 21:22, col = c("blue","red"), lwd = 2, lty = 1)
}
