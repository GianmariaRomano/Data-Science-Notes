# Lab Session 04 - Exercises on Transformations ---------------------------

# Choose plotting layout.
par(mfrow = c(1, 1)) # For single plots.
par(mfrow = c(2, 1)) # For side-by-side plots.

# Exercise 1 --------------------------------------------------------------

# Given X ~ Unif(-1,1), plot the CDF and PDF of Y = X^2.

# Evaluate `sqrt(abs(y))` to avoid having `sqrt(y) = NaN` whenever y < 0.
# Alternatively, it is possible to use `if` or `ifelse` instead.
Fy <- function(y) (sqrt(abs(y))) * (y > 0) * (y <= 1) + (y > 1)
fy <- function(y) (1 / (2 * sqrt(abs(y)))) * (y > 0) * (y < 1)

curve(fy(x), from = -1, to = 2, lwd = 4, col = "gold",
      main = "PDF of Y", xlab = "y", ylab = "fy(y)",
      n = 301, ylim = c(0.0, 1.0))

curve(Fy(x), from = -1, to = 2, lwd = 4, col = "gold",
      main = "CDF of Y", xlab = "y", ylab = "Fy(y)",
      n = 301, ylim = c(0.0, 1.0))

# Exercise 2 --------------------------------------------------------------

# Get analytically the quantile function of Y and plot it.

# Assuming Fy(y) is invertible, it is sufficient to let Qy(p) = Fy^{-1}(p).
Qy_analytically <- function(p) (p^2) * (p >= 0) * (p <= 1)

curve(Qy_analytically(x), from = -1, to = 2, lwd = 4, col = "cyan",
      main = "Quantile Function of Y - Analytically", xlab = "p", ylab = "Qy(p)",
      n = 301, ylim = c(0.0, 1.0))

# It is possible compare the CDF with its quantile function with a manuel plot.
y_grid <- seq(from = -1, to = 2, length.out = 301)
F_grid <- Fy(y_grid)
plot(y_grid, F_grid, type = "l", lwd = 3, col = "skyblue") # Plot Fy(y).
lines(F_grid, y_grid, type = "l", lwd = 3, col = "cyan") # Add Qy(p).
abline(v = 1, col = gray(.75), lty = 3, lwd = 3)
legend("topleft",
       c("CDF", "Quantile Function"),
       lwd = 3, col = c("skyblue", "cyan"), bty = "n", cex = .8)

# Exercise 3 --------------------------------------------------------------

# Get numerically the quantile function of Y and plot it.

# If Fy(y) is known, it is possible to use the `uniroot` command to plot the
# root of Fy(yp) - p = 0.
equation <- function(yp, p) (Fy(yp) - p)
Qy_numerically <- function(p){
  # Input check: If p < 0 or p > 1, return 0.
  if ((p < 0) || (p > 1)){
    root <- 0
  } else{
    root <- uniroot(equation, c(0, 1), p = p)$root
  }
  return(root)
}

# Safe version that returns NA in case the `uniroot` function fails.
Qy_numerically_safe <- function(p){
  # Input check: If p < 0 or p > 1, return 0.
  if ((p < 0) || (p > 1)){
    root <- 0
  } else{
    root <- tryCatch(uniroot(equation, c(0, 1), p = p)$root,
                     error = function(e) NA)
  }
  return(root)
}

# To avoid errors during plotting, it is necessary to vectorize the function with
# respect to the parameter p in order to properly use the input check.
Qy_numerically_v <- Vectorize(Qy_numerically, "p")

curve(Qy_numerically_v(x), from = -1, to = 2, lwd = 4, col = "cyan",
      main = "Quantile Function of Y - Numerically", xlab = "p", ylab = "Qy(p)",
      n = 301, ylim = c(0.0, 1.0))

# If Fy(y) is not known, it is necessary to recover it using the `integrate`
# command before using the `uniroot` command to find the quantile.

# Exercise 4 --------------------------------------------------------------

# Use the quantile transform method to simulate 1000 samples from Fy.
# Check if it is a good sample with basic visualization tools.

# For simplicity, construct the sample using a linear congruential generator.
lcg <- function(u0, n=1000, a=69069069, b=12345, M=2^32){
  x_out <- rep(NA, n)
  x_out[1] <- u0
  
  for (i in 2:n) {
    x_out[i] <- (a + (b * x_out[i - 1])) %% M
  }
  
  return(x_out / (M + 1))
}

# Create the sample using the linear congruential generator.
u_sample <- lcg(253, n=1000)
hist(u_sample, prob = T, col = "purple", border = "white",
     main = "Samples From LCG")

# Simulate a sample from Fy via quantile transform and compare its distribution
# with fy(y).
y_sample <- Qy_analytically(u_sample)
hist(y_sample, prob = T, col = "purple", border = "white",
     main = "Samples From Y", xlab = "y")
rug(y_sample)
curve(fy(x), lwd = 4, col = "pink", add = T)

# Alternatively, it is possible to evaluate the quality of the sample distribution
# using its empirical cumulative distribution function.
plot(ecdf(y_sample), lwd = 3, col = "purple")
rug(y_sample)
curve(Fy(x), lwd = 6, col = "pink", add = T)

# Defining the Empirical CDF and Histograms -------------------------------

# From a broader perspective, given a random sample Dn = {D1, ..., Dn} ~ Fx i.i.d.,
# the empirical cumulative distribution function Fn is the cumulative distribution
# of the discrete distribution that places mass 1/n for each data point.
# This means that Fn = (sum of 1(xi <= x)) / n.

# Similarly, on a support Sx that is divided into bins B1, ..., Bn, each of size
# h, the histogram of the distribution can be defined as follows:
# f = (proportion in bin) / (value of bin) = ((sum of 1(x belongs to Bj)) / n) / h
# Furthermore, this definition can be generalized for multi-dimensional supports
# to create multivariate histograms.
# On d dimensions, f = ((sum of 1(x belongs to Bj)) / n) / (h^d).

# It should be noticed that, for this type of tasks, it is necessary to assume
# that the ground-truth distribution is described by a function that is
# homogeneously smooth in its support, meaning that information will be naturally
# spread across fixed-size bins.

# Kernel Density Estimation -----------------------------------------------

# One of the issues of learning a probability density function using a histogram
# lies in the fact that a step function would be used to approximate a smooth
# function.
# For this reason, it can be more convenient to perform kernel density estimation,
# which consists of applying a smoothing kernel to a discrete probability mass
# and evaluating the density as the sum of the spread-out masses, resulting in
# f = ((sum of k(x - xi) / h) / h) / n, where h is the bandwidth of the kernel.

y_sample <- Qy_analytically(u_sample)
hist(y_sample, prob = T, col = "purple", border = "white",
     main = "Samples From Y", xlab = "y")
rug(y_sample)
curve(fy(x), lwd = 4, col = "pink", add = T)
lines(density(y_sample), lwd = 4, col = "violet", add = T)

# For this method, the issues lies in the fact that the kernel might spread some
# masss outside the support of the distribution.