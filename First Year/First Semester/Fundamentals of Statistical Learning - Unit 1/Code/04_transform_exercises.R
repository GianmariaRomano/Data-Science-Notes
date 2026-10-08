# Lab Session 04 - Exercises on Transformations ---------------------------

# Choose plotting layout.
par(mfrow = c(1, 1)) # For single plots.
par(mfrow = c(2, 1)) # For side-by-side plots.

# Exercise 1 --------------------------------------------------------------

# Given X ~ Unif(-1,1), plot the CDF and PDF of Y = X^2.

# Evaluate `sqrt(abs(y))` to avoid having `sqrt(y) = NaN` whenever y < 0.
# Alternatively, it is posible to use `if` or `ifelse` instead.
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

# Assuming Fy(y) is invertible, just let Qy(p) = F^{-1}(p).
Qy_analytically <- function(p) (p^2)

curve(Qy_analytically(x), from = 0, to = 1, lwd = 4, col = "skyblue",
      main = "Quantile Function of Y - Analytically", xlab = "p", ylab = "y",
      n = 301, ylim = c(0.0, 1.0))

# Exercise 3 --------------------------------------------------------------

# Get numerically the quantile function of Y and plot it.

# If Fy(y) is unknown or not invertible, it can be retrieved numerically by
# using commands such as `uniroot` or `integrate`.

Qy_num <- function(p){
  uniroot(function(y) Fy(y) - p, interval = c(0, 1))$root
}
Qy_numerically <- Vectorize(Qy_num) # For compatibility with `curve`.

curve(Qy_numerically(x), from = 0, to = 1, lwd = 4, col = "skyblue",
      main = "Quantile Function of Y - Numerically", xlab = "p", ylab = "y",
      n = 301, ylim = c(0.0, 1.0))

# Exercise 4 --------------------------------------------------------------

# Use the quantile transform method to simulate 1000 samples from Fy.
# Check if it is a good sample with basic visualization tools.