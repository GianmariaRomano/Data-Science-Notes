# Lab Session 03 - Simulating Generic Populations -------------------------

lcg <- function(u0, n=1000, a=69069069, b=12345, M=2^32){
  # Start by allocating the output.
  x_out <- rep(NA, n) # Create a vector of n NA values.
  x_out[1] <- u0
  
  # Iterate to generate every other term of the sequence.
  for (i in 2:n) {
    x_out[i] <- (a + (b * x_out[i - 1])) %% M
  }
  
  # Return the output vector.
  return(x_out / (M + 1))
}

run <- lcg(78910, n=100000)

# Continuous Random Variables ---------------------------------------------

# Let X be a random variable with probability density function fx = 3x^2 in [0, 1].
fx <- function(x) (3 * x^2) * (x > 0) * (x < 1)
curve(fx(x), -1, 2, lwd=3, col="blue")

# Recover the cumulative distribution function of X.
Fx <- function(x) (x^3) * (x > 0) * (x < 1) + (x > 1)
curve(Fx(x), -1, 2, lwd=3, col="green")

# Lastly, plot the quantile function associated to X.
Qx <- function(p) (p^(1 / 3))
curve(Qx, lwd=4, col="pink")

# Get a sample and check whether it comes from the indicated population.
data <- Qx(run)
hist(data, prob=T, col="purple", border="white")
rug(data)
curve(fx(x), lwd=9, col="pink", add=T)