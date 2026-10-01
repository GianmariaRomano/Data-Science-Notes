# Lab Session 02 - Stochastic Simulations ---------------------------------

# Loops -------------------------------------------------------------------

# A loop is a code that performs some task until a certain event.

# For instance, a `while` loop runs as long as a condition is true.
x = 10
i = 0
while (i < x) {
  i <- i + 2
}

# On the other hand, a `for` loop iterates through a collection of items.
y = 10
for (j in 1:y) {
  j
}

# Pseudorandom Number Generators ------------------------------------------

# Implement a Linear Congruential Generator as a custom function.
# Note: The default implementation creates a sequence of 1000 numbers with the
#       specified values for a, b and M.
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

# Run the `lcg` function and plot the result to check for correctness.
run01 <- lcg(26)
head(run01)
tail(run01)

run02 <- lcg(9856)

# Use a histogram to check if the sequence follows a Unif(0, 1) distribution.
# Note: Setting prob=T provides a density plot.
par(mfrow = c(1, 2)) # For sideways plots.
hist(run01, prob=T)
hist(run02, prob=T)