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

# Distribution Check ------------------------------------------------------

# It is possible to check if the sequence follows a Unif(0, 1) distribution by
# plotting a histogram of the generated values.
# Note: Setting prob=T provides a density plot.
par(mfrow = c(1, 2)) # For sideways plots.
hist(run01, prob=T)
hist(run02)

# Try running the simulation on larger samples, also using a different seed.
run03 <- lcg(9856, n=10000)
hist(run03, prob=T, breaks=20)
rug(run03)
abline(h=1, lwd=4, col="peachpuff")

run04 <- lcg(9856, n=100000)
hist(run04, prob=T, breaks=20)
rug(run04)
abline(h=1, lwd=4, col="peachpuff")

run05 <- lcg(78910, n=100000)
hist(run05, prob=T, breaks=20)
rug(run05)
abline(h=1, lwd=4, col="peachpuff")

# Independence Check ------------------------------------------------------

# One can visually check for independence using a random scatter that plots each
# term in the generated sequence against the previous one.
N = length(run05)
plot(x = run05[1:N-1], y=run05[2:N], pch = ".")

# Comments on Performance Analysis ----------------------------------------

# One can formally assess the quality of a linear congruential generator by
# checking the distance between its probability density function and the one of
# the target Unif(0, 1) distribution.

# For a more reliable analysis, a commonly used distance metric is the Chebyshev
# distance, which generalizes the Lp distance family by taking the maximum
# absolute difference between the two function on any point.

# In particular, the linear congruential generator will be good if, for any data
# sequence, the loss approaches 0 as the sequence length grows infinitely large.
# A weaker condition instead asks that the risk, which denotes the average loss
# over all possible data sequences approaches 0 as the sequence length grows
# infinitely large.

# Qualitative Idea: If the ground-truth is not known, it is necessary to ensure
#                   that, for any sensible choice of the ground-truth, the
#                   supremum of the risk approaches 0 as the sequence length
#                   grows infinitely large.

# Quantitative Idea: One should take into consideration the rate at which this
#                    convergence occurs with respect to the sample size.

# Native PRNG in R --------------------------------------------------------

?set.seed