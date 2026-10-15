# Lab Session 05 - Exercise on Simulations --------------------------------

# Suppose that X ~ Unif(0, 1) and that you independently draw {Y1, Y2, Y3, ...}
# from another Unif(0, 1) variable until you reach the random stopping time T
# such that YT < X.

# Question 1 --------------------------------------------------------------

# Determine the distribution of T conditioned on {X = x}.

# Assuming E = {YT < X}, the random variable T denotes the number of trials until
# event E occurs.
# This means that, for any natural number t, the event {T = t} corresponds to
# a sequence of (t - 1) failures, where Yi >= X, before the first success at step T.

# Knowing that each draw Yi is independent and identically distributed, if p
# is the probability that event E occurs, it is possible to state that
# P(T = t|X = x) = (1 - p)^{t - 1}p, meaning that (T|X = x) ~ Geom(p).

# More specifically, knowing that each Yi follows a Unif(0, 1) distribution, it
# is possible to retrieve the parameter p = P(Unif(0, 1) < x) = x.

# Question 2 --------------------------------------------------------------

# Determine the marginal distribution of T.

# Let f(x, t) = f(t|x)f(x) and marginalize T by mixing the joint distribution
# with respect to all possible values of x.

# Knowing that X ~ Unif(0, 1), with SX = [0, 1], and (T|X) ~ Geom(x), the goal
# is to solve \int_0^1 (1 - x)^{t - 1}x dt, which is a Beta integral equal to
# (1! x (t - 1)!) / ((t + 1)!) = 1/(t(t + 1)).
# This means that P(T = t) = 1/(t(t + 1)), for any natural number t.

# Question 3 --------------------------------------------------------------

# Write a simulation to numerically check the marginal distribution of T.

# Create a vector of 10000 NA values to keep track of the simulated stopping times.
M <- 10000
times <- rep(NA, M)

# Run each simulation sequentially.
for (m in 1:M){
  x <- runif(1)
  y <- runif(1)
  t <- 1
  
  # Stop whenever the event E is triggered.
  while (y > x){
    t <- t + 1
    y <- runif(1)
  }
  
  # Store the stopping time of simulation E.
  times[m] <- t
}

# For readability purposes, filter simulated values up to a chosen value.
max_t <- 15
vis_times <- times[times <= max_t]

# Plot histogram with breaks centered around integers.
hist(vis_times, 
     breaks = seq(0.5, max_t + 0.5, by = 1), 
     prob = TRUE, 
     col = "mediumpurple1", 
     border = "white",
     main = "Simulation Results for T", 
     xlab = "t", 
     ylab = "P(T = t)")

# Overlay theoretical points and connecting line.
t_vals <- 1:max_t
true_pmf <- 1 / (t_vals * (t_vals + 1))

points(t_vals, true_pmf, col = "firebrick", pch = 19, cex = 1.2)
lines(t_vals, true_pmf, col = "firebrick", lwd = 2)

legend("topright", legend = c("Simulation", "True PMF"),
       col = c("mediumpurple1", "firebrick"), 
       pch = c(15, 19), lwd = c(NA, 2), pt.cex = 1.2)