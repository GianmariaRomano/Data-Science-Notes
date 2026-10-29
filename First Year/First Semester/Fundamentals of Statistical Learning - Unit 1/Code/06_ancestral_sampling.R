# Lab Session 06 - Ancestral Sampling -------------------------------------

# Scale Mixture of Gaussians ----------------------------------------------

# Perform a single run of the model.
nu <- 1.5 # Pick a low value to have heavier tails.
lambda <- rgamma(n = 1, shape = nu / 2, rate = nu / 2)
obs <- rnorm(n = 1, 0, 1 / sqrt(lambda))
c(lambda, obs)

# Repeat the simulation for 100000 times.
set.seed(213) # For reproducibility.
nu <- 1.5
M <- 10^6

out <- matrix(NA, ncol = 2, nrow = M)
for (m in 1:M){
  out[m, 1] <- rgamma(n = 1, shape = nu / 2, rate = nu / 2)
  out[m, 2] <- rnorm(n = 1, 0, 1 / sqrt(out[m, 1]))
}
head(out)

hist(out[, 2], prob = T, breaks = 50,
     col = "gray85", border = "white",
     main = "Simulation Results - Scale Mixture of Gaussians",
     xlab = "x", ylab = "f(x)")

# While the plot looks weird, the distribution appears to be symmetric and
# heavy-tailed, suggesting that it does resemble a t distribution.

# Visualize the distribution in better detail by taking a smaller support.
hist(out[, 2], prob = T, breaks = 100000,
     col = "gray85", border = "white",
     xlim = c(-12, 12),
     main = "Simulation Results - Scale Mixture of Gaussians",
     xlab = "x", ylab = "f(x)")
curve(dt(x, nu), add = T, col ="red", lwd = 2, n = 501)
legend("topright",
       legend = "Theoretical Student-t",
       col = "red",
       lwd = 2, 
       bty = "n", cex = .7)

# Alternatively, compare theoretical and empirical quantiles.
qlev <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.95, 0.99)
emp <- round(quantile(out[, 2], qlev), 2)
true <- round(qt(qlev, df = nu), 2)
cbind(emp, true)

# A QQ-plot allows to visually compare theoretical and empirical quantiles.
p <- ppoints(M)
empirical_q <- sort(out[,2])
theoretical_q <- qt(p, nu)
plot(theoretical_q, empirical_q,
     pch = 16, cex = 0.5,
     col = rgb(0, 0, 0, 0.5),
     main = "Student-t Q-Q Plot",
     xlab = "Theoretical Quantiles",
     ylab = "Empirical Quantiles")
abline(0, 1, col = rgb(1,0,0,.25), lwd = 8)
summary(out[, 2]) # Consider the ground-truth as a reference.

# Hypothesis Testing for Ancestral Sampling -------------------------------

# In a more formal sense, it is possible to check whether the model resembles a
# t distribution using a many-to-one transformation that, for an independent and
# identically distributed sample DM = {x1, ..., xM} from F, indicates the maximum
# value under the ground-truth assumption that each data point is independent and
# identically distributed from a t distribution with 1.5 degrees of freedom.

# Assuming X(M) = max{X1, ..., XM}, where each Xi is independent and identically
# distributed from a t distribution with 1.5 degrees of freedom, the goal is to
# evaluate P(X(M) >= xM) = 1 - F(x(M)) under the ground-truth.
# In fact, if the probability of seeing something even more extreme than x(M) is
# very small, the sample acts as evidence against the ground-truth assumption.

# In this case, let F(x) = P(max{X1, ..., XM} <= x) = P(X1 <= x, ..., XM <= x):
# by the IID assumption, this becomes F(x) = \prod P(Xi <= x) = \prod Fx(x) = (Fx(x))^M.
x_M <- max(out[, 2])
p_value <- 1 - (pt(x_M, df = 1.5))^M # Use p = 1 - F(x) = 1 - (Fx(x))^M

# Assuming a significance level of 0.05, the sample acts as evidence in favour
# of the hypothesis that the model follows a t distribution.

# This result can be double-checked via simulation by repeatedly constructing a
# random sample and running the test on each data sample.