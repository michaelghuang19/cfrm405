# Michael Huang
# Homework 7
# Adekoya
# Problem 2

# This is an optimization problem to maximize the expected returns of
# a portfolio subject to constraint on risk

# 2(a)

# define the vector of expected returns 
mu <- c(0.08, 0.10, 0.13, 0.15, 0.20)

# define the covariance matrix
Sigma <- matrix(c(0.019600,-0.007560,0.012880,0.008750,-0.009800,
                -0.007560,0.032400,-0.004140,-0.009000,0.009450,
                0.012880,-0.004140,0.052900,0.020125,0.020125,
                0.008750,-0.009000,0.020125,0.06250,-0.013125,
                -0.009800,0.009450,0.020125,-0.013125,0.122500),
                5, 5)

# define the target risk
sigmaP2 <- 0.25

# Define G(w,\lambda)
G<- function(x, mu, Sigma, sigmaP2)
{
  n <- length(mu)
  c(mu + rep(x[n+1], n) + 2*x[n+2]*(Sigma %*% x[1:n]),
    sum(x[1:n]) - 1,
    t(x[1:n]) %*% Sigma %*% x[1:n] - sigmaP2)
}


# Define gradient of G, DG(w,\lambda)
DG <- function(x, mu, Sigma, sigmaP2)
{
  n <- length(mu)
  grad <- matrix(0.0, n+2, n+2)
  grad[1:n, 1:n] <- 2*x[n+2]*Sigma
  grad[1:n, n+1] <- 1
  grad[1:n, n+2] <- 2*(Sigma %*% x[1:n])
  grad[n+1, 1:n] <- 1
  grad[n+2, 1:n] <- 2*t(x[1:n]) %*% Sigma
  grad
}

# 2(b)

# Executing Newton's method 

# starting points
x <- c(rep(0.5, 5), 1, 1)
u <- rep(1, length(x))

# Newton Iterations
while(sqrt(sum(u^2)) / sqrt(sum(x^2)) > 1e-6) {
  u <- solve(DG(x, mu, Sigma, sigmaP2),
             G(x, mu, Sigma, sigmaP2))
  x <- x - u
}

# display the solution
x

# asset proportions are therefore
x[1]
x[2]
x[3]
x[4]
x[5]
 
# 2(c)
# confirm if we do indeed have maximum at the resultant x
# take the upper left nxn block of DG and find the eigenvalues
DG(x, mu, Sigma, sigmaP2)[1:5,1:5]
eigen(DG(x, mu, Sigma, sigmaP2)[1:5,1:5])
# all negative, so we can verify it is the maximum.

# calculate the portfolio return mu^Tw
t(x[1:5]) %*% mu
