# Michael Huang
# Homework 7
# Adekoya
# Problem 3

# This is an optimization problem to minimize the variance of
# a portfolio subject to constraint on risk

# 3(b)

# define the vector of expected returns 
mu <- c(0.08,0.10,0.13,0.15,0.20)

# define the covariance matrix
Sigma <- matrix(c(0.019600,-0.007560,0.012880,0.008750,-0.009800,
                -0.007560,0.032400,-0.004140,-0.009000,0.009450,
                0.012880,-0.004140,0.052900,0.020125,0.020125,
                0.008750,-0.009000,0.020125,0.06250,-0.013125,
                -0.009800,0.009450,0.020125,-0.013125,0.122500),
                5, 5)

# define the target risk
sigmaP2 <- 0.0225

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

# Executing Newton's method 

# starting points
x <- c(rep(1, 5), 1, 1)
u <- rep(1, length(x))

# Newton Iterations
while(sqrt(sum(u^2)) / sqrt(sum(x^2)) > 1e-6) {
  u <- solve(DG(x, mu, Sigma, sigmaP2),
             G(x, mu, Sigma, sigmaP2))
  x <- x - u
}

#display the solution
x

# 3(c)
# need to confirm if we do indeed have minimum variance at the resultant x
# first need the upper left nxn block of DG
DG(x, mu, Sigma, sigmaP2)[1:5,1:5]

# then we find the eigenvalues
eigen(DG(x, mu, Sigma, sigmaP2)[1:5,1:5])

# now to calculate portfolio risk
t(x[1:5])%*%mu

