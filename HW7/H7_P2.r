# Michael Huang
# Homework 7
# Adekoya
# Problem 2

# 2(a)
bsp <- function(S, T, t, K, r, s, q) {
  # equation for d1
  d1 <- (log(S/K)+(r-q+0.5*s^2)*(T-t)) / (s*sqrt(T-t)) 
  # equation for d2
  d2 <- d1 - s*sqrt(T-t) 
  K*exp(-r*(T-t))*pnorm(-d2) - S*exp(-q*(T-t))*pnorm(-d1) 
  #put price equation

  # here pnorm is the cumulative distribution function (CDF)
  # of the standard normal distribution Phi(x)
}

# 2(b)
Bisection <- function(f, left, right, tol=`^`(10, -4)) {
  # check if we are within our tolerance as our while loop condition
  while(right - left > tol){
    # take the mid-point of two limits, and see which
    # way to shift our limits (i.e. which side to bisect on)
    mid <- left + ((right - left) / 2)
    if (sign(f(left)) == sign(f(mid))) {
      left <- mid
    } else {
      right <- mid
    }
  }
  (left + right) / 2
}

# 2(c)
#defining helper for partial derivative of f with respect to sigma
dbsp <- function(S, T, t, K, r, s, q) { 
  
  d1 <- (log(S/K)+(r-q+0.5*s^2)*(T-t)) / (s*sqrt(T-t))
  
  #defining f'()
  # S*sqrt(T-t)*exp(-q*(T-t)) * exp(-(d1^2)/2)*1/sqrt(2*pi)
  S*sqrt(T - t)*exp(-q*(T - t))*1/sqrt(2*pi)*exp(-d1^2/2)
  # We use Phi(x) as defined in the original Black-Scholes slide
}

# define helper f(sigma)= bsp(sigma) - 8
fsig <- function(sigma) {
  bsp(50, 0.5, 0.0, 45, 0.06, sigma, 0.02) - 8
}

#define helper f'(sigma)
dfsig <- function(sigma) {
  dbsp(50, 0.5, 0.0, 45, 0.06, sigma, 0.02)
}

# Implement Newton's method
Newtons <- function() {
  u <- 500
  x <- 1
  k <- 0
  tol <- `^`(10, -5)
  
  while ((abs(u)/abs(x)) > tol) {
    u <- fsig(x)/dfsig(x)
    x <- x-u
    k <- k+1 #monitors number of steps
  }

  x
}

# 2(d)
# Calculate Black-Scholes price using volatility from Bisection method

# create our lower and upper limits for Bisection method
left <- 0.05
right <- 0.5

# Do Bisection method to find volatility
BiVolatility <- Bisection(fsig, left, right);

# Calculate final Black-Scholes Price
BiBSP <- bsp(50, 0.5, 0.0, 45, 0.06, BiVolatility, 0.02) - 8
BiBSP

# 2(e)
# Calculate Black-Scholes price using volatility from Newton's method

# Do Newton's method to find volatility
NewVolatility <- Newtons()

# Calculate final Black-Scholes Price
NewBSP <- bsp(50, 0.5, 0.0, 45, 0.06, NewVolatility, 0.02) - 8
NewBSP
