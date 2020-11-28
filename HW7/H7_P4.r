# Michael Huang
# Homework 7
# Adekoya
# Problem 4

Bisection <- function(f, left, right, tol=`^`(10, -5)) {
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

# input function for g(x)
g <- function(x) {
  (x^3) - (5*(x^2)) - (7*x)
}

# Set bound values and check proper bounds for Bisection method
left <- 6
right <- 6.25
g(left)  # This should be negative
g(right)  # This should be positive

y <- Bisection(g, left, right)
y  # final value for c
g(y)  # final value for g(c)

# The initial interval choice was decided by looking at a graph
# of the function and picking two bound values where
# one value was negative, and the other was positive. 
# Since g(6) is negative and g(6.25) is positive, I decided
# to use those values as my two bounds.
