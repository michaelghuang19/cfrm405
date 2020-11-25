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

y <- Bisection(g, 6, 6.25)
y  # final value for c
g(y)  # final value for g(c)

# The initial interval choice was decided by seeing that the upper
# values for 6.5 and 6.25 were both greater than 0, so we must have
# to start with an upper limit that is at most these values.
# Therefore, we could have chosen 6.25 or 6.5 as our starting upper limit.
