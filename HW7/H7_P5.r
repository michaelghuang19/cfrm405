# Michael Huang
# Homework 7
# Adekoya
# Problem 5

# 5(b)

f <- function(x) {
  -1*(x^5) - cos(x)
}

df <- function(x) {
  -5*(x^4) + sin(x)
}

u <- 500 
x <- 1
steps <- 0

while (abs(u)/abs(x) > 1e-4) {
  u <- f(x) / df(x)
  x <- x - u
  steps <- steps + 1
}

x
steps

# 5(c)
# One change that we could make to the Newton's method implementation
# to try to make the estimate more accurate is to decrease the tolerance,
# which we will try to implement as 1e-10 rather than 1e-4.

u_smallTol <- 500 
x_smallTol <- 1
steps_smallTol <- 0

while (abs(u_smallTol)/abs(x_smallTol) > 1e-10) {
  u_smallTol <- f(x_smallTol) / df(x_smallTol)
  x_smallTol <- x_smallTol - u_smallTol
  steps_smallTol <- steps_smallTol + 1
}

x_smallTol
steps_smallTol

# Interestingly enough, we don't find a better approximation,
# even though we increased the number of steps we took,
# we still didn't find a better approximation.

# 2(d)
# We can't apply Newton's method with x_0 = 0 since x_0 is guaranteed to be 0,
# so Newton's method wouldn't give us a valid value since we can't divide by
# x in the while check.