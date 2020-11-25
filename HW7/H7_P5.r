# Michael Huang
# Homework 7
# Adekoya
# Problem 5

# 2(b)

f <- function(x) {
  -1*(x^5) - cos(x)
}

df <- function(x) {
  -5*(x^4) + sin(x)
}

u <- 500 
x <- 1
k <- 0

while (abs(u)/abs(x) > 1e-4) {
  u <- f(x) / df(x)
  x <- x - u
  k <- k + 1
}

x
k

# 2(c)
# One change that we could make to the Newton's method implementation
# to try to make the estimate more accurate is to decrease the tolerance,
# which we will try to implement as 1e-10 rather than 1e-4.

u <- 500 
x <- 1
k <- 0

while (abs(u)/abs(x) > 1e-10) {
  u <- f(x) / df(x)
  x <- x - u
  k <- k + 1
}

x
k

# Interestingly enough, we don't find a better approximation,
# even though we increased the number of steps we took,
# we still didn't find a better approximation.

# 2(d)
# We can't apply Newton's method with x_0 = 0 since x_0 is guaranteed to be 0,
# so Newton's method wouldn't give us a valid value since we can't divide by
# x in the while check.