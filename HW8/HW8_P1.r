# Michael Huang
# Homework 7
# Adekoya
# Problem 1

# 1(a)

# original optimization function f
fa <- function(x) {
  3*x[1] - 4*x[2] + x[3] - 2*x[4]
}

# Set function F
Fa <- function(x) {
  c(3 + 6*x[1]*x[6],
    -4 - 2*x[2]*x[5],
    1 + 2*x[3]*x[5] + 2*x[3]*x[6],
    -2 + 2*x[4]*x[5] + 4*x[4]*x[6],
    -x[2]^2 + x[3]^2 + x[4]^2 - 1,
    3*x[1]^2 + x[3]^2 + 2*x[4]^2 - 6)
}

# set gradient of function F i.e. DF
DFa <- function(x) {
  matrix(c(6*x[6], 0, 0, 0, 0, 6*x[1],
    0, -2*x[5], 0, 0, -2*x[2], 0,
    0, 0, 2*x[5] + 2*x[6], 0, 2*x[3], 2*x[3],
    0, 0, 0, 2*x[5] + 4*x[6], 2*x[4], 4*x[4],
    0, -2*x[2], 2*x[3], 2*x[4], 0, 0,
    6*x[1], 0, 2*x[3], 4*x[4], 0, 0),
    6, 6, byrow = TRUE)
}

# set starting points
xa <- c(1, -1, 1, -1, 1, -1)
ua <- rep(1, 6)

# perform Newton's method
while (sqrt(sum(ua^2)) / sqrt(sum(xa^2)) > 1e-6) {
  ua <- solve(DFa(xa), Fa(xa))
  xa <- xa - ua
}

# First critical point
crita_1 <- xa
crita_1

# set new starting points
xa <- c(-1, 1, -1, 1, -1, 1)
ua <- rep(1, 6)

# perform Newton's method
while (sqrt(sum(ua^2)) / sqrt(sum(xa^2)) > 1e-6) {
  ua <- solve(DFa(xa), Fa(xa))
  xa <- xa - ua
}

# Second critical point
crita_2 <- xa
crita_2

# Determine if critical points are maximum or minimum
# In HW8 PDF, we find crita_1 to be a maximum,
# and crita_2 to be a minimum.

# Evaluate maximum or minimum
# Final values of crit point 1 (max) and crit point 2 (min)
fa(crita_1)
fa(crita_2)

# 1(b)

# original optimization function f
fb <- function(x) {
  x[1] - 2*x[2] + 3*x[3] - 4*x[4]
}

# Set function F
Fb <- function(x) {
  c(1 + 2*x[1]*x[5] + 2*x[1]*x[6],
    -2 + 2*x[2]*x[5] + 4*x[2]*x[6],
    3 + 6*x[3]*x[6],
    -4 - 2*x[4]*x[5],
    x[1]^2 + x[2]^2 - x[4]^2 - 1,
    x[1]^2 + 2*x[2]^2 + 3*x[3]^2 - 6)
}

# set gradient of function F i.e. DF
DFb <- function(x) {
  matrix(c(2*x[5] + 2*x[6], 0, 0, 0, 2*x[1], 2*x[1],
    0, 2*x[5] + 4*x[6], 0, 0, 2*x[2], 4*x[2],
    0, 0, 6*x[6], 0, 0, 6*x[3],
    0, 0, 0, -2*x[5], -2*x[4], 0,
    2*x[1], 2*x[2], 0, -2*x[4], 0, 0,
    2*x[1], 4*x[2], 6*x[3], 0, 0, 0),
    6, 6, byrow = TRUE)
}

# set starting points
xb <- c(1, -1, 1, -1, 1, -1)
ub <- rep(1, 6)

# perform Newton's method
while (sqrt(sum(ub^2)) / sqrt(sum(xb^2)) > 1e-6) {
  ub <- solve(DFb(xb), Fb(xb))
  xb <- xb - ub
}

# First critical point
critb_1 <- xb
critb_1

# set new starting points
xb <- c(1, 1, 1, 1, 1, 1)
ub <- rep(1, 6)

# perform Newton's method
while (sqrt(sum(ub^2)) / sqrt(sum(xb^2)) > 1e-6) {
  ub <- solve(DFb(xb), Fb(xb))
  xb <- xb - ub
}

# Second critical point
critb_2 <- xb
critb_2

# Determine if critical points are maximum or minimum
# In HW8 PDF, we find crita_1 to be a maximum,
# and crita_2 to be a minimum.

# Evaluate maximum or minimum
# Final values of crit point 1 (max) and crit point 2 (min)
fb(critb_1)
fb(critb_2)
