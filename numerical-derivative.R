# Numerical derivative function

numerical_derivative <- function(x0, f, h=1e-05){
  # x0 is the initial point
  # f is the function
  # h is "step"
  rightder <- (f(x0 + h) - f(x0)) / h
  leftder <- (f(x0) - f(x0-h)) / h
  out = NULL
  out$rightder <- rightder
  out$leftder <- leftder
  out
}