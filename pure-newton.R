# Pure newton minimization algorithm
# Requires input of first and second derivatives of f
# Warning: have to check that it is indeed a maximum


pure_newton <- function(f, df, ddf, init=1, max_iter=100, eps=1e-5){
  # prepare the while loop
  x <- init  # not overriding init may be useful to remember where the algorithm began
  ctd <- T
  iter <- 0
  while(ctd){
    # TODO: write exceptions handling if ddf is (close to) 0
    x <- x - df(x)/ddf(x)
    iter <- iter + 1
    ctd <- (iter < max_iter & abs(df(x))/(1+abs(ddf(x))) > eps)
  }
  
  # check if it is a maximum
  if (any(f(x+eps) < f(x), f(x-eps) < f(x))){
    warning(paste("Warning, solution", x, "may be a maximum."))
  }
  return(c(x, f(x)))
}


if(interactive()){
  # Tested functions:
  f0   <- function(x) x^4 - 3 * x^3 + 2
  df0  <- function(x) 4 * x^3 - 9*x^2
  ddf0 <- function(x) 12 * x^2 - 18*x
  f1   <- function(x) 10 * sin(x) + x^2 
  df1  <- function(x) 10 * cos(x) + 2*x
  ddf1 <- function(x) 10 * (-sin(x)) + 2
  f2   <- function(x) x^2
  df2  <- function(x) 2 * x
  ddf2 <- function(x) 2
  
  print("Running pure_newton on some functions...")
  print(body(f0))
  print(body(df0))
  print(body(ddf0))
  print(pure_newton(f0,df0, ddf0))  # finds a local minimum at 0.004... global is 2.25
  print(pure_newton(f0,df0, ddf0, init=2.5))
  print(body(f1))
  print(body(df1))
  print(body(ddf1))
  print(pure_newton(f1,df1, ddf1))  # catches the maximum at 1.3... not the minimum at -1.2
  print(pure_newton(f1,df1, ddf1, init=-2)) # catches the minimum
  print(body(f2))
  print(body(df2))
  print(body(ddf2))
  print(pure_newton(f2,df2, ddf2))
}