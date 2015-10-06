# Golden-section minimization algorithm

golden_section <- function(f, a,b, tau=(3-sqrt(5))/2, iter=100, eps=1e-5){
  # Tries to find one local minimum strictly between a and b, a < b.
  # f is the function
  # TODO: raise exception if a > b (warning). Solution not affected.
  # prepare the while loop
  i <- 0
  ctd <- T
  while(ctd){
    # initialize values
    x <- a + tau * (b - a)
    y <- b - tau * (b - a)
    
    # TODO: write exceptions if these produce NaNs
    
    fa <- f(a)
    fb <- f(b)
    fx <- f(x)
    fy <- f(y)
  
    # five cases:
    if (fx < fy & fa <= fx){
      b <- x
    }
    else if (fx < fy & fa > fx){
      b <- y
    }
    else if (fx > fy & fy >= fb){
      a <- y
    }
    else if (fx > fy & fy < fb){
      a <- x
    }
    else if (fx == fy){
      a <- x
      b <- y
    }
    else print("Error in code: ifs should catch all possibilities.")
    
    i <- i + 1
    ctd <- (i < iter & abs(b - a) > eps)
  }
  
  # Print type of exit: due to 'convergence' or reached maximum iteration
  if (i < iter){
    print(paste("Exit 'with convergence' after", i, "iterations."))
    }
  else print(paste("Exit 'without convergence' after", i, "iterations."))
  # TODO: warns if either a or b is same as initial, which may indicate no minimum between a and b
  #     if so, extend range a-b, re-test. Note: optimal selection of a,b should rather be done
  #     using an external function. Final a,b are returned here for that reason.
  return(c(a,b, (a+b)/2, f((a+b)/2)))  # a, b, min, f(min)
}

if(interactive()){
  # Tested functions:
  f0 <- function(x) x^4 -3*x^3 + 2
  f1 <- function(x) 10*sin(x)+x^2 
  f2 <- function(x) x^2
  
  print("Running golden_section on some functions...")
  print(body(f0))
  print(golden_section(f0,0,6))
  print(body(f1))
  print(golden_section(f1,0,5))  # yields local minimum at 3.8, even though f1(0) = 0 is smaller (algorithm "looks" for point with slope 0)
  print(golden_section(f1,-10,5)) # finds global minimum
  print(body(f2))
  print(golden_section(f2, 3,5))
  print(golden_section(f2, -1,1))
}