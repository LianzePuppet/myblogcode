Fibonaccidqd <- function(n)
{
  f1 <- 1
  f2 <- 1
  if(n == 1|n == 2)
  {
    return(f2)
  }
  else
  {
    for(i in 3:n)
    {
      temp1 <- f2
      f2 <- f1 + f2
      f1 <- temp1
    }
    return(f2)
  }
}
