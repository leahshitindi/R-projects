
############ QUESTION NINE############################################|
# Creating the GKTau function

GKTau <- function(x, rowResponse = TRUE) {
  # function checks!
  if(any(x<0) | !is.matrix(x))stop('x must be a matrix of positive values',call. = FALSE)
  

  
  # 1: grand total
  N <- sum(x)
  
  # 2: relative frequency
  p <- x/N
  
  # 3: check whether the rows are the response or the columns
  rho <- ifelse(rowResponse==TRUE,1,0)
  
  # 4 Begin the function here with the given objects
  
  #  inner sum of the numerator
  inner_sum<- function(i){
    (p[i,1:ncol(p)]-rowSums(p)[i]*
       colSums(p)[1:ncol(p)])^2/
      (((rowSums(p)[i])^(1-rho))*((colSums(p)[1:ncol(p)])^rho))
  }
  # numerator part of the function
  numerat<-sum(sapply(1:nrow(p),
                      function(i)inner_sum(i)))
  
  # denominator part of the function
  
  ssq_row <- sum(sapply(1:nrow(p),function(i)(rowSums(p)[i])^2))
  ssq_col <- sum(sapply(1:ncol(p),function(j)(colSums(p)[j])^2))
  denom <- 1-((ssq_row)^rho)*((ssq_col)^(1-rho))
  
  # the whole function 
  tau <- numerat/denom
  tau
  
}


