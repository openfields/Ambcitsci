calclam <- function(outdata){
  # calculate lambda from series of population counts
  # outdata is matrix with total number of adults and subadults
  
  # get total pop
  tpop <- apply(outdata,1,sum)
  
  # divide current pop by previous pop to get lambda, store in vector for output
  lam <- tpop[2:dim(outdata)[1]]/tpop[1:(dim(outdata)[1]-1)]
  return(lam)
  
}
