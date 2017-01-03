lampop <- function(dat,qe){
  # calculate lambda from time series: divide 2nd through last element by 1st through 2nd to last element
  # sum across time steps to get total population
  dat <- apply(dat,1,sum)
  # set quasi-ext threshold
  dat[which(dat<qe)] <- 0
  
  tmp1 <- dat[1:length(dat)-1]
  tmp2 <- dat[2:length(dat)]
  tmp2/tmp1 -> lam
  return(list(lam=lam, vlam=var(lam)))
}
