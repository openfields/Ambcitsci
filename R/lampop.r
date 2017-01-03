lampop <- function(dat){
  # calculate lambda from time series: divide 2nd through last element by 1st through 2nd to last element
  dat <- apply(dat,1,sum)
  tmp1 <- dat[1:length(dat)-1]
  tmp2 <- dat[2:length(dat)]
  tmp2/tmp1 -> lam
  return(list(lam=lam, vlam=var(lam)))
}
