lampop <- function(vec){
  # calculate lambda from time series: divide 2nd through last element by 1st through 2nd to last element
  tmp1 <- vec[1:length(vec)-1]
  tmp2 <- vec[2:length(vec)]
  tmp2/tmp2 -> lam
  return(lam=lam, vlam=var(lam))
}
