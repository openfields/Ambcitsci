popmets <- function(simlist, qe){
# function to calculate population attributes from list of simulation output
# 1. calculate avg min pop size
# 2. calculate lambda
# 3. calculate pr(ext) and time to ext
  
  # preliminary output: columns for summary data from each simulation
  outs <- matrix(0, nrow=length(simlist), ncol=5)
  
  for(i in 1:length(simlist)){
    minpop(simlist[[i]]) -> outs[i,1] # minimum population size from simulations
    mean(lampop(simlist[[i]])$lam) -> outs[i,2] # avg lambda for simulation
    lampop(simlist[[i]])$vlam -> outs[i,3] # variance of lambda
    pext(simlist[[i]],qe)$t -> outs[i,4] #time to extinction: NA means persistence, t means year of extinction 
    pext(simlist[[i]],qe)$ext -> outs[i,5] # extinction: 0 means persistence, 1 means extinction occurred
    } #i
  
  mean(outs[,1]) -> amp
  mean(outs[,2]) -> alam
  var(outs[,3]) -> vlam
  t.ext <- outs[,4]
  t.ext[is.na(t.ext)] <- 50
  mean(t.ext) -> tte
  sum(outs[,5])/length(outs[,5]) -> pe
  return(c(amp,alam,vlam,tte,pe))
}
