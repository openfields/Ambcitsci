pext <- function(mat, qe){
  # inputs: matrix of adult/subad pop
  #         quasiextinction threshold
  # processing: find first time when population size meets quasi-extinction threshold
  #         
  # output: ext: 0 (no extinction) or 1 (extinction),
  #         t: time at which extinction occurs
  tot <- apply(mat, 1, sum)
  which(tot<=qe) -> ex

  if(length(ex)>0) ext <- 1 else ext <- 0
  return(list(t=ex[1], ext=ext))
  
}
