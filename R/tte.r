tte <- function(popdata, qe){
  # calculate time to extinction given qe threshold
  which(popdata<=qe) -> extincts
  return(extincts[1])
  # need to fix for when population doesn't go extinct
}
