#' breedsurv
#'
#' @description This function calculates the survival and transition probabilities of breeding adults to determine how many adults go on to breed or not breed in the following year
#' @param nonbreedsurv survival probability for adults that breed
#' @param nonbreedtran transition probability to give proportion of adults that breed and survive that go on to be nonbreeders in the next year
#' @param initpop initial number of adults at start of year
#' @author Will Fields
#' @export

nonbreedsurv <- function(initpop, nonbreedsurv, nonbreedtran){
  # inputs: initial population of breeding adults, survival probability for breeding adults, and transition probability (breed -> nonbreed)
  # processing: calculate number of breeding/nonbreeding adults that result from the initial population of breeding adults
  # outputs: list of breeding and nonbreeding adults for next year

  # survival process for initial population
  endpop <- rbinom(initpop, 1, nonbreedsurv)

  # transition rate from breeders to nonbreeders
  transpop <- rbinom(sum(endpop), 1, nonbreedtran)

  # number of nonbreeders in next time step
  nextbreed <- sum(transpop)

  # number of breeders in next time step
  nextnonbreed <- length(transpop) - sum(nextbreed)

  # return output
  out <- list(br=nextbreed, non=nextnonbreed)
  return(out)
}
