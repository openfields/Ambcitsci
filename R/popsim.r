popsim <- function(nyears, init_pop, dphi_sa, pr_b, nb_dphi, a_in_night_prob, a_rmort, a_in_num_guards, guard_limit){
  # parameters for function: nyears (50?), init_pop, dphi (daily survival - possibly 3 different rates: adult pond/forest and met forest),


  # source other functions
  source("~/Documents/bignight/R/bprob.r")
  source("~/Documents/bignight/R/migrate.r")
  source("~/Documents/bignight/R/fecund.r")
  source("~/Documents/bignight/R/surv.r")

  # create object for output
  out <- matrix(NA, nrow=nyears, ncol=2)

  # simulation
  for(i in 1:nyears){
    if (i==1) {
      adults <- init_pop[1]
      subadults <- init_pop[2]
    }

    # survival and maturity of subadults (assume everyone matures after 1 year; could modify this)
    #dphi_sa <- .999
    sa_days <- 365  # need to track days with in/out migration to keep accounting straight for survival rate
    surv(subadults, dphi_sa, sa_days) -> newads

    # get number of (non)breeding individuals
    bprob(adults, pr_b) -> potbreeders
    adults - sum(potbreeders) -> nonbreeders

    # survival of nonbreeding adults
    #nb_dphi <- .9992
    nb_days <- 365
    surv(nonbreeders, nb_dphi, nb_days) -> rnbreed

    # inbound road mortality for adults
    #night_prob <- c(.8, .1, .1)
    #a_rmort <- .3
    #a_in_num_guards <- c(2,0,0)
    #guard_limit <- 20
    migrate(sum(potbreeders), a_in_night_prob, a_rmort, a_in_num_guards, guard_limit) -> breeders

    # adult survival in pond
    dphi <- .99 # DAILY survival rate
    days <- 21 # duration of breeding period
    surv(sum(breeders), dphi, days) -> radults # remaining adults

    # breeding productivity: need to think about females/female in terms of breeding females/female metamorphs
    fec <- 150
    fecund(breeders, fec) -> eggs
    lmort <- .98 # think about losing ~7200/7300 eggs produced
    #sum(eggs)*(1-lmort) -> mets #metamorphs produced
    sum(rbinom(sum(eggs),1,(1-lmort))) -> mets

    # outbound adult mortality
    np2 <- c(.6,.1,.1,.1,.1)
    rmort2 <- .3
    ng2 <- c(0,0,0,0,0)
    gl2 <- 20
    migrate(sum(radults), np2, rmort2, ng2, gl2) -> pbadults #post breeding adults

    # outbound metamorph mortality
    np3 <- np2
    rmort3 <- .3
    ng3 <- c(0,0,0,0,0)
    gl3 <- 20
    migrate(mets, np3, rmort3, ng3, gl3) -> rmets

    # adult survival during remainder of year after outbound migration
    dphi <- .99
    days <- 180
    surv(sum(pbadults), dphi, days) -> rbads # remaining adults that bred

    # metamorph survival during remainder of year after outbound migration
    dphi <- .99
    days <- 90
    surv(sum(rmets), dphi, days) -> newsas

    # end of year accounting
    adults <- sum(rbads) + sum(rnbreed) + sum(newads)
    subadults <- sum(newsas)

    #return(list(newads=newads, nonbreeders=nonbreeders, rnbreed=rnbreed, breeders=breeders, radults=radults, mets=mets))

    #     print(i)
    #     print("adults")
    #     print(adults)
    #     print("subadults")
    #     print(subadults)
    out[i,] <- c(adults, subadults)
  }
  return(out)
}
