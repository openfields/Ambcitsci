popsim <- function(nyears, init_pop, dphi_sa, pr_b, nb_dphi, a_in_night_prob, a_rmort, a_in_num_guards, guard_limit, a_p_dphi, p_days, fec, lmort, a_out_night_prob, rmort2, 
                   a_out_nguard, met_night_prob, rmort_met, ng_met){
  
  # parameters for function: 
  # nyears: number of years to simulate population
  # init_pop: initial population vector (adults, subadults)
  # dphi_sa: daily survival for subadults
  # pr_b: probability of adults choosing to breed
  # nb_dphi: daily survival for nonbreeding adults
  # a_in_night_prob: probability of adult moving into pond to breed on different nights [should sum to 1]
  # a_rmort: road mortality for inbound adults
  # a_in_num_guards: number of xing guards for inbound migration
  # guard_limit: max number of individuals a guard can save on a night
  # a_p_dphi: daily survival for adults in pond
  # p_days: time (days) adults spend in pond
  # fec: female eggs/female
  # lmort: mortality from eggs to metamorphs
  # a_out_night_prob: probability of adult moving out of pond on different nights
  # rmort2: road mortality for outbound adults
  # a_out_nguard: number of guards for outbound adults on different nights
  # met_night_prob: probability of metamorphs moving out on different nights
  # rmort_met: road mortality for metamorphs on different nights
  # ng_met: number of xing guards for metamorphs on different nights


  # source other functions
  # source("~/Documents/ambcitsci/R/bprob.r")
  # source("~/Documents/ambcitsci/R/migrate.r")
  # source("~/Documents/ambcitsci/R/fecund.r")
  # source("~/Documents/ambcitsci/R/surv.r")

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
    # a_p_dphi <- .99 # DAILY survival rate
    # p_days <- 21 # duration of breeding period
    surv(sum(breeders), a_p_dphi, p_days) -> radults # remaining adults

    # breeding productivity: need to think about females/female in terms of breeding females/female metamorphs
    # fec <- 150
    fecund(breeders, fec) -> eggs
    # lmort <- .98 # think about losing ~7200/7300 eggs produced
    #sum(eggs)*(1-lmort) -> mets #metamorphs produced
    sum(rbinom(sum(eggs),1,(1-lmort))) -> mets

    # outbound adult mortality
    # np2 <- c(.6,.1,.1,.1,.1)
    # rmort2 <- .3
    # ng2 <- c(0,0,0,0,0)
    # gl2 <- 20
    migrate(sum(radults), a_out_night_prob, rmort2, a_out_nguard, guard_limit) -> pbadults #post breeding adults

    # outbound metamorph mortality
    #np3 <- np2
    #rmort3 <- .3
    #ng3 <- c(0,0,0,0,0)
    #gl3 <- 20
    migrate(mets, met_night_prob, rmort_met, ng_met, guard_limit) -> rmets

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
