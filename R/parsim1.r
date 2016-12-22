# script to do population simulations in parallel

library(foreach)
library(doParallel)
library(parallel)
source('./R/popsim.r')
source('./R/surv.r')
source('./R/bprob.r')
source('./R/migrate.r')
source('./R/fecund.r')
load('./data/pars.Rdata')
numCores <- detectCores()-1
cl <- makeCluster(numCores)
registerDoParallel(cl)

inputs <- 1:500 # set number of iterations here
processInput <- function(i) {
  popsim(nyears=50, init_pop = c(pars[1,4],pars[1,5]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
         nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
         guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = 0, p_days = 14, fec = pars$fec[1], 
         lmort = (1-pars$lmort[1]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = 0, 
         a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = 0, 
         ng_met = c(0,0,0)) 
}

system.time(results <- foreach(i=inputs) %dopar% {
  processInput(i)
  }
) # ~ 1 minute for 500 simulations

