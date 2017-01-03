library(foreach)
library(doParallel)
library(parallel)

numCores <- detectCores()-1
cl <- makeCluster(numCores)
registerDoParallel(cl)


source('./R/popsim.r')
source('./R/bprob.r')
source('./R/fecund.r')
source('./R/surv.r')
source('./R/migrate.r')
load('./data/scenarios.Rdata')

counter2 <- 1
j <- 1:400  # change to 400

system.time(
for(i in 1:3){  # loop over 3 scenarios
  # pull in road xing guard data from scenarios object
  gds_ai <- c((scenarios$persons_adultmigtation[i]*scenarios$night1_adult_in_migtation[i]),(scenarios$persons_adultmigtation[i]*scenarios$night2_adult_inout_migtation[i]),
              (scenarios$persons_adultmigtation[i]*scenarios$night3_adult_inout_migtation[i]))
  gds_ao <- c((scenarios$persons_adultmigtation[i]*scenarios$night2_adult_inout_migtation[i]),(scenarios$persons_adultmigtation[i]*scenarios$night3_adult_inout_migtation[i]),
              (scenarios$persons_adultmigtation[i]*scenarios$night4_adult_out_migtation[i]))
  gd_m <- c((scenarios$persons_juvemigration[i]*scenarios$night5_meta_out_migration[i]), (scenarios$persons_juvemigration[i]*scenarios$night6_meta_out_migration[i]), 
            (scenarios$persons_juvemigration[i]*scenarios$night7_meta_out_migration[i]))
 
  # # create new function for simulations
  paraloop <- function(j) {
    popsim(nyears=50, init_pop = c(pars$init_pop[1],pars$init_pop.1[1]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1],
           nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = gds_ai,
           guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort, p_days = 14, fec = pars$fec[1],
           lmort = (1-pars$lmort[1]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort,
           a_out_nguard = gds_ao, met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1],
           ng_met = gd_m)
  } #paraloop
  # 
  # # parallel for loop
  results <- foreach(i=inputs) %dopar% {
    paraloop(j)
  }
  # write output 
  #assign(paste("cs",i,sep=""),popmets(results,5))
}
)
