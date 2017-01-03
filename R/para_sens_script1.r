# script to do sensitivity analyses with no road mortality

# create matrix with parameter values for no road mortality
source('./R/popsim.r')
source('./R/surv.r')
source('./R/bprob.r')
source('./R/migrate.r')
source('./R/fecund.r')
source('./R/popmets.r')
source('./R/minpop.r')
source('./R/lampop.r')
source('./R/pext.r')

load('./data/pars.Rdata')
spars.NRM <- pars
spars.NRM$a_rmort <- c(0,0,0)
spars.NRM$rmort_met <- c(0,0,0)
spars.NRM[,2:11] -> spars.NRM # dropped breeding to nonbreeding transition prob

rbind(spars.NRM[1,], spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],
      spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],
      spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],spars.NRM[1,],
      spars.NRM[1,],spars.NRM[1,],spars.NRM[1,]) -> spNR

counter <-1
for(i in 1:6){
  for(j in 2:3){
    spNR[counter, i] <- spars.NRM[j,i]
    counter <- counter + 1  
  }
}
for(i in 9:10){
  for(j in 2:3){
    spNR[counter, i] <- spars.NRM[j,i]
    counter <- counter + 1
  }
}
spNR <- spNR[1:17,] # drop extra rows

scennam <- c("pr_b_low", "pr_b_hi", "fec_lo", "fec_hi", "initad_low", "initad_hi", "initsa_lo", "initsa_hi", "dphi_sa_lo", "dphi_sa_hi", "nb_dphi_lo", "nb_dphi_hi", 
             "ap_dphi_lo", "ap_dphi_hi", "lmort_hi", "lmort_lo", "avg")

cbind(scennam, spNR) -> spNR

# start parallel loops
library(foreach)
library(doParallel)
library(parallel)

numCores <- detectCores()-1
cl <- makeCluster(numCores)
registerDoParallel(cl)

inputs <- 1:500
# counter2 <- 1
# for(k in 1:8){
#   for(m in 1:2){
     
system.time(
  for(j in 1:17){
    processInput <- function(i) {
    popsim(nyears=50, init_pop = c(spNR[j,4],spNR[j,5]), dphi_sa = spNR$dphi_sa[j], pr_b = spNR$pr_b[j], 
         nb_dphi = spNR$nb_dphi[j], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
         guard_limit = 10, a_p_dphi = spNR$a_p_dphi[j], a_rmort = 0, p_days = 14, fec = spNR$fec[j], 
         lmort = (1-spNR$lmort[j]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = 0, 
         a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = 0, 
         ng_met = c(0,0,0)) 
  } #procIn
  #counter2 <- counter2+1
  results <- foreach(i=inputs) %dopar% {
    processInput(i)
  }
  # ~ 1 minute for 500 simulations
  assign(paste("s.",spNR$scennam[j],sep=""),popmets(results[[1:499]],5))
} 
)
#} # k


# do for last value: high fecundity
# 
#   processInput <- function(i) {
#   popsim(nyears=50, init_pop = c(spNR[1,4],spNR[1,5]), dphi_sa = spNR$dphi_sa[1], pr_b = spNR$pr_b[1], 
#          nb_dphi = spNR$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
#          guard_limit = 10, a_p_dphi = spNR$a_p_dphi[1], a_rmort = 0, p_days = 14, fec = spNR$fec[1], 
#          lmort = (1-spNR$lmort[1]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = 0, 
#          a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = 0, 
#          ng_met = c(0,0,0)) 
# } #procIn
  
  
results2 <- foreach(i=inputs) %dopar% {
  processInput(i)
}

assign(paste("sim8.2"),popmets(results2,5))

rbind(sim1.1,sim1.2,sim2.1,sim2.2,sim3.1,sim3.2,sim4.1,sim4.2,sim5.1,sim5.2,sim6.1,sim6.2,sim7.1,sim7.2,sim8.1,sim8.2) -> psens

psens <- as.data.frame(psens)
names(psens) <- c("minpop", "lam", "vlam", "tte", "prext")

# calculate for avg values with no road mortality
inputs <- 1:500
counter2 <- 1
#for(k in 1:8){
 # for(m in 1:2){
    processInput <- function(i) {
      popsim(nyears=50, init_pop = c(spNR[1,4],spNR[1,5]), dphi_sa = spNR$dphi_sa[1], pr_b = spNR$pr_b[1], 
             nb_dphi = spNR$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
             guard_limit = 10, a_p_dphi = spNR$a_p_dphi[1], a_rmort = 0, p_days = 14, fec = spNR$fec[1], 
             lmort = (1-spNR$lmort[1]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = 0, 
             a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = 0, 
             ng_met = c(0,0,0)) 
    } #procIn
    #counter2 <- counter2+1
    results <- foreach(i=inputs) %dopar% {
      processInput(i)
    }
    # ~ 1 minute for 500 simulations
    assign(paste("sim0.0"),popmets(results,5))
#  } # m
#} # k

    rbind(psens, sim0.0)
