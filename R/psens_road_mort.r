spNR -> spRM

spRM$a_rmort <- rep(.2,17)
spRM$rmort_met <- rep(.2,17)
rbind(spRM,spRM[17,],spRM[17,],spRM[17,],spRM[17,])->spRM

scennam <- c("pr_b_low", "pr_b_hi", "fec_lo", "fec_hi", "initad_low", "initad_hi", "initsa_lo", "initsa_hi", "dphi_sa_lo", "dphi_sa_hi", "nb_dphi_lo", "nb_dphi_hi", 
             "ap_dphi_lo", "ap_dphi_hi", "lmort_hi", "lmort_lo", "armort_hi", "armort_lo", "m_rmort_hi", "m_rmort_lo","avg")

cbind(scennam, spRM[,2:11]) -> spRM
spRM$a_rmort[17] <- 0.45
spRM$a_rmort[18] <- 0.05
spRM$rmort_met[19] <- 0.45
spRM$rmort_met[20] <- 0.05

library(foreach)
library(doParallel)
library(parallel)

numCores <- detectCores()-1
cl <- makeCluster(numCores)
registerDoParallel(cl)

inputs <- 1:400
# counter2 <- 1
# for(k in 1:8){
#   for(m in 1:2){

system.time(
  for(j in 1:21){
    processInput <- function(i) {
      popsim(nyears=50, init_pop = c(spRM[j,4],spRM[j,5]), dphi_sa = spRM$dphi_sa[j], pr_b = spRM$pr_b[j], 
             nb_dphi = spRM$nb_dphi[j], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
             guard_limit = 10, a_p_dphi = spRM$a_p_dphi[j], a_rmort = spRM$a_rmort, p_days = 14, fec = spRM$fec[j], 
             lmort = (1-spRM$lmort[j]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = spRM$a_rmort[j], 
             a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = spRM$rmort_met[j], 
             ng_met = c(0,0,0)) 
    } #procIn
    #counter2 <- counter2+1
    results <- foreach(i=inputs) %dopar% {
      processInput(i)
    }
    # ~ 1 minute for 500 simulations
    assign(paste("rms.",spRM$scennam[j],sep=""),popmets(results,5))
  } 
)


rbind(rms.ap_dphi_hi,rms.ap_dphi_lo,rms.dphi_sa_hi,rms.dphi_sa_lo,rms.fec_hi,rms.fec_lo,rms.initad_hi,rms.initad_low,rms.initsa_hi,rms.initsa_lo,rms.lmort_hi,
      rms.lmort_lo,rms.nb_dphi_hi,rms.nb_dphi_lo,rms.pr_b_hi,rms.pr_b_low,rms.armort_hi,rms.armort_lo,rms.m_rmort_hi,rms.m_rmort_lo,rms.avg) -> sen_rm1

as.data.frame(sen_rm1) -> sen_rm1
names(sen_rm1) <- c("aminpop", "lambda", "vlamb", "tte", "pre")
