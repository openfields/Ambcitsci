# script to do sensitivity analysis: 
# need to cycle through things 
source('~/Documents/Ambcitsci/R/minpop.r')


# create output object: 12 parameters: avg pop rate at initial conditions, 80% lower, 80% higher
pars <- rbind(vrates[1,2:12],vrates[5:6,2:12])
senout <- matrix(0, nrow=12, ncol=6)

# loop through for each parameter

# adults
  i <- 1
  
      tmppop <- popsim(nyears=50, init_pop = c(pars[i,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[1,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[1,2]
          
      tmppop <- popsim(nyears=50, init_pop = c(pars[2,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[1,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[1,4]
  
      tmppop <- popsim(nyears=50, init_pop = c(pars[3,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[1,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[1,6]
  
  
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[2,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[2,2]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[2,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[2,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[2,4]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[3,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], 
                       lmort = pars$lmort[1], a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], 
                       ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[2,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[2,6]
      
      # dphi_sa
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[3,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[3,2]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[2,4]), dphi_sa = pars$dphi_sa[2], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[3,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[3,4]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[3,4]), dphi_sa = pars$dphi_sa[3], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], 
                       lmort = pars$lmort[1], a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], 
                       ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[3,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[3,6]
      
      # pr_b
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[4,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[4,2]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[2,4]), dphi_sa = pars$dphi_sa[2], pr_b = pars$pr_b[2], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[4,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[4,4]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[3,4]), dphi_sa = pars$dphi_sa[3], pr_b = pars$pr_b[3], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], 
                       lmort = pars$lmort[1], a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], 
                       ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[4,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[4,6]
      
      # nb_dphi
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[5,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[5,2]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[2,4]), dphi_sa = pars$dphi_sa[2], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[2], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[5,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[5,4]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[3,4]), dphi_sa = pars$dphi_sa[3], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[3], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], 
                       lmort = pars$lmort[1], a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], 
                       ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[5,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[5,6]
      
      # a_p_dphi
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[1,4]), dphi_sa = pars$dphi_sa[1], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[1], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[6,1]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[6,2]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[2,4]), dphi_sa = pars$dphi_sa[2], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[2], a_rmort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], lmort = pars$lmort[1], 
                       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], a_out_nguard = c(0,0,0), 
                       met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[6,3]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[6,4]
      
      tmppop <- popsim(nyears=50, init_pop = c(pars[1,4],pars[3,4]), dphi_sa = pars$dphi_sa[3], pr_b = pars$pr_b[1], 
                       nb_dphi = pars$nb_dphi[1], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = pars$a_p_dphi[3], armort = pars$a_rmort[1], p_days = 14, fec = pars$fec[1], 
                       lmort = pars$lmort[1], a_out_night_prob = c(0.333,0.333,0.334), rmort2 = pars$a_rmort[1], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = pars$rmort_met[1], 
                       ng_met = c(0,0,0))
      apply(tmppop,1,sum)->tpop
      min(tpop) -> senout[6,5]
      lampop(tmppop)->foo
      mean(foo$lam) -> senout[6,6]
      
      senout -> senout2
      tout <- matrix(0,nrow=12,ncol=3)
      sout<-function(pars, pnum, outmat, outrow){

      
      # get pnum as arg
        # replicate parameters
        rbind(pars[1,],pars[1,],pars[1,])->tpar
        tpar[2,pnum] <- pars[2,pnum]
        tpar[3,pnum] <- pars[3,pnum]
            
        for(i in 1:3){
          tmppop <- popsim(nyears=50, init_pop = c(tpar[i,4],tpar[i,5]), dphi_sa = tpar$dphi_sa[i], pr_b = tpar$pr_b[i], 
                       nb_dphi = tpar$nb_dphi[i], a_in_night_prob = c(0.333,0.333,0.334), a_in_num_guards = c(0,0,0), 
                       guard_limit = 10, a_p_dphi = tpar$a_p_dphi[i], a_rmort = tpar$a_rmort[i], p_days = 14, fec = tpar$fec[i], 
                       lmort = (1-tpar$lmort[i]), a_out_night_prob = c(0.333,0.333,0.334), rmort2 = tpar$a_rmort[i], 
                       a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), rmort_met = tpar$rmort_met[i], 
                       ng_met = c(0,0,0))
          apply(tmppop,1,sum)->tpop
          min(tpop) -> outmat[outrow,5]
          #lampop(tmppop)->foo
          #mean(foo$lam) -> outmat[outrow,6]
        }
      return(outmat)
      }
      sout(pars=pars, pnum=4, outmat=tout, outrow=1)
      
      