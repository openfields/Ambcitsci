# scenario simulations
system.time(for(i in 1:100){
out[,,i] <- popsim(nyears=50, init_pop=c(100,100), dphi_sa = .9986, pr_b= .4230, nb_dphi = .9992, a_in_night_prob = c(0.333,0.333,0.334), 
       a_rmort = 0.0407, a_in_num_guards = c(0,0,0), guard_limit = 10, a_p_dphi = 0.9994, p_days = 14, fec = 165, lmort = 0.0523, 
       a_out_night_prob = c(0.333,0.333,0.334), rmort2 = 0.0407, a_out_nguard = c(0,0,0), met_night_prob = c(0.333,0.333,0.334), 
       rmort_met = 0.0407, ng_met = c(0,0,0))
})


# set parameters:
# 
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

# parameters varying by strategy:
# number of people in adult in migration
# number of people in adult out migration
# number of people in metamorph migration