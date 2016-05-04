# salamander big night migration simulation
# date created 10/19/2015, Rachel Katz

#*********************************************
# Comments added by Will Fields, 05 Nov 2015
#*********************************************

library(popbio)

##### potential frequency distributions for migration events  ######
# salamanders migration into and out of pond at different days of the year and at different frequencies
# frequency distribution of adult in-migration (most cross on day 2, with some on day 1 and day 3 (sd=0.5))
# frequency distribution of adult out-migration (most cross on day 15, with greater variance (sd = 3))
# frequency distribution of juvenile out-migration (most cross on day 90, with greater variance (sd = 10))
par(mfrow=c(1,3))
plot(density(rnorm(100000,2,.5)),xlim=c(0,150),xlab="no. days",ylab="frequency",main="adult in-migration")
plot(density(rnorm(100000,15,3)),xlim=c(0,150),xlab="no. days",ylab="frequency",main="adult out-migration")
plot(density(rnorm(100000,90,10)),xlim=c(0,150),xlab="no. days",ylab="frequency",main="juvenile out-migration")

##### lefkovitch matrix from excel file 09/29/2015 #####

# create vector with data
L<-c(0.0,0.0,0.0,0.0,0.0,150,
     0.1,0.0,0.0,0.0,0.0,0.0,
     0.0,0.2,0.0,0.0,0.0,0.0,
     0.0,0.0,0.3,0.4,0.0,0.0,
     0.0,0.0,0.0,0.4,0.5,0.0,
     0.0,0.0,0.0,0.0,0.4,0.5)

# this generates errors - parameters are undefined
# L.fun1<-c(0 ,0 ,0 ,0 ,0 ,0 ,F7,
#           G1,P2,0 ,0 ,0 ,0 ,0 ,
#           0 ,G2,P3,0 ,0 ,0 ,0 ,
#           0 ,0 ,G3,P4,0 ,0 ,0 ,
#           0 ,0 ,0 ,G4,P5,0 ,0 ,
#           0 ,0 ,0 ,0 ,G5,P6,G7,
#           0 ,0 ,0 ,0 ,G8,G6,P7)


# initial set of parameters
fec<-150
phi_egg_lar<-0.10
phi_lar_met<-0.20
phi_met_juv<-0.30
phij<-0.50
phinb<-0.85
phib<-0.851                # annual survival of breeding adults
mort1<-0; road1<-1.0-mort1 # prob of surviving big night road crossing (immigratation into ponds) (mort = morality rate due to cars)
mort2<-0; road2<-1.0-mort2 # prob of surviving road crossing during outmigration (emigratation from ponds)
mort3<-0; road3<-1.0-mort3 # prob of surviving last road crossing (emigratation of juveniles into ponds)
trans1<-0.65               # transition probability from non-breeding to breeding
mig1<-0.83                 # movement probability of breeding adults

# beta shape parameters
a=0.1
b=40
no.sims<-10
mort1<-rbeta(no.sims,shape1=a,shape2=b)
mort1
hist(mort1)
road1<-1.0-mort1

L.fun2<-NULL
for(i in 1:no.sims){
L.fun<-c(0 ,          0 ,          0 ,          0 ,         0 ,                             fec,
          phi_egg_lar, 0 ,          0 ,          0 ,         0 ,                             0 ,
          0 ,          phi_lar_met, 0 ,          0 ,         0 ,                             0 ,
          0 ,          0 ,          phi_met_juv, 0 ,         0 ,                             0 ,
          0 ,          0 ,          0 ,          phij*road3, phinb*(1-trans1),               0 ,
          0 ,          0 ,          0 ,          0 ,         phinb*trans1*mig1*road1[i]*road2,  phib*mig1*road1[i]*road2)
L.fun2<-cbind(L.fun2,L.fun)
}
L.fun2

stages<-c("egg","lar","met","juv","non_breeding","breeding")


# turn vector into matrix
L<-L.fun2[,1]
#*****************************************************************************************************************************
#**********************************************L.fun2[1] will return one value - added comma to change to return first column
A<-matrix(L,ncol=6,nrow=6,byrow=TRUE, dimnames = list(stages, stages))
A

# initial abundances in each stage as a vector
n_0<-c(8000,800,165,50,45,55)

# eigen analysis
eigen(A)

# matrix population model (A x n) for each time-step
n_1 <- A %*% n_0
#*****************************************************************************************************************************
#***************************************This will generate fractions of individuals; there are ways to get around this with functions, here you can just use round()
n_1 <- round(n_1)
n_1
n_2 <- A %*% n_1
n_2 <- round(n_2)
n_2

# create a loop to do multiple time-steps at once
# create pops to store data
no.years<-100
no.stages<-6
pops <- matrix(rep(NA,no.years*no.stages),ncol=no.stages,nrow=no.years)
pops[1,] <- n_0
for(i in 2: no.years){ # gives 20 years as annual time steps
  pops[i,] <- A %*% pops[i-1,] # assign your matrix mult to a list slot
#****************************************************************************************************************************
#*********Added round() to get whole numbers
  pops[i,] <- round(pops[i,])
}
pops[no.years,] # the last generation (because the first slot is the 0 generation)
sum_pops<-rowSums(pops)

par(mfrow=c(3,3))
plot(c(1:no.years),pops[1:no.years,1],type="l",col=1, ylim=c(min(pops),max(pops)))
plot(c(1:no.years),pops[1:no.years,2],type="l",col=2)
plot(c(1:no.years),pops[1:no.years,3],type="l",col=3)
plot(c(1:no.years),pops[1:no.years,4],type="l",col=4)
plot(c(1:no.years),pops[1:no.years,5],type="l",col=5)
plot(c(1:no.years),pops[1:no.years,6],type="l",col=6)
plot(sum_pops)

# calculate population growth rate by projection (install popbio library)
p<-pop.projection(A, n_0, no.years)
p

par(mfrow=c(2,1))
plot(c(1:no.years),p$stage.vectors["egg",]/p$pop.sizes[],col=1,type="l",ylim=c(0,1))
lines(c(1:no.years),p$stage.vectors["lar",]/p$pop.sizes[],col=2,type="l",ylim=c(0,1))
lines(c(1:no.years),p$stage.vectors["met",]/p$pop.sizes[],col=3,type="l",ylim=c(0,1))
legend("topright",legend=c("egg","lar","met"),text.col=c(1:3),cex=0.5)
plot(c(1:no.years),p$stage.vectors["juv",]/p$pop.sizes[],col=4,type="l",ylim=c(0,0.01))
lines(c(1:no.years),p$stage.vectors["non_breeding",]/p$pop.sizes[],col=5,type="l",ylim=c(0,1))
lines(c(1:no.years),p$stage.vectors["breeding",]/p$pop.sizes[],col=6,type="l",ylim=c(0,1))
legend("topright",legend=c("juv","non-breeding","breeding"),text.col=c(4:6),cex=0.5)

plot(p$pop.changes);abline(1,0)

# Calculating growth rates using eigenvalues
# The population growth rate is the dominant eigenvalue and the stable stage distribution and reproductive values are the corresponding right and left eigenvectors, respectively.
# The sensitivity and elasticity matrices identify matrix elements with the greatest absolute or proportional effects on population growth rate (de Kroon et al. 2000).
eigA <- eigen.analysis(A)
eigA

##### lefkovitch matrix 10/16/2015 - from lab meeting discussion (oct 2015) #####
#
# # initial set of parameters
# fec.mean<-125
# fec.sd  <-0
# no.clutches.mean<-10                # fecundity (average # of eggs per clutch * no.clutches per year)
# no.clutches.sd  <-00                # fecundity (average # of eggs per clutch * no.clutches per year)
#
# phi_egg.mean<-0.10          # survival from egg to larval stage
# phi_egg.sd  <-0.00          # survival from egg to larval stage
# phi_lar.mean<-0.20          # survival from larval to metamorph stage
# phi_lar.sd  <-0.00          # survival from larval to metamorph stage
# phi_met.mean<-0.30         # survival from metamorph to juvenile1 stage (non-breeding)
# phi_met.sd  <-0.00         # survival from metamorph to juvenile1 stage (non-breeding)
#
# # juvenile stages for northern populations, assume reproductive at at year 7? - ask
# # do juveniles have age/stage-based survival rates, else juvenile stages are irrelevant
# phij1.mean<-0.50                # survival (and stay) juvenile 1 stage (juvenile year 1)
# phij1.sd  <-0.00                # survival (and stay) juvenile 1 stage (juvenile year 1)
# phij2.mean<-0.50                # survival (and stay) juvenile 2 stage (juvenile year 2-3)
# phij2.sd  <-0.00                # survival (and stay) juvenile 2 stage (juvenile year 2-3)
# phij3.mean<-0.50                # survival (and stay) juvenile 3 stage (juvenile year 4-7)
# phij3.sd  <-0.00                # survival (and stay) juvenile 3 stage (juvenile year 4-7)
# phinma.mean<-0.70                # survival (and stay) non-migratory,breeding adult stage
# phinma.sd  <-0.00                # survival (and stay) non-migratory,breeding adult stage
# phima.mean <-0.65                 # survival (and stay) migratory, breeding adult stage
# phima.sd   <-0.00                 # survival (and stay) migratory, breeding adult stage
#
# psi1.mean<-1.00                 # transition probability from juv1 to juv2 (all transition after year 1?)
# psi1.sd  <-0.00                 # transition probability from juv1 to juv2 (all transition after year 1?)
# psi2.mean<-0.50                 # transition probability from juv2 to juv3  (3 years to transition)
# psi2.sd  <-0.00                 # transition probability from juv2 to juv3  (3 years to transition)
# psi3.mean<-0.50                 # transition probability from juv3 to non-breeding adult  (2 years to transition)
# psi3.sd  <-0.00                 # transition probability from juv3 to non-breeding adult  (2 years to transition)
# psi4.mean<-0.50                 # transition probability from juv3 to breeding adult  (2 years to transition)
# psi4.sd  <-0.00                 # transition probability from juv3 to breeding adult  (2 years to transition)
# psi5.mean<-0.50                 # transition probability from migrating to non-migrating breeding adult
# psi5.sd  <-0.00                 # transition probability from migrating to non-migrating breeding adult
# psi6.mean<-0.50                 # transition probability from non-migrating to migrating breeding adult
# psi6.sd  <-0.00                 # transition probability from non-migrating to migrating breeding adult
#
# road1.mean<-0.00                   # probabilty of migrating breeding adult mortality during in-migration
# road1.sd  <-0.00                   # probabilty of migrating breeding adult mortality during in-migration
# road2.mean<-0.00                   # probabilty of small juvenile mortality during out-migration
# road2.sd  <-0.00                   # probabilty of small juvenile mortality during out-migration
# road3.mean<-0.00                   # probabilty of migrating breeding adult mortality during out-migration
# road3.sd  <-0.00                   # probabilty of migrating breeding adult mortality during out-migration
#
# #L.matrix.m<-NULL
# #parms.m<-NULL
#
# parms.v<-c( fec.mean,no.clutches.mean,phi_egg.mean,phi_lar.mean,phi_met.mean,phij1.mean,phij2.mean,phij3.mean,phinma.mean,phima.mean,psi1.mean,psi2.mean,psi3.mean,psi4.mean,psi5.mean,psi6.mean,road1.mean,road2.mean,road3.mean,
#             fec.sd,no.clutches.sd,phi_egg.sd,phi_lar.sd,phi_met.sd,phij1.sd,phij2.sd,phij3.sd,phinma.sd,phima.sd,psi1.sd,psi2.sd,psi3.sd,psi4.sd,psi5.sd,psi6.sd,road1.sd,road2.sd,road3.sd)
#
#
# #***********************************************************************************
# #********I'm not sure what you're doing here exactly, but parms.m isn't defined, so this returns an error
# parms.m<-cbind(parms.m,parms.v)
# rownames(parms.m)<-c("fec.mean","no.clutches.mean","phi_egg.mean","phi_lar.mean","phi_met.mean","phij1.mean","phij2.mean","phij3.mean","phinma.mean","phima.mean","psi1.mean","psi2.mean","psi3.mean","psi4.mean","psi5.mean","psi6.mean","road1.mean","road2.mean","road3.mean",
#                      "fec.sd","no.clutches.sd","phi_egg.sd","phi_lar.sd","phi_met.sd","phij1.sd","phij2.sd","phij3.sd","phinma.sd","phima.sd","psi1.sd","psi2.sd","psi3.sd","psi4.sd","psi5.sd","psi6.sd","road1.sd","road2.sd","road3.sd")
# parms.m
#
# # create a lefkovich matrix
# stages<-c("juv1","juv2","juv3","migrating","nonmigrating")
# L.matrix.v<-c(rnorm(1,phij1.mean,phij1.sd)*(1-rnorm(1,psi1.mean,psi1.sd)), 0,                                                           0,                                                                                      rnorm(1,fec.mean,fec.sd)*rnorm(1,no.clutches.mean,no.clutches.sd)*(rnorm(1,phi_egg.mean,phi_egg.sd)*rnorm(1,phi_lar.mean,phi_lar.sd)*rnorm(1,phi_met.mean,phi_met.sd))*(1-rnorm(1,road1.mean,road1.sd))*(1-rnorm(1,road2.mean,road2.sd)),  0,
#               rnorm(1,phij1.mean,phij1.sd)*rnorm(1,psi1.mean,psi1.sd),     rnorm(1,phij2.mean,phij2.sd)*(1-rnorm(1,psi2.mean,psi2.sd)), 0,                                                                                      0,                                                                                                                             0,
#               0,                                                           rnorm(1,phij2.mean,phij2.sd)*rnorm(1,psi2.mean,psi2.sd),     rnorm(1,phij3.mean,phij3.sd)*(1-rnorm(1,psi3.mean,psi3.sd)-rnorm(1,psi4.mean,psi4.sd)), 0,                                                                                                                             0,
#               0,                                                           0,                                                           rnorm(1,phij3.mean,phij3.sd)*rnorm(1,psi3.mean,psi3.sd),                                rnorm(1,phima.mean,phima.sd)*(1-rnorm(1,psi5.mean,psi5.sd))*(1-rnorm(1,road1.mean,road1.sd))*(1-rnorm(1,road3.mean,road3.sd)), rnorm(1,phinma.mean,phinma.sd)*rnorm(1,psi6.mean,psi6.sd),
#               0,                                                           0,                                                           rnorm(1,phij3.mean,phij3.sd)*rnorm(1,psi4.mean,psi4.sd),                                rnorm(1,phima.mean,phima.sd)*rnorm(1,psi5.mean,psi5.sd)*(1-rnorm(1,road1.mean,road1.sd))*(1-rnorm(1,road3.mean,road3.sd)),     rnorm(1,phinma.mean,phinma.sd)*(1-rnorm(1,psi6.mean,psi6.sd)))
#
# #***********************************************************************************************************
# #********This also makes an error for me, there are ways to just paste the vector into a matrix, that's probably what you're trying to do?
# L.matrix.m<-cbind(L.matrix.m,L.matrix.v)
# row.names(L.matrix.m)<-c("P1","F2","F3","F4","F5",
#                          "G1","P2","X1","X2","X3",
#                          "X4","G2","P3","X5","X6",
#                          "X4","X5","G3","P4","G5",
#                          "X6","X7","G6","G4","P5")
# L.matrix.m
#
# # initial abundances in each stage as a vector (juv1, juv2, juv3, non-breeders, breeders)
# n_0<-c(100,90,80,50,40)
# initial_N<-c(100,90,80,50,40)
#
# func<-function(L.matrix,n_0,filename) {
# # convert values to L.matrix (A)
# A<-matrix(L.matrix,nrow=5,ncol=5,byrow=TRUE,dimnames = list(stages, stages))
# print(A)
#
# # eigen analysis
# eigen(A)
#
# # matrix population model (A x n) for each time-step
# #*******************************************************************************
# #*********Can do the round trick again to enforce counts of whole salamanders
# n_1 <- A %*% n_0
# n_1
# n_2 <- A %*% n_1
# n_2
#
# # create a loop to do multiple time-steps at once
# # create pops to store data
# no.years<-50
# no.stages<-5
# pops <- matrix(rep(NA,no.years*no.stages),ncol=no.stages,nrow=no.years)
# pops[1,] <- n_0
# for(i in 2: no.years){ # gives 20 years as annual time steps
#   pops[i,] <- A %*% pops[i-1,] # assign your matrix mult to a list slot
# }
# pops[no.years,] # the last generation (because the first slot is the 0 generation)
# sum_pops<-rowSums(pops)
#
# tiff(file=filename,width=2000,height=1000,res=200)
# par(mfrow=c(3,3))
# plot(c(1:no.years),pops[1:no.years,1],type="l",main="juv1",col=1, ylim=c(min(pops),max(pops)))
# plot(c(1:no.years),pops[1:no.years,2],type="l",main="juv2",col=2)
# plot(c(1:no.years),pops[1:no.years,3],type="l",main="juv3",col=3)
# plot(c(1:no.years),pops[1:no.years,4],type="l",main="migr",col=4)
# plot(c(1:no.years),pops[1:no.years,5],type="l",main="nonmigr",col=5)
# plot(sum_pops)
#
# # calculate population growth rate by projection (install popbio library)
# p<-pop.projection(A, n_0, no.years)
# print(p)
#
# plot(c(1:no.years),p$stage.vectors["juv1",]/p$pop.sizes[],col=1,type="l",ylim=c(0,1))
# lines(c(1:no.years),p$stage.vectors["juv2",]/p$pop.sizes[],col=2,type="l",ylim=c(0,1))
# lines(c(1:no.years),p$stage.vectors["juv3",]/p$pop.sizes[],col=3,type="l",ylim=c(0,1))
# lines(c(1:no.years),p$stage.vectors["migrating",]/p$pop.sizes[],col=4,type="l",ylim=c(0,1))
# lines(c(1:no.years),p$stage.vectors["nonmigrating",]/p$pop.sizes[],col=5,type="l",ylim=c(0,1))
# #legend("topright",legend=c("juv1","juv2","juv3","migrating","nonmigrating"),text.col=c(1:5),cex=0.5)
#
# plot(p$pop.changes);abline(1,0)
# dev.off()
#
# # Calculating growth rates using eigenvalues
# # The population growth rate is the dominant eigenvalue and the stable stage distribution and reproductive values are the corresponding right and left eigenvectors, respectively.
# # The sensitivity and elasticity matrices identify matrix elements with the greatest absolute or proportional effects on population growth rate (de Kroon et al. 2000).
# eigA <- eigen.analysis(A)
# eigA
# } # function to run pop matrix analysis for 50 years
#
# func(L.matrix.m[,1],initial_N,filename="noroad.tiff")
# func(L.matrix.m[,2],initial_N,filename="road1=0.01.tiff")
# func(L.matrix.m[,3],initial_N,filename="road2=0.01.tiff")
# func(L.matrix.m[,4],initial_N,filename="road3=0.01.tiff")
#
# ##### road mortality rates = person nights = effort and effect #####
# no.sims<-1000
# no.scenarios<-5
# road.mort<-new.road.mort<-road.surv<-person.effect<-persons<-matrix(NA,ncol=no.scenarios,nrow=no.sims)
#
# road.mort[,1]    <- rnorm(no.sims,0.20,0.20*0.10) # road mortality = 20% with 10% variation in mean
# road.mort[,2]    <- rnorm(no.sims,0.10,0.10*0.10) # road mortality = 20% with 10% variation in mean
# road.mort[,3]    <- rnorm(no.sims,0.10,0.05*0.10) # road mortality = 20% with 10% variation in mean
# road.mort[,4]    <- rnorm(no.sims,0.10,0.01*0.10) # road mortality = 20% with 10% variation in mean
# road.mort[,5]    <- rnorm(no.sims,0.10,0.001*0.10) # road mortality = 20% with 10% variation in mean
# person.effect[,1:no.scenarios] <- rnorm(no.sims,0.01,0.01*0.10) # one person night reduces mortality by 1% with 10% variation in mean
# persons[,1:no.scenarios]      <-runif(no.sims,0,100) # between 0 and 100 person nights available (uniform)
#
# for(i in 1:no.sims){for(j in 1:no.scenarios){
# new.road.mort[i,j]<-road.mort[i,j]-road.mort[i,j]*person.effect[i,j]*persons[i,j] # new mortality given person nights
# road.surv[i,j]    <-(1-road.mort[i,j]) # road survival = 1-mortality
# }}
# road.mort[road.mort>1]<-1 # trick to keep survival between 0 and 1 (or draw from beta)
# road.mort[road.mort<0]<-0 # trick to keep survival between 0 and 1 (or draw from beta)
# new.road.mort[new.road.mort<0]<-0 # trick
# new.road.mort[new.road.mort>1]<-1 # trick
# new.road.surv<-1-new.road.mort
#
# par(mfrow=c(2,2));
# hist(road.surv[,1]);abline(v=mean(road.surv[,1]),col=1);mean(road.surv[,1])
# hist(new.road.surv[,1]);abline(v=mean(new.road.surv[,1]));mean(new.road.surv[,1])
# plot(new.road.surv[,1]~road.surv[,1],col=1, ylim=c(.5,1),xlim=c(.5,1))
# points(new.road.surv[,2]~road.surv[,2],col=2)
# points(new.road.surv[,3]~road.surv[,3],col=3)
# points(new.road.surv[,4]~new.road.surv[,4],col=4)
# points(new.road.surv[,5]~new.road.surv[,5],col=5)
# plot(new.road.surv[,1]~persons[,1], xlab="person-nights")
# points(new.road.surv[,2]~persons[,2], col=2)
# points(new.road.surv[,3]~persons[,3], col=3)
# points(new.road.surv[,4]~persons[,4], col=4)
# points(new.road.surv[,5]~persons[,5], col=5)
#
