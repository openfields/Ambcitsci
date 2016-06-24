# example script to run sample population data
# created by Will Fields, 05 Nov 2015

# parameters for breeders and nonbreeders
breed0 <- 75
nonbreed0 <- 25
Sbreed <- 0.75
Snonbreed <- 0.75
tr2nonbr <- 0.3
tr2br <- 0.5

# process for breeders
b1 <- breedsurv(breed0, Sbreed, tr2nonbr)
nb1 <- nonbreedsurv(nonbreed0, Snonbreed, tr2br)

breed1 <- b1$br + nb1$br
nonbreed1 <- b1$non + nb1$non

