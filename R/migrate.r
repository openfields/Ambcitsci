# adult population at start of season

# proportion that breeds

# road mortality, inbound

# road mortality, outbound

# other breeding mortality


migrate <- function(num_inbound, night_prop, fec, rmort, bmort){
#   input: number of inbound adults, proportion of adults migrating each night, fecunedity, road mortality, breeding mortality
#   processing:
#       mortality of adults moving per night
#       fecundity of surviving adults
#       mortality of adults that survive inbound migration
#   output:
#       offspring produced
#       number of adults that will make outbound migration

night_prop*num_inbound -> night_inb
night_inb <- round(night_inb)
inb_surv <- rbinom(night_inb, 1, (1-rmort))
surv_fec <- inb_surv*fec


}
