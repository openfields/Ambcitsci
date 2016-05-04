# read in data
# fit distribution
# simulation function: inputs - population size, pop distribution function
# processing: calculate # individuals/night and # nights
# output: returns # individuals/night and # nights

# for processing: could do function or summary statistics for function

read.table("~/Documents/bignight/amma_cross1.csv", header=TRUE, sep=",")->cross1

read.csv("~/Documents/bignight/amma_2015.csv", header=TRUE) -> amma15
amma15$tot <- amma15$live+amma15$dead
qplot(amma15$site_num,amma15$tot,col=amma15$volhour)
qplot(amma15$site_num,amma15$tot,col=amma15$town)

#sites 6, 8 12 & 19 had more than 3 surveys done in a season

# for a year, could get proportion of counts for sites with 4 surveys, then randomly draw from those to get population distribution

amma15[(amma15$site_num==6|amma15$site_num==8|amma15$site_num==12|amma15$site_num==16),]->amma4

amma4$live[is.na(amma4$live)]<-0
amma4$dead[is.na(amma4$dead)]<-0
amma4$tot[is.na(amma4$tot)]<-0

which(amma15$tot>0) -> actnight
anight <- actnight[1:23]
cor(amma15$tot[anight],as.numeric(amma15$volhour[anight]))


