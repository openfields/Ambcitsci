minex <- function(inputs){
mins <- rep(NA, dim(inputs)[3])
for(i in dim(inputs)[3]){
  minpop(inputs[,,i]) -> mins[i]
}
return(mins)
}