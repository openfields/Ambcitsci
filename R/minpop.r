minpop <- function(datmat){
# function to find minimum population of matrix, assume data for different life stages are in columns, each row is diff time step
apply(datmat, 1, sum) -> tpop
min(tpop) -> minpop
return(minpop)

}
