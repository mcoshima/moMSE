#' Convert numbers at age to numbers at length
#'
#' @param ALK age-length key from SS report
#' @param Nage dataframe with N-at-age
#' @param year current year in simulation
#' @param age_0 include age 0 in numbers at age matrix, default is true
#' @export
#'


age_to_len <- function(ALK, Nage, year, age_0 = T){

  if("Year" %in% colnames(Nage)){
    col. <- which(colnames(Nage) == "Year")
    Nage <- Nage[,-col.]
  }
  if(age_0 == F){
    col. <- which(colnames(Nage) == "Zero" | colnames(Nage) == "0")
    Nage <- Nage[,-col.]
    ALK <- ALK[,-1]
  }
  nl <- matrix(NA, nrow = ncol(ALK), ncol = nrow(ALK))
  NatL <- matrix(NA, nrow = 1, ncol = nrow(ALK))

  for (i in 1:ncol(ALK)) {
    nl[i, ] <- Nage[year - 1, i] * ALK[, i]
  }
  NatL[1, ] <- colSums(nl)
  return(NatL)
}


