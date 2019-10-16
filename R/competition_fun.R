#' Convert numbers at age to numbers at length
#'
#' @param ALK age-length key from SS report
#' @param N dataframe with N-at-age for species 1 (vermilion)
#' @param year current year in simulation
#' @param Nrs dataframe with N-at-age for species 2 (red)
#' @param ALK_r age-length key for red snapper or (species 2)
#' @param beta.vec vector of length-specific betas from TMB model
#' @param carry.K combined carrying capacity
#' @export
#'

competition_fun <- function(ALK, year, N, Nrs, ALK_r, beta.vec, carry.K){

  #step one, convert to NLen
  VS.nl <- age_to_len(ALK, N, year, T)
  RS.nl <- age_to_len(ALK_r, Nrs, year, T)

  colnames(RS.nl) <- row.names(ALK_r)
  colnames(VS.nl) <- row.names(ALK)
  #aggregate RS lengths
  RS.nl <- RS.nl %>%
    as.data.frame() %>%
    mutate("14" = sum(.[,1:4]),
           "20" = sum(.[,5:7]),
           "26" = sum(.[,8:10]),
           "32" = sum(.[,11:13]),
           "38" = sum(.[,14:16]),
           "44" = sum(.[,17:19]),
           "50" = sum(.[,20:22]),
           "56" = sum(.[,23:25]),
           "62" = sum(.[,26:28]),
           "68" = sum(.[,29:31]),
           "74" = sum(.[,32:34]),
           "80" = sum(.[,35:52])) %>%
    select(-c(1:3, 5,6, 8,9, 11,12, 14,15, 17,18, 20,21, 23,24, 26,27, 29,30, 32,33, 35, 36, 38:52))

  #step two, calculate number that died from competition (don't include year 0)
  comp.dead <-  VS.nl[1, ] * ((carry.K - VS.nl[1,] - (beta.vec * RS.nl[1,]))/carry.K)
  colnames(comp.dead) <- colnames(VS.nl)

  #step three, convert back to numbers at age

  matrix(NA, nrow = ncol(ALK_r), ncol = nrow(ALK_r))
  Natlen <-matrix(NA, nrow = ncol(comp.dead)-1, ncol = ncol(ALK)-1)

  ages <- seq(1,14)
  df <- matrix(0, nrow = 12, ncol = 14)
  colnames(df) <- seq(1,14)
  for(i in 2:ncol(comp.dead)){
    na <- sample(c(1:14), size = round(comp.dead[,i]), replace = T, prob = ALK[i,-1])
    tab <- as.data.frame(table(na))
    tab$na <- as.numeric(as.character(tab$na))

    for(j in 1:nrow(tab)){
      ind <- which(colnames(df) == tab$na[j])
      df[i, ind] <- tab$Freq[j]

    }

  }

  return(colSums(df))
}
