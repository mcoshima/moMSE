
#' Calcualte total mortality at age
#'
#' @param dat.list Contains natural mortality at age, matrix or df and selectivity-at-age matrix
#' @param year current year
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @keywords mortality
#' @export
#'
#'
zatage <- function(dat.list,year,f.by.fleet){

  M <- dat.list$M
  asel <- dat.list$age_selectivity
  Nages <- dat.list$Nages
  Nfleet <- dat.list$N_totalfleet
  f <- matrix(NA, nrow = Nfleet, ncol = Nages)
  for(fleet in 1:Nfleet){

    f[fleet,] <- as.numeric(asel[fleet,])*as.numeric(f.by.fleet[fleet])
  }
  Z <- as.numeric(M[year,]) + colSums(f)

  Z
}
#' Converts catch in numbers to catch in biomass
#'
#' @param dat.list Contains selectivity-at-age matrix and weight-at-age matrix
#' @param N matrix with numbers at age
#' @param year current year in simulation
#' @param z total mortality at age vector
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @keywords catch
#' @export
#'
#'
catch.in.biomass <- function(dat.list,N,year,z,f.by.fleet){

  sel <- dat.list$age_selectivity
  wtatage <- dat.list$wtatage
  catch <- matrix(data = NA, nrow = 2, ncol = 15)
  for(fleet in 1:2){
    for(age in 1:15){

      catch[fleet, age] <-
        ((f.by.fleet[fleet]) / z[age]) * (1 - exp(- z[age])) * sel[fleet, age] * N[year-1, age+1] * wtatage[1  ,age]

    }}
  catch
}



#' Calculates numbers at age of catch
#'
#' @param dat.list Contains selectivity-at-age matrix of fisheries and catch SE
#' @param N matrix with numbers at age
#' @param year current year in simulation
#' @param z total mortality at age vector
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @keywords catch
#' @export
#'
#'

catch.in.number <- function(dat.list,N,year,z,f.by.fleet){

  sel <- dat.list$age_selectivity
  bycatch.sel <- sel[4,]
  bycatch.sel[,2] <- .75
  se <- dat.list$catch_se
  Nfleet <- dat.list$N_totalfleet
  catch <- matrix(data = NA, nrow = Nfleet, ncol = 15)
  for(fleet in 1:Nfleet){
    for(age in 1:15){

      catch[fleet, age] <- ((f.by.fleet[fleet]) / z[age]) * (1 - exp(- z[age])) * sel[fleet, age] * N[year-1, age+1] *exp(rnorm(1,0,se[fleet]))

    }
  }

  catch
}

#' Converts numbers-at-age to biomass-at-age or vice versa.
#' Inputs need to be a dataframe, not a matrix.
#'
#' @param df dataframe of values you are converting
#' @param dat.list list that contains matrix of weights at age
#' @param Natage default is TRUE, T converts N-at-age to biomass, F converts B-at-age to numbers
#' @param age0 defualt is FALSE, will exclude age0 in calculations, TRUE will include them
#' @keywords numbers-at-age, biomass-at-age
#' @export
#'
#'
num_to_bio <- function(df, dat.list, Natage = T, age0 = F){

  wtatage <- dat.list$wtatage

  if(Natage == T){
    Batage <- matrix(NA, nrow= nrow(df), ncol = ncol(df))
    if(age0 == F){
      for(i in 1:nrow(df)){

        Batage[i,] <- unlist(df[i,]*wtatage[1,-1])

      }
    } else {
      for(i in 1:nrow(df)){

        Batage[i,] <- unlist(df[i,]*wtatage[1,])

      }

    }
    return(Batage)
  }else {
    Natage <- matrix(NA, nrow= nrow(df), ncol = ncol(df))
    if(age0 == F){
      for(i in 1:nrow(df)){

        Natage[i,] <- unlist(df[i,]/wtatage[1,-1])

      }

    } else {
      for(i in 1:nrow(df)){

        Natage[i,] <- unlist(df[i,]/wtatage[1,])

      }

    }

    return(Natage)

  }
}


#' Calculates biomass at age for fishery dependent surveys
#'
#' @param N matrix with numbers at age
#' @param dat.list list with weight-at-age matrix, selectivity-at-age matrix, number of ages, and number of fishing fleets
#' @param year current year in simulation
#' @keywords biomass-at-age
#' @export
#'
#'
simBatage <- function(N, dat.list, year){
  #Flt is the max = 3
  #asel is the age selectivity matrix

  wtatage <- dat.list$wtatage
  asel <- dat.list$age_selectivity
  Nages <- dat.list$Nages
  Flt <- dat.list$N_fishfleet
  b.age <- matrix(data = NA, nrow = 3, ncol = 15)
  for(fleet in 1:Flt){
    for(age in 1:Nages){#for fishery dependent surveys
      if(fleet < 3){

        b.age[fleet,age] <- N[year,age+1] *wtatage[,age] * asel[fleet,age]

      }else{

        b.age[fleet,age] <- N[year,age+1] * asel[fleet,age]

      }
    }
  }

  b.age
}

#' Calculates biomass at age for fishery independent surveys
#'
#' @param Nlen matrix with numbers at length
#' @param dat.list list with the number of fishery independent surveys and a selectivity-at-length matrix
#' @param year current year in simulation
#' @keywords biomass-at-length
#' @export
#'
#'
simBatlen <- function(Nlen, dat.list, year){
  #Flt is the number of fishery-independent surveys

  Flt <- dat.list$N_survey
  lsel <- dat.list$length_selectivity
  b.len <- matrix(NA, nrow = 2, ncol =12)
  for(fleet in 1:Flt){
    for(len in 1:ncol(Nlen)){
      if(fleet == 1){
        b.len[fleet,len] <- Nlen[year-1,len]*lsel[fleet,len]
      }else{
        b.len[fleet,len] <- Nlen[year,len]*lsel[fleet,len]
      }
    }
  }
  b.len
}


#' Generates age composition data from simulated catches
#'
#' @param catch.by.fleet matrix with catches at age by fleet
#' @param year current year in simulation
#' @param dat.list list with vector of error for each age and vector of the years in simulation
#' @keywords age composition
#' @export
#'
#'
simAgecomp <-  function(catch.by.fleet, year, dat.list){
  year.seq <- dat.list$year_seq
  ageerror <- dat.list$ageerror
  agecompinfo.list<- list(
    Yr = rep(floor(year.seq[year]),3),
    Seas = rep(1,3),
    FltSvy = seq(1,3),
    Gender = rep(0,3),
    Part = rep(2,3),
    Ageerr = rep(1,3),
    Lbin_lo = rep(-1,3),
    Lbin_hi = rep(-1,3),
    Nsamp = rep(75,3))

  n = 500
  prob <- catch.by.fleet[,-1]/apply(catch.by.fleet[,-1], 1, sum)
  flt1comp <- rmultinom(1, size = n, prob = prob[1,]*ageerror)/n
  flt2comp <- rmultinom(1, size = n, prob = prob[2,]*ageerror)/n
  flt3comp <- rmultinom(1, size = n, prob = prob[3,]*ageerror)/n


  comps <- rbind(t(flt1comp), t(flt2comp), t(flt3comp))
  colnames(comps) <- paste0("a",1:14)
  compinfo.list <- as.data.frame(do.call(cbind, agecompinfo.list))
  agecomp.list[[year]] <- cbind(compinfo.list, comps)
  return(agecomp.list[[year]])

}


#' Generates indices of abundance based on simulated catch
#'
#' @param dat.list list with catchability and CPUE SE values
#' @param b vector of vulnerable biomass for each fleet
#' @keywords index of abundance
#' @export
#'
simIndex <- function(dat.list,b){

  q <- dat.list$q
  se <- dat.list$CPUE_se
  I <- matrix(data = NA, nrow =1, ncol = 6)
  for(fleet in 1:6){

    I[, fleet] <- rlnorm(1,log(q[fleet]*b[fleet]),as.numeric(se[fleet,2]^2))

  }
  return(I)
}


simCompetition <- function(r, beta, N, sigma, K, Nj){

  Ni <- sum(N[year,-1])
  exp.N <- r*Ni*(1-(K - Ni - beta*Nj)/K)

}

#' Generates competition index based on relative Red Snapper abundances
#'
#' @param r rate of population growth
#' @param beta competition coeffiecient
#' @param N matrix of numbers at age
#' @param sigma standard deviation
#' @param K carrying capacity
#' @param Nj abundance of species j
#' @param year current year
#' @keywords competition, index
#' @export
#'
#'
simCompetition <- function(r, beta, N, sigma, K, Nj, year){

  Ni <- sum(N[year,-1])
  exp.N <- r*Ni*(1-(K - Ni - beta*Nj)/K)

}

