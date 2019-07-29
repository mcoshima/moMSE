
#' Convert fish age to length using von Bertalanffy 3-param equation
#'
#' @param N matrix with numbers at age
#' @param age max age
#' @param year current year in simulation
#' @param linf L-infinity value
#' @param k K coefficient
#' @param t0 age at length 0
#' @param cvdist vector of CVs to sample from
#' @keywords age, length, von Bertalanffy
#' @export
#' @examples
#' age.to.length()

age.to.length <- function(N,age,year,linf,k,t0,cvdist){
  len.vec <- c()
  for(n in 1:round(N[year,age+2],0)){
    len.vec[n] <- linf*(1-exp(-k*(age-t0)))*exp(sample(cvdist, 1))
  }
  return(len.vec)
}

#' Calcualte total mortality at age
#'
#' @param M natural mortality at age, matrix or df
#' @param sel matrix of selectivities at age for each fleet
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @keywords mortality
#' @export
#' @examples
#' zatage()
#'
zatage <- function(M,sel,f.by.fleet){
  f <- matrix(NA, nrow = length(f.by.fleet), ncol = Nages)
  for(fleet in 1:4){

    f[fleet,] <- as.numeric(asel[fleet,])*as.numeric(f.by.fleet[fleet])
  }
  Z <- as.numeric(M[year,]) + colSums(f)

  Z
}

#' Converts catch in numbers to catch in biomass
#'
#' @param sel Selectivity-at-age matrix
#' @param N matrix with numbers at age
#' @param year current year in simulation
#' @param z total mortality at age vector
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @keywords catch
#' @export
#' @examples
#' catch.in.biomass()
#'
catch.in.biomass <- function(sel,N,year,z,f.by.fleet){
  for(fleet in 1:2){
    for(age in 1:15){

      catch[fleet, age] <-
        ((f.by.fleet[fleet]) / z[age]) * (1 - exp(- z[age])) * sel[fleet, age] * N[year-1, age+1] * wtatage[1  ,age]

    }}
  catch
}


#' Calculates numbers at age of catch
#'
#' @param fsel Selectivity-at-age matrix of fisheries
#' @param N matrix with numbers at age
#' @param year current year in simulation
#' @param z total mortality at age vector
#' @param f.by.fleet Fishing mortality by fleet for year y
#' @param se vector of log(catch error)
#' @keywords catch
#' @export
#' @examples
#' catch.in.number()
#'
catch.in.number <- function(fsel,N,year,z,f.by.fleet,se){
  for(fleet in 1:4){
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
#' @param wtatage matrix of weights at age
#' @param Natage default is TRUE, T converts N-at-age to biomass, F converts B-at-age to numbers
#' @param age0 defualt is FALSE, will exclude age0 in calculations, TRUE will include them
#' @keywords numbers-at-age, biomass-at-age
#' @export
#' @examples
#' num_to_bio()
#'
num_to_bio <- function(df, wtatage, Natage = T, age0 = F){

  #age0 = F means you don't want to include age0 in calculations, if T you do

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
#' @param Flt max fishing fleet number (3)
#' @param asel selectivity-at-age matrix
#' @param year current year in simulation
#' @keywords biomass-at-age
#' @export
#' @examples
#' simBatage()
#'
simBatage <- function(N, Flt, asel, year){

  for(fleet in 1:Flt){
    for(age in 1:Nages){
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
#' @param Flt number of fleets (2)
#' @param lsel selectivity-at-length matrix
#' @param year current year in simulation
#' @keywords biomass-at-length
#' @export
#' @examples
#' simBatlen()
#'
simBatlen <- function(Nlen, Flt, lsel, year){

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
#' @param ageerror vector of error for each age
#' @keywords age composition
#' @export
#' @examples
#' simAgecomp()
#'
simAgecomp <-  function(catch.by.fleet, year, ageerror){
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
#' @param q vector of catchability values for each fleet
#' @param b vector of vulnerable biomass for each fleet
#' @param se error for each index
#' @param year current year in simulation
#' @keywords index of abundance
#' @export
#' @examples
#' simIndex()
#'
simIndex <- function(q,b,se,year){

  for(fleet in 1:6){

    I[year, fleet] <- rlnorm(1,log(q[fleet]*b[fleet]),as.numeric(CPUE.se[fleet,2]^2))

  }
  return(I[year,])
}

#' Generates competition index based on relative Red Snapper abundances
#'
#' @param r rate of population growth
#' @param beta competition coeffiecient
#' @param N matrix of numbers at age
#' @param sigma standard deviation
#' @param K carrying capacity
#' @param Nj abundance of species j
#' @keywords competition, index
#' @export
#' @examples
#' simCompetition()
#'
simCompetition <- function(r, beta, N, sigma, K, Nj){

  Ni <- sum(N[year,-1])
  exp.N <- r*Ni*(1-(K - Ni - beta*Nj)/K)

}

