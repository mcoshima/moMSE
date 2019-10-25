#' Function to solve for F by fleet based on catch for that year
#' @param proj_catch the expected catch from fleet for the year (in numbers)
#' @param fleet fleet you are calculating F for
#' @param dat.list list including M, Nages, weight-at-age, and selectivity
#' @param year year index
#' @param N matrix of numbers-at-age, first column is year
#' @param total_catch is the projected catch a combined catch (T) or only for that fleet (F)
#' @return The F value that achieves the projected catch level
#' @keywords F, projections
#' @export
solve_for_f <- function(proj_catch, fleet, dat.list, year, N, total_catch = T){

  F_upper <- 2
  F_lower <- 0
  test_f <- (F_upper + F_lower)/2
  est_catch <- c()
  Nages <- dat.list$Nages
  M <- dat.list$M
  S <- dat.list$age_selectivity
  wt <- dat.list$wtatage
  ii <- 1
  if (total_catch == T) {
    prop_catch <- proj_catch * dat.list$catch_proportions[fleet]
  }
  else {
    prop_catch <- proj_catch
  }

  MN <- sum(N[year,-1] - N[year,-1]*exp(-M[1,])) - prop_catch
  Z <- log(sum(N[year,-1])/MN)

  if (fleet == 1 | fleet == 2) {
    est_catch <- sum(wt[1, ] * ((S[fleet, ] * test_f)/Z) * N[year, -1] * (1 - exp(-Z)))
  }

  else {
    est_catch <- sum(((S[fleet, ] * test_f)/Z) * N[year, -1] * (1 - exp(-Z)))
  }
  catch_diff <- est_catch - prop_catch
  while (abs(catch_diff) > 1e-06 & ii < 200) {
    if (catch_diff > 0) {
      F_upper <- test_f
    }
    else {
      F_lower <- test_f
    }
    test_f <- (F_upper + F_lower)/2
    est_catch <- 0

    MN <- sum(N[year,-1] - N[year,-1]*exp(-M[1,])) - prop_catch
    Z <- log(sum(N[year,-1])/MN)

    if (fleet == 1 | fleet == 2) {
      est_catch <- sum(wt[1, ] * ((S[fleet, ] * test_f)/Z) * N[year, -1] * (1 - exp(-Z)))
    }
    else {
      est_catch <- sum(((S[fleet, ] * test_f)/Z) * N[year, -1] * (1 - exp(-Z)))
    }
    catch_diff <- est_catch - prop_catch
    ii <- ii + 1
  }
  Fproj <- test_f
  return(Fproj)
  }


#' Function to solve for harvest(exploitation) rate of a fleet based on catch for that year
#' @param catch the expected catch from fleet for the year (in units of the fleet)
#' @param fleet fleet you are calculating F for
#' @param N matrix of numbers-at-age, first column is year
#' @param year year index
#' @param dat.list list including M, Nages, weight-at-age, and selectivity
#' @param total.catch is the projected catch a combined catch (T) or only for that fleet (F)
#' @return The harvest rate that achieves the projected catch level
#' @keywords harvest rate, projections
#' @export

H_rate <- function(catch, fleet, N, year, dat.list, total.catch = F){

  wt <- dat.list$wtatage
  S <- dat.list$age_selectivity
  c_prop <- dat.list$catch_proportions

  if(total.catch == T){
    catch <- catch/c_prop[fleet]
  }
  if(fleet == 1 | fleet == 2){
    hrate <- catch/sum(N[year-1,-1]*wt[1,]*S[fleet,])
  }else{
    hrate <- catch/sum(N[year-1,-1]*S[fleet,])
  }

  return(hrate)
}
