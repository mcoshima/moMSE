#' Function to solve for F by fleet based on catch for that year
#' @param proj_catch the expected catch from fleet for the year
#' @param fleet fleet you are calculating F for
#' @param dat.list list including M, Nages, weight-at-age, and selectivity
#' @param year year index
#' @param N matrix of numbers-at-age, first column is year
#' @return The F value that achieves the projected catch level
#' @keywords F, projections
#' @export
solve_for_f <- function(proj_catch, fleet, dat.list, year, N){

  F_upper <- 2
  F_lower <- 0
  test_f <- (F_upper + F_lower)/2
  est_catch <- c()
  Nages <- dat.list$Nages
  wt_a <- dat.list$wtatage
  M <- dat.list$M
  S <- dat.list$age_selectivity
  ii <- 1

  estz <- M[year,] + S[fleet,] * test_f
  est_catch <- sum(wt_a * S[fleet,] * test_f * N[year,] * (1-exp(-estz))/estz)
  catch_diff <- est_catch - proj_catch

  while(abs(catch_diff) > 1e-6  & ii < 200){

    if(catch_diff > 0){
      F_upper <- test_f
    }else{
      F_lower <- test_f
    }
    test_f <- (F_upper + F_lower)/2
    est_catch <- 0
    estz <- M[year,] + S[fleet,] * test_f
    est_catch <- sum(wt_a * S[fleet,] * test_f * N[year,] * (1-exp(-estz))/estz)
    catch_diff <- est_catch - proj_catch
    ii <- ii + 1

  }

  Fproj <- test_f
  return(Fproj)

}
