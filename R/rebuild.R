#' Find the time it takes to rebuild the stock once SSB is below 30\%
#'
#' @param forefile full or relative path to forecast file
#' @param dir. directory to send new forecast file
#' @param dat.list list with the sequence of years
#' @keywords rebuild, t_target
#' @export
#'
rebuild_ttarg <- function(forefile, dir., dat.list){

  gen <- 7
  nfishfleet <- 5
  nareas <- dat.list$N_areas
  year.seq <- as.numeric(dat.list$year_seq)
  yr <- floor(year.seq[year])

  fcast. <- SS_readforecast(forefile, Nfleets = as.numeric(nfishfleet), Nareas = nareas)

  year.seq <- as.numeric(dat.list$year_seq)
  yr <- floor(year.seq[year])
  years <-  rep(seq(yr+1, 2064, by =1),each = 4)
  zero_catches <- data.frame("Year" = years,
                             "Seas" = rep(1, length(years)),
                             "Fleet" = rep(c(1,2,3,4), length(years)/4),
                             "Catch" = rep(0, length(years)))
  row.names(zero_catches) <- NULL

  fcast.$Ncatch <- paste0(nrow(zero_catches),
                          " # Number of forecast catch levels to input (else calc catch from forecast F)")
  fcast.$InputBasis <- paste0(99, " # basis for input Fcast catch: 2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)")
  if(nrow(zero_catches) > 0){
    fcast.$ForeCatch <- print(zero_catches, row.names = F)
  }
  SS_writeforecast(fcast., dir = dir., overwrite = T)
  shell(paste("cd/d", dir., "&& ss3", sep = " "))

  rep.file <- SS_output(dir = dir., verbose = F, printstats = F)

  recovered <- rep.file$derived_quants %>%
    filter(str_detect(Label,"Bratio_")) %>%
    select(Label, Value) %>%
    filter(Value >= 0.299 & Value <= 0.31) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year >= yr)

  if(nrow(recovered) == 1){
    t_min <- recovered$Year - yr

  }else{
    temp.year <- recovered %>% filter(Year > yr) %>% slice(1)
    t_min <- temp.year$Year - yr
  }

  t_targ <- ifelse(t_min < 10, 10, t_min + gen)

  return(t_targ)

}



#' Find the catch and f for time t_target required to rebuild stock
#'
#' @param forefile full or relative path to forecast file
#' @param dir. directory to send new forecast file
#' @param dat.list list with the sequence of years
#' @param t_targ calculated by rebuild_ttarg, the number of years it will take to rebuild stock
#' @keywords rebuild, t_target, catch, F
#' @export
#'

rebuild_f <- function(forefile, dir., dat.list, t_targ){

  nfishfleet <- dat.list$N_fishfleet + 1
  nareas <- dat.list$N_areas
  fcast. <- SS_readforecast(forefile, Nfleets = nfishfleet, Nareas = nareas)

  fcast.$ForeCatch <- NULL
  fcast.$Ncatch <- 0
  fcast.$InputBasis <- -1
  fcast.$SPRtarget <- fcast.$SPRtarget

  SS_writeforecast(fcast., dir = dir., overwrite = T)

  shell(paste("cd/d", dir., "&& ss3", sep = " "))

  temp.rep <- SS_output(dir = dir., verbose = F, printstats = F)

  recovered <- rep.file$derived_quants %>%
    filter(str_detect(Label,"Bratio_")) %>%
    select(Label, Value) %>%
    filter(Value >= 0.299 & Value <= 0.31) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year >= yr & Year <= yr+t_targ)

  recov <- ifelse(nrow(recovered) > 0, TRUE, FALSE)

  if(recov == FALSE){

    SPRtarget <- seq(fcast.$SPRtarget, 0, by = -.01)

    for(i in SPRtarget){

      fcast. <- SS_readforecast(forefile, Nfleets = nfishfleet, Nareas = nareas)
      fcast.$SPRtarget <- paste(i, "# SPR target (e.g. 0.40)", sep = " ")
      SS_writeforecast(fcast., dir = dir., overwrite = T)
      shell(paste("cd/d", dir., "&& ss3", sep = " "))
      temp.rep <- SS_output(dir = dir., verbose = F, printstats = F)

      recovered <- rep.file$derived_quants %>%
        filter(str_detect(Label,"Bratio_")) %>%
        select(Label, Value) %>%
        filter(Value >= 0.299 & Value <= 0.31) %>%
        separate(Label, into = c("Label", "Year"), sep = "_") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(Year >= yr & Year <= yr+t_targ)

      if(nrow(recovered) >= 1){
        break
      }
    }
  }

  temp.rep <- SS_output(dir = dir., verbose = F, printstats = F)
  nw_catch <- temp.rep$derived_quants %>%
    filter(str_detect(Label, "ForeCatch_")) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year > yr & Year <= yr+t_targ) %>%
    select(Value) %>%
    pull()

  nw_f <- temp.rep$derived_quants %>%
    filter(str_detect(Label, "F_")) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year > yr & Year <= yr+t_targ) %>%
    select(Value) %>%
    pull()

  rebuild.ls <- list(catch = nw_catch, f = nw_f)

  return(rebuild.ls)


}



#' Find the acceptable probability of going past the OFL (P*)
#'
#' @param Assess_info number for how much info is available for assessement
#' @param OFL_uncert number describing the level of uncertainty acknowledged in the model
#' @param Retro_pats number describing the retrospective patterns
#' @param Environ number describing if environmental covariates were included in model
#' @param min.risk the level of minimal risk (default at .3)
#' @param max.risk the level of max risk (default at .5)
#' @keywords rebuild, p-star
#' @export
#'
p_star <- function(Assess_info = "2", OFL_uncert = "2", Retro_pats = "3", Environ = "3", min.risk = .3, max.risk = .5){

  a <- switch(Assess_info,
            "1" = 0,
            "2" = .67,
            "3" = 1.33,
            "4" = 2)
  b <- switch(OFL_uncert,
            "1" = 0,
            "2" = .67,
            "3" = 1.33,
            "4" = 2)
  c <- switch (Retro_pats,
             "1" = 0,
             "2" = 1,
             "3" = 2)
  d <- switch(Environ,
            "1" = 0,
            "2" = 1,
            "3" = 2)

  a <- a * 1
  b <- b * .33
  c <- c * .33
  d <- d * .33
  Dim <- a + b + c + d
  alpha <- -log(max.risk)
  beta <- -((alpha + log(min.risk))/3.998)
  p <- exp(-alpha - beta * Dim)

  return(p)
}
