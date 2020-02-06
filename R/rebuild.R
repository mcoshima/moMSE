#' Find the time it takes to rebuild the stock once SSB is below 30\%
#'
#' @param dir. directory to send new forecast file
#' @param dat.list list with the sequence of years
#' @param lin if TRUE running on a linux system, default is FALSE
#' @keywords rebuild, t_target
#' @import r4ss dplyr
#' @importFrom magrittr %>%
#' @export
#'

rebuild_ttarg <- function(dir., dat.list, lin = FALSE){

  gen <- 7
  nfishfleet <- as.numeric(dat.list$N_fishfleet) + 2
  nareas <- dat.list$N_areas
  year.seq <- as.numeric(dat.list$year_seq)
  yr <- floor(year.seq[year])
  shrimp.forecast.h <- 0.07356127

  fcast. <- SS_readforecast(paste0(dir., "/forecast.ss"), Nfleets = 5, Nareas = 1, version = "3.24")

  years <-  rep(seq(yr+1, yr+60, by =1),each = 3)

  no_catches <- data.frame("Year" = years,
                           "Seas" = rep(1, length(years)),
                           "Fleet" = rep(c(1,2,3), length(years)/3),
                           "Catch" = rep(0, length(years)))

  f1 <- dat.list$RS_projections %>%
    dplyr::filter(Year > yr & Year <= yr+60) %>%
    mutate(Seas = rep(1, nrow(.)),
           Fleet = rep(5, nrow(.)),
           Catch = RS_relative) %>%
    select(Year, Seas, Fleet, Catch)

  f2 <- data.frame("Year" = unique(years),
                   "Seas" = rep(1, length(unique(years))),
                   "Fleet" = rep(4, length(unique(years))),
                   "Catch" = rep(shrimp.forecast.h, length(unique(years))))


  zero_catches <- bind_rows(f1,f2, no_catches) %>% arrange(Year, Fleet)
  row.names(zero_catches) <- NULL

  fcast.$Ncatch <- nrow(zero_catches)
  fcast.$InputBasis <- 99
  if(nrow(zero_catches) > 0){
    fcast.$ForeCatch <- NULL
    fcast.$ForeCatch <- zero_catches
  }
  MO_writeforecast(fcast., dir = dir., overwrite = T)
  if(isTRUE(lin)){
    system(paste("cd", dir., "&& SS3 > /dev/null 2>&1", sep = " "))
  }else{
      shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))}

  rep.file <- MO_SSoutput(dir = dir., verbose = F, printstats = F)

  recovered <- rep.file$derived_quants %>%
    dplyr::filter(str_detect(Label,"Bratio_")) %>%
    select(Label, Value) %>%
    dplyr::filter(Value >= 0.299) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::filter(Year >= yr)

  if(nrow(recovered) == 1){
    t_min <- recovered$Year - yr

  }else{
    temp.year <- recovered %>% dplyr::filter(Year > yr) %>% slice(1)
    t_min <- temp.year$Year - yr
  }

  t_targ <- ifelse(t_min < 10, 10, t_min + gen)

  return(t_targ)

}




#' Find the catch and f for time t_target required to rebuild stock
#' @param year year
#' @param dir. directory to send new forecast file
#' @param dat.list list with the sequence of years
#' @param t_targ calculated by rebuild_ttarg, the number of years it will take to rebuild stock
#' @param lin if TRUE running on a linux system, default is FALSE
#' @keywords rebuild, t_target, catch, F
#' @import r4ss dplyr
#' @importFrom magrittr %>%
#' @export
#'

rebuild_f <- function(year, dir., dat.list, t_targ, lin = FALSE){

  nfishfleet <- dat.list$N_totalfleet + 1
  nareas <- dat.list$N_areas
  shrimp.forecast.h <- 0.07356127
  fcast. <- SS_readforecast(paste0(dir., "/forecast.ss"), Nfleets = 5, Nareas = 1, version = "3.24")
  year.seq <- as.numeric(dat.list$year_seq)
  yr <- floor(year.seq[year])
  years <- seq(yr, yr + 60)

  f1 <- dat.list$RS_projections %>%
    filter(Year > yr & Year <= yr+60) %>%
    mutate(Seas = rep(1, nrow(.)),
           Fleet = rep(5, nrow(.)),
           Catch = RS_relative) %>%
    select(Year, Seas, Fleet, Catch)

  f2 <- data.frame("Year" = unique(years),
                   "Seas" = rep(1, length(unique(years))),
                   "Fleet" = rep(4, length(unique(years))),
                   "Catch" = rep(shrimp.forecast.h, length(unique(years))))

  Fore_h <- bind_rows(f1,f2) %>% arrange(Year, Fleet)

  fcast.$ForeCatch <- Fore_h
  fcast.$Ncatch <- nrow(Fore_h)
  fcast.$InputBasis <- 99
  #fcast.$SPRtarget <- fcast.$SPRtarget

  MO_writeforecast(fcast., dir = dir., overwrite = T)

  if(isTRUE(lin)){
    system(paste("cd", dir., "&& SS3 > /dev/null 2>&1", sep = " "))
  }else{
    shell(paste("cd/d", dir., "&& ss3 -nohess >NUL 2>&1", sep = " "))}

  temp.rep <- MO_SSoutput(dir = dir., verbose = F, printstats = F)

  recovered <- temp.rep$derived_quants %>%
    dplyr::filter(str_detect(Label,"Bratio_")) %>%
    select(Label, Value) %>%
    dplyr::filter(Value >= 0.299) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::filter(Year >= yr & Year <= yr+t_targ)

  recov <- ifelse(nrow(recovered) > 0, TRUE, FALSE)

  if(recov == FALSE){

    SPRtarget <- seq(fcast.$SPRtarget, 0, by = -.01)

    for(i in SPRtarget){

      fcast. <- SS_readforecast(paste0(dir., "/forecast.ss"), Nfleets = 5, Nareas = 1, version = "3.24")
      fcast.$SPRtarget <- paste(i, "# SPR target (e.g. 0.40)", sep = " ")
      MO_writeforecast(fcast., dir = dir., overwrite = T)
      if(isTRUE(lin)){
        system(paste("cd", dir., "&& SS3 > /dev/null 2>&1", sep = " "))
      }else{
        shell(paste("cd/d", dir., "&& ss3 -nohess >NUL 2>&1", sep = " "))}
      temp.rep <- MO_SSoutput(dir = dir., verbose = F, printstats = F)

      recovered <- temp.rep$derived_quants %>%
        dplyr::filter(str_detect(Label,"Bratio_")) %>%
        select(Label, Value) %>%
        dplyr::filter(Value >= 0.299) %>%
        separate(Label, into = c("Label", "Year"), sep = "_") %>%
        mutate(Year = as.numeric(Year)) %>%
        dplyr::filter(Year >= yr & Year <= yr+t_targ)

      if(nrow(recovered) >= 1){
        break
      }
    }
  }

  temp.rep <- MO_SSoutput(dir = dir., verbose = F, printstats = F)
  nw_catch <- temp.rep$derived_quants %>%
    dplyr::filter(str_detect(Label, "ForeCatch_")) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::filter(Year > yr & Year <= yr+t_targ) %>%
    select(Value) %>%
    pull()

  nw_f <- temp.rep$derived_quants %>%
    dplyr::filter(str_detect(Label, "F_")) %>%
    separate(Label, into = c("Label", "Year"), sep = "_") %>%
    mutate(Year = as.numeric(Year)) %>%
    dplyr::filter(Year > yr & Year <= yr+t_targ) %>%
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
