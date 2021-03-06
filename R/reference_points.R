#' Find the SPR input value for forecast.ss file to achieve SPR 30\%
#'
#' @param dir. directory of where the original and new forecast file are. Also sends notifications to phone (pbpost) after each run and when 30\% is reached.
#' @param nfleet the number of non-survey fleets for reading in forecast file
#' @param notifications True or False, whether to send push notifications to phone or not. Set to F if you don't have internet connection.
#' @param lin if TRUE running on linux system, default if FALSE
#' @import dplyr r4ss RPushbullet stringr
#' @importFrom magrittr %>%
#' @keywords SPR
#' @export
#'

find_spr <- function(dir., nfleet, notifications = T, lin = FALSE) {
  rep.file <- MO_SSoutput(dir., forecast = FALSE, verbose = F, printstats = F, forefile = "Forecast-report.sso", covar = F)

  message("Rep file read in")

  SPR <- rep.file$derived_quants %>%
    dplyr::filter(str_detect(Label, "Bratio")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  delta <- as.character(sign(SPR - 0.3))    #-1 if it ratio is smaller than .3, 0 if ratio is bigger
  fcast. <- SS_readforecast(paste0(dir., "/forecast.ss"), Nfleets = nfleet, Nareas = 1, version = "3.24")
  fcast_spr <- fcast.$SPRtarget
  spr.seq <- switch(
    delta,
    "1" = seq(fcast_spr, fcast_spr - .25, by = -.01),
    "-1" = seq(fcast_spr, fcast_spr + .25, by = .01),
    "0" = NA
  )



  if(length(spr.seq)==0){
    print("Already at spr30")
  }else{
  for (i in spr.seq) {
    print(i)
    fcast. <- SS_readforecast(paste0(dir., "/forecast.ss"), Nfleets = nfleet, Nareas = 1, version = "3.24")
    fcast.$SPRtarget <- i
    MO_writeforecast(fcast., dir = dir., overwrite = T)

    tryCatch(if(isTRUE(lin)){
      system(paste("cd", dir., "&& ./SS3 > /dev/null 2>&1", sep = " "))
      }else{
        shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))},
             warning = function(c) {
               starter <- SS_readstarter(paste0(dir., "/starter.ss"))
               starter$init_values_src <- 1
               SS_writestarter(starter,dir=dir.,file="starter.ss",overwrite=T)
               if(isTRUE(lin)){
                 system(paste("cd", dir., "&& ./SS3 > /dev/null 2>&1", sep = " "))
               }else{
                   shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))}
               },
             error = function(e) {
               starter <- SS_readstarter(paste0(dir., "/starter.ss"))
               starter$init_values_src <- 1
               SS_writestarter(starter,dir=dir.,file="starter.ss",overwrite=T)
               if(isTRUE(lin)){
                 system(paste("cd", dir., "&& ./SS3 > /dev/null 2>&1", sep = " "))
                 }else{
                   shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))}})

    out <- tryCatch(

      expr = {
        rep.file <- MO_SSoutput(dir.)
        assign("error", FALSE, envir = globalenv())
      },

      error = function(e){
        assign("error", TRUE, envir = globalenv())
        return(error)
      }

    )

    if(isTRUE(out)){
      run_ss(dir., lin)
      rep.file <- MO_SSoutput(dir.)
    }

    SPR <- rep.file$derived_quants %>%
      dplyr::filter(str_detect(Label, "Bratio")) %>%
      slice(tail(row_number(), 10)) %>%
      summarise(mean(Value)) %>%
      pull()

    if(notifications == T){
      pbPost(
        "note",
        title = "SS run",
        body = paste(
          "The SPR value is",
          round(SPR, 3),
          "when the forecast SPR value was set to",
          i
        )
      )
    }

    if (SPR >= 0.299 & SPR <= 0.31) {

      break
      if(notifications == T){
        pbPost("note",
               title = "SS run",
               body = paste("The new SPR value is", i, "."))
      }
    }

  } }

}


#' Get reference point values after assessment run
#'
#' @param rep. report file from SS assessment
#' @param dat.list contains sequence of years in simulation
#' @param year current year in simulation
#' @import dplyr stringr
#' @importFrom magrittr %>%
#' @export
#'

getRP <- function(rep., dat.list, year){

  year.seq <- dat.list$year_seq
  rp.df <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 9))
  colnames(rp.df) <- c("SSB0",
                       "F_cur",
                       "Fspr30",
                       "F_ratio",
                       "SSB_equ",
                       "SSB_cur",
                       "spr30",
                       "bratio_cur",
                       'status_cur')

  rp.df$SSB0 <- rep.$timeseries %>%
    slice(1) %>%
    select(SpawnBio) %>%
    pull()

  #End year F
  rp.df$F_cur <- rep.$derived_quants %>%
    dplyr::filter(str_detect(Label, "F_")) %>%
    dplyr::filter(str_detect(Label, paste(floor(year.seq[year])))) %>%
    select(Value) %>%
    pull()

  #average of F from terminal 10 years of forecast
  equ_SPR <- rep.$derived_quants %>%
    dplyr::filter(str_detect(Label, "Bratio")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  #F at SPR 30%
  rp.df$Fspr30 <- rep.$derived_quants %>%
    dplyr::filter(str_detect(Label, "F_")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  rp.df$F_ratio <- rp.df$F_cur/rp.df$Fspr30

  rp.df$SSB_equ <- rep.$derived_quants %>%
    dplyr::filter(str_detect(Label, "SSB_20")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  rp.df$SSB_cur <-  rep.$derived_quants %>%
    dplyr::filter(str_detect(Label, paste0("SSB_", floor(year.seq[year])))) %>%
    pull(Value)

  MSST <- (1-.25)*rp.df$SSB_equ

  #if achieved spr 30%
  rp.df$spr30 <- equ_SPR
  #current bratio
  rp.df$bratio_cur <- rp.df$SSB_cur/rp.df$SSB_equ
  #current stock status compared to MSST
  rp.df$status_cur <- rp.df$SSB_cur/MSST


  return(rp.df)


}
