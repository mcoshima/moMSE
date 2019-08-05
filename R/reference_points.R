#' Find the SPR input value for forecast.ss file to achieve SPR 30\%
#'
#' @param dir. directory of where the original and new forecast file are. Also sends notifications to phone (pbpost) after each run and when 30\% is reached.
#' @keywords SPR
#' @export
#'
#'
#'
find_spr <- function(dir.) {
  rep.file <- SS_output(dir.)

  SSB0 <- rep.file$timeseries %>%
    slice(1) %>%
    select(SpawnBio) %>%
    pull()

  SSB_equ <- rep.file$derived_quants %>%
    filter(str_detect(Label, "SSB")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  delta <-
    as.character(sign(SSB_equ / SSB0 - 0.3))                 #-1 if it ratio is smaller than .3, 0 if ratio is bigger
  fcast. <- readLines(paste0(dir., "/Forecast.ss"), -1)
  fcast_spr <- as.numeric(strsplit(fcast.[7], "#")[[1]][1])
  spr.seq <- switch(
    delta,
    "0" = seq(fcast_spr, fcast_spr - .15, by = -.01),
    "-1" = seq(fcast_spr, fcast_spr + .15, by = .01)
  )

  print(spr.seq)

  msg1 <- "SS run is complete and new report file was read in."

  for (i in spr.seq) {
    print(i)
    fcast. <- readLines(paste0(dir., "/Forecast.ss"), -1)
    fcast.[7] <- paste(i, "# SPR target (e.g. 0.40)", sep = " ")
    writeLines(fcast., paste0(dir., "/Forecast.ss"))

    shell(paste("cd/d", dir., "&& ss3", sep = " "))
    rep.file <- SS_output(dir.)
    pbPost("note",
           title = "SS run",
           body = msg1)

    SSB0 <- rep.file$timeseries %>%
      slice(1) %>%
      select(SpawnBio) %>%
      pull()

    SSB_equ <- rep.file$derived_quants %>%
      filter(str_detect(Label, "SSB")) %>%
      slice(tail(row_number(), 10)) %>%
      summarise(mean(Value)) %>%
      pull()

    pbPost(
      "note",
      title = "SS run",
      body = paste(
        "The SPR value is",
        round(SSB_equ / SSB0, 3),
        "when the forecast SPR value was set to",
        i
      )
    )

    if (SSB_equ / SSB0 > 0.299 & SSB_equ / SSB0 < 0.31) {
      pbPost("note",
             title = "SS run",
             body = paste("The new SPR value is", i, "."))
      break


    }

  }

}


#' Get reference point values after assessment run
#'
#' @param rep. report file from SS assessment
#' @param dat.list contains sequence of years in simulation
#' @param year current year in simulation
#' @export
#'
getRP <- function(rep., dat.list, year){

  year.seq <- dat.list$year_seq
  rp.df <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 10))
  colnames(rp.df) <- c("SSB0",
                       "F_cur",
                       "equ_SPR",
                       "Fspr30",
                       "F_ratio",
                       "SSB_equ",
                       "SSB_cur",
                       "spr30",
                       "bratio",
                       'status_cur')

  rp.df$SSB0 <- rep.$timeseries %>%
    slice(1) %>%
    select(SpawnBio) %>%
    pull()

  #End year F
  rp.df$F_cur <- rep.$derived_quants %>%
    filter(str_detect(Label, "F_")) %>%
    filter(str_detect(Label, paste(floor(year.seq[year])))) %>%
    select(Value) %>%
    pull()

  #average of F from terminal 10 years of forecast
  rp.df$equ_SPR <- rep.$derived_quants %>%
    filter(str_detect(Label, "Bratio")) %>%
    slice(tail(row_number(), 10)) %>%
    summarise(mean(Value)) %>%
    pull()

  #F at SPR 30%
  rp.df$Fspr30 <- rep.$derived_quants %>%
    filter(str_detect(Label, "Fstd_MSY")) %>%
    select(Value) %>%
    pull()

  rp.df$F_ratio <- rp.df$F_cur/rp.df$Fspr30

  rp.df$SSB_equ <- rep.$derived_quants %>%
    filter(str_detect(Label, "SSB_Btgt")) %>%
    select(Value) %>%
    pull()

  rp.df$SSB_cur <-  rep.$derived_quants %>%
    filter(str_detect(Label, paste0("SSB_", floor(year.seq[year])))) %>%
    pull(Value)

  MSST <- (1-.25)*rp.df$SSB_equ

  #if achieved spr 30%
  rp.df$spr30 <- rp.df$SSB_equ/rp.df$SSB0
  #current bratio
  rp.df$bratio <- rp.df$SSB_cur/rp.df$SSB0
  #current stock status compared to MSST
  rp.df$status_cur <- rp.df$SSB_cur/MSST


  return(rp.df)


}
