#' This takes simulated data from the operating model pop dynamics and adds it in the correct format to the .dat file for SS.
#'
#' @param year last year in sequence of new data
#' @param dat.list list that has the sequence of years
#' @param dat. data file that needs to be updated
#' @param agecomp.list simulated age comps from catch
#' @param I simulated indices of abundance for each fleet. Default is NULL and competition index will not be included in assessment.
#' @param .datcatch A dataframe with the catch for each fishing fleet in the correct units (biomass and numbers)
#' @param comp.I index of red snapper abundance, a measure of competition
#' @param dir. directory of where to read and write new files from/to
#' @param write default set to TRUE, if FALSE, new .dat file will not be saved (won't overwrite the old file)
#' @import dplyr purrr tidyr
#' @importFrom reshape2 melt
#' @importFrom r4ss SS_writedat
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @keywords SS data, update
#' @export
#'

dat.update <- function(year, dat.list, dat., agecomp.list, I, .datcatch, comp.I = NULL, dir., write = T){

  year.seq <- as.numeric(dat.list$year_seq)
  yr <- floor(year.seq[year])
  rows <- seq(year-9,year) #rows from the past 4 years
  yrs. <- year.seq[rows]
  yrs. <- yrs.[which((yrs. %% 1) %in% 0)]


  #Add catch for past 5 years

  if(!is.null(comp.I)){
    new.catch <-
      .datcatch %>%
      as.data.frame() %>%
      slice(rows)  %>%
      na.omit() %>%
      mutate(
        year = yrs.,
        seas = rep(1,5)) %>%
      rename(CM_E = V1,
             CM_W = V2,
             REC = V3,
             SMP_BYC = V4,
             COMP = V5) %>%
      mutate(SMP_BYC = rep(0.001,5),
             COMP = rep(0.001,5),
             CM_E = ifelse(CM_E > 0, CM_E, 0.001),
             CM_W = ifelse(CM_W > 0, CM_W, 0.001),
             REC = ifelse(REC > 0, REC, 0.001))

  }
  if(is.null(comp.I)){

    new.catch <-
      .datcatch %>%
      as.data.frame() %>%
      slice(rows)  %>%
      na.omit() %>%
      mutate(
        year = yrs.,
        seas = rep(1,5)) %>%
      rename(CM_E = V1,
             CM_W = V2,
             REC = V3,
             SMP_BYC = V4) %>%
      mutate(SMP_BYC = rep(0.001,5),
             CM_E = ifelse(CM_E > 0, CM_E, 0.001),
             CM_W = ifelse(CM_W > 0, CM_W, 0.001),
             REC = ifelse(REC > 0, REC, 0.001))
  }

  dat.$catch <- rbind(dat.$catch, new.catch)

  dat.$catch <- dat.$catch %>% distinct(year, .keep_all = T)

  dat.$N_catch <- nrow(dat.$catch)

  #Add Shrimp bycatch as discard
  new.discard <-
    .datcatch %>%
    as.data.frame() %>%
    slice(rows) %>%
    select(4) %>%
    na.omit() %>%
    mutate(
      Yr = yrs.,
      Seas = rep(1,5),
      Flt = rep(-4,5),
      Discard = V4,
      Std_in = rep(0.5,5)
    ) %>%
    select(-1)

  if(!is.null(comp.I)){
    comp.discard <-
      .datcatch %>%
      as.data.frame() %>%
      slice(rows) %>%
      select(5) %>%
      na.omit() %>%
      mutate (
        Yr = yrs.,
        Seas = rep(1,5),
        Flt = rep(5,5),
        Discard = V5,
        Std_in = rep(0,5)
      ) %>% select(-1)

    dat.$discard_data[which(dat.$discard_data$Flt == 4),3] <- -4

    flt.4 <- dat.$discard_data[which(dat.$discard_data$Flt == -4),]

    flt.5 <- dat.$discard_data[which(dat.$discard_data$Flt == 5),]

    flt.4 <- rbind(flt.4, new.discard)

    flt.4 <- flt.4 %>% distinct(Yr, Flt, .keep_all = T)

    flt.4 <- flt.4 %>%
      mutate(Seas = ifelse(Yr == 1972 | Yr == 2013 | Yr == 2014 | Yr == yr, -1, 1),
             Flt = ifelse(Yr == 1972 | Yr == 2014, 4, -4))

    flt.5 <- rbind(flt.5, comp.discard)

    dat.$discard_data <- rbind(flt.4, flt.5)

    dat.$N_discard <- nrow(dat.$discard_data)
  }

  if(is.null(comp.I)){
    dat.$discard_data[which(dat.$discard_data$Flt == 4),3] <- -4

    flt.4 <- dat.$discard_data[which(dat.$discard_data$Flt == -4),]

    flt.4 <- rbind(flt.4, new.discard)

    flt.4 <- flt.4 %>% distinct(Yr, Flt, .keep_all = T)

    flt.4 <- flt.4 %>%
      mutate(Seas = ifelse(Yr == 1972 | Yr == 2013 | Yr == 2014 | Yr == yr, -1, 1),
             Flt = ifelse(Yr == 1972 | Yr == 2014, 4, -4))

    dat.$discard_data <- flt.4

    dat.$N_discard <- nrow(dat.$discard_data)
  }


  #Add CPUE

  if(!is.null(comp.I)){
    comp.index <- comp.I %>%
      dplyr::filter(Year > yr - 5 & Year <= yr) %>%
      select(-Year) %>%
      rename("obs" = RS_relative) %>%
      as.data.frame()

    new.index <-  I %>%
      as.data.frame() %>%
      slice(rows)  %>%
      na.omit() %>%
      bind_cols(comp.index) %>%
      rename("8" = V1,
             "9" = V2,
             "3" = V3,
             "4" = V4,
             "11" = V5,
             "12" = V6,
             "5" = obs) %>%
      melt() %>%
      mutate(
        year = rep(yrs.,7),
        seas = rep(1,35),
        variable = as.factor(variable),
        se_log = c(rep(CPUE.se$SE, each = 5), rep(0.01, 5))
      ) %>%
      select(year,
             seas,
             variable,
             value,
             se_log) %>%
      rename(index = variable,
             obs = value)


    new.cpue <- splt.recombine(dat.$CPUE, new.index, 'index', N = length(unique(new.index$index)))

    dat.$CPUE <- new.cpue[which(new.cpue$obs > 0),]

    dat.$CPUE <- dat.$CPUE %>%
      group_by(index) %>%
      distinct(year, .keep_all = T) %>%
      as.data.frame()

    dat.$N_cpue <- nrow(dat.$CPUE)
  }

  if(is.null(comp.I)){
    new.index <-  I %>%
      as.data.frame() %>%
      slice(rows)  %>%
      na.omit() %>%
      rename("7" = V1,
             "8" = V2,
             "3" = V3,
             "4" = V4,
             "10" = V5,
             "11" = V6) %>%
      melt() %>%
      mutate(
        year = rep(yrs.,6),
        seas = rep(1,30),
        variable = as.factor(variable),
        se_log = c(rep(CPUE.se$SE, each = 5))
      ) %>%
      select(year,
             seas,
             variable,
             value,
             se_log) %>%
      rename(index = variable,
             obs = value)

    new.cpue <- splt.recombine(dat.$CPUE, new.index, 'index', N = length(unique(new.index$index)))

    dat.$CPUE <- new.cpue[which(new.cpue$obs > 0),]

    dat.$CPUE <- dat.$CPUE %>%
      group_by(index) %>%
      distinct(year, .keep_all = T) %>%
      as.data.frame()

    dat.$N_cpue <- nrow(dat.$CPUE)
  }

  #Add age comps
  if(!is_empty(agecomp.list)){

    sub.acomps <- compact(agecomp.list[rows])
    new.acomp <- do.call(rbind, sub.acomps)
    agecomp <- splt.recombine(dat.$agecomp, new.acomp, 'FltSvy', N=3)

    dat.$agecomp <- agecomp %>%
      group_by(FltSvy) %>%
      distinct(Yr, .keep_all = T) %>%
      as.data.frame()

    dat.$N_agecomp <- nrow(dat.$agecomp)
  }

  dat.$endyr <- yr

  if(write == T){
    SS_writedat_3.24(dat., outfile = paste0(dir.,"/VS.dat"), overwrite = T)

    ct. <- readLines(paste0(dir.,"/VS.ctl"),-1)
    ct.[83] <- paste(yr, "# last year of main recr_devs; forecast devs start in following year", sep = " ")
    writeLines(ct., paste0(dir., "/VS.ctl"))
    fore <- SS_readforecast(paste0(dir., "/Forecast.ss"), Nfleets = 5, Nareas = 1, version = "3.24")
    fore$Nforecastyrs <- 60
    fore$ForeCatch <- dat.list$full_forecast %>% filter(Year > yr & Year < yr+60)
    fore$Ncatch <- nrow(fore$ForeCatch)
    fore$InputBasis <- 99
    MO_writeforecast(fore, dir = dir., overwrite = T)
  }

}

