#' This takes simulated data from the operating model pop dynamics and adds it in the correct format to the .dat file for SS.
#'
#' @param year last year in sequence of new data
#' @param .dat data file that needs to be updated
#' @param agecomp.list simulated age comps from catch
#' @param I simulated indices of abundance for each fleet
#' @param .datcatch A dataframe with the catch for each fishing fleet in the correct units (biomass and numbers)
#' @param comp.I index of red snapper abundance, a measure of competition
#' @param dir. directory of where to read and write new files from/to
#' @param write default set to TRUE, if FALSE, new .dat file will not be saved (won't overwrite the old file)
#' @keywords SS data, update
#' @export
#' @examples
#'
dat.update <- function(year, dat., agecomp.list, I, .datcatch, comp.I, dir., write = T){

  year.seq <- seq(2014, 2014+50, by = .5)
  yr <- floor(year.seq[year])
  rows <- seq(year-9,year) #rows from the past 4 years

  #Add catch for past 5 years

  new.catch <-
    .datcatch %>%
    as.data.frame() %>%
    slice(rows)  %>%
    na.omit() %>%
    mutate(
      COMP = rep(0.001,5),
      year = seq(yr-4,yr),
      seas = rep(1,5)) %>%
    rename(CM_E = V1,
           CM_W = V2,
           REC = V3,
           SMP_BYC = V4) %>%
    mutate(SMP_BYC = rep(0.001,5),
           CM_E = ifelse(CM_E > 0, CM_E, 0.001),
           CM_W = ifelse(CM_W > 0, CM_W, 0.001),
           REC = ifelse(REC > 0, REC, 0.001))


  dat.$catch <- rbind(dat.$catch, new.catch)

  dat.$catch <- dat.$catch %>% distinct(year, .keep_all = T)

  dat.$N_catch <- nrow(dat.$catch)

  #Add Shrimp bycatch as discard
  new.discard <-
    catch.fleet.year %>%
    as.data.frame() %>%
    slice(rows) %>%
    select(4) %>%
    na.omit() %>%
    mutate(
      Yr = seq(yr-4,yr),
      Seas = rep(1,5),
      Flt = rep(-4,5),
      Discard = V4,
      Std_in = rep(0.5,5)
    ) %>%
    select(-1)

  dat.$discard_data <- rbind(dat.$discard_data, new.discard)

  dat.$discard_data <- dat.$discard_data %>% distinct(Yr, .keep_all = T)

  old.tail <-which(dat.$discard_data$Seas < 1)[2]

  dat.$discard_data$Seas[old.tail] <- 1

  dat.$N_discard <- nrow(dat.$discard_data)

  dat.$discard_data$Seas[dat.$N_discard] <- -1


  #Add CPUE

  comp.index <- comp.I %>%
    filter(Year > yr - 5 & Year <= yr) %>%
    select(-Year) %>%
    rename("obs" = RS_relative) %>%
    as.data.frame()


  new.index <-  I %>%
    as.data.frame() %>%
    slice(rows)  %>%
    na.omit() %>%
    bind_cols(comp.index) %>%
    rename("7" = V1,
           "8" = V2,
           "3" = V3,
           "4" = V4,
           "11" = V5,
           "12" = V6,
           "5" = obs) %>%
    melt() %>%
    mutate(
      year = rep(seq(yr-4,yr),7),
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

  new.cpue <- split.recombine(dat.$CPUE, new.index, 3, N = length(unique(new.index$index)))

  dat.$CPUE <- new.cpue[which(new.cpue$obs > 0),]

  dat.$CPUE <- dat.$CPUE %>%
    group_by(index) %>%
    distinct(year, .keep_all = T) %>%
    as.data.frame()

  dat.$N_cpue <- nrow(dat.$CPUE)

  #Add age comps
  if(!is_empty(agecomp.list)){

    sub.acomps <- compact(agecomp.list[rows])
    new.acomp <- do.call(rbind, sub.acomps)
    agecomp <- split.recombine(dat.$agecomp, new.acomp, 3, N=3)

    dat.$agecomp <- agecomp %>%
      group_by(FltSvy) %>%
      distinct(Yr, .keep_all = T) %>%
      as.data.frame()

    dat.$N_agecomp <- nrow(dat.$agecomp)
  }

  dat.$endyr <- yr

  if(write == T){
    SS_writedat(dat., outfile = paste0(dir.,"/VS.dat"), version = "3.24", overwrite = T)

    ct. <- readLines(paste0(dir.,"/VS.ctl"),-1)
    ct.[83] <- paste(yr, "# last year of main recr_devs; forecast devs start in following year", sep = " ")
    writeLines(ct., paste0(dir., "/VS.ctl"))
  }

}
