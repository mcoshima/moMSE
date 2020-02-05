#' Check for SS convergence
#'
#' @param dir. file path for assessment files
#' @import dplyr r4ss
#' @keywords convergence
#' @return a vector of values: the number of parameters, the objective function value, and the max gradient component
#' @export
#'
check_convergence <- function(dir.){

  par.file <- paste0(dir., "/ss3.par")
  convCheck <- readLines(par.file,n=1)
  convCheck <- strsplit(convCheck, split = "Maximum gradient component = ")[[1]][2]

  # if(length(convCheck) > 3){
  #   convCheck <- gsub("-", "e-", convCheck)
  #   convCheck[3] <- paste0(convCheck[3], convCheck[4])
  #   convCheck[4] <- NULL
  # }
  # convCheck is now a vector of three values:
  # 1. Number of parameters
  # 2. Objective function value
  # 3. Maximum gradient component

  return(convCheck)

}

#' Run jitter analysis if model didn't converge
#'
#' @param convCheck vector of convergence values (from check_convergence)
#' @param dir. file path for assessment files
#' @param lin if TRUE running on a linux system, default is FALSE
#' @import r4ss
#' @keywords convergence, jitter
#' @return a logical term (T or F), T if the model converged from the jitter, F if the model didn't converge within 3 tries
#' @export
#'
jit_for_converg <- function(convCheck, dir., lin = FALSE){

  .CONV_CRITERIA         <- 0.1
  .MAX_ITERATIONS        <- 3
  .JITTER                <- 1e-5
  .JITTER_MULTIPLIER     <- 10

  iteration <- 1
  jit <- .JITTER
  while(as.numeric(convCheck) > .CONV_CRITERIA & iteration < .MAX_ITERATIONS){
    # Redo the assessment with jitter if gradient is less than the convergence criterion
    iteration <- iteration + 1
    print("Redoing assessment due to non-convergence")
    starter <- SS_readstarter(paste0(dir., "/starter.ss"))
    starter$jitter_fraction <- jit
    starter$init_values_src <- 1
    SS_writestarter(starter,dir=dir.,file="starter.ss",overwrite=T)
    if(isTRUE(lin)){
      system(paste("cd", dir., "&& SS3 > /dev/null 2>&1", sep = " "))
    }else{
        shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))}

    convCheck <- check_convergence(dir.)

    jit <- jit * .JITTER_MULTIPLIER
  }
  if(convCheck < .CONV_CRITERIA){
    converged <- T
  }else{
    converged <- F
  }

  return(converged)

}

