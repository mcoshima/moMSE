#' This runs SS and returns TRUE if there was an error that prevented the model from converging.
#' @param dir. the directory of where to run ss from
#' @param lin if TRUE, running on linux system, default is FALSE
#' @keywords SS
#' @export
#'

run_ss <- function(dir., lin = FALSE){
  out <- tryCatch(
    expr = {

      message("Running SS now")
      if(isTRUE(lin)){
        system("cd ../one_plus ss3 > /dev/null 2>&1")
      }else{shell(paste("cd/d", dir., "&& ss3 >NUL 2>&1", sep = " "))}

      assign("error", FALSE, envir = globalenv())

    },
    warning = function(w){
      print(paste("There was a warning", w))
      assign("error", TRUE, envir = globalenv())
      return(error)
    },
    error = function(e){

      print(paste("The model didn't converge", e))
      assign("error", TRUE, envir = globalenv())
      return(error)
    },
    finally = {
      return(error)
    }

  )
  return(out)
}


#' This runs a jitter for ss if the model does not converge. Returns error = TRUE if it still doesn't converge.
#' @param dir. the directory of where to run ss from
#' @keywords SS
#' @export
#'
try_jit <- function(dir.){

  out <- tryCatch(
    expr = {

      message("Running jitter")
      converge.params <- check_convergence(dir.)
      jit_for_converg(converge.params, dir.)
      assign("error", FALSE, envir = globalenv())

    },
    warning = function(w){
      print(paste("There was a warning", w))
      assign("error", TRUE, envir = globalenv())
      return(error)
    },
    error = function(e){

      print(paste("The model didn't converge", e))
      assign("error", TRUE, envir = globalenv())
      return(error)
    },
    finally = {
      return(error)
    }

  )
  return(out)
}
