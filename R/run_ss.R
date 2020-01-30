#' This runs SS and jitters if the model does not converge.
#' @param dir. the directory of where to run ss from
#' @keywords SS
#' @export
#'

run_ss <- function(dir.){

tryCatch(
  expr = {
    shell(paste("cd/d", dir., "&& ss3", sep = " "))

  },
  warning = {
    print("There was a warning")
  },
  error = {
    print("There was an error")
    error <- TRUE
    err_count <- err_count + 1
  },
  finally = {
    if(error == TRUE){
      converge.params <- check_convergence(dir.)
      jit_for_converg(converge.params, dir.)
    }
  }
)
}
