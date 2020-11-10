
#' General use functions for data generation and manipulation
#'
#' This is a generic function to split and recombine dataframes based on common attributes.
#' I use this for spliting sections of the data files from SS by fleet, adding in the new data, and then recombining everything into the original format.
#' @param df1 first dataframe you want to split
#' @param df2 second dataframe you want to split
#' @param group_var column name of index attribute you want to group and split dataframes by (do not need quotes around the name)
#' @param N number of individual indices
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @keywords split, recombine
#' @export
#'

splt.recombine <- function(df1, df2, group_var, N){

  group_var <- enquo(group_var)
  col <- quo_name(group_var)

  names <- df1 %>%
    select(!!group_var) %>%
    unique() %>%
    pull() %>%
    sort()

  split.dat <- df1 %>%
    group_by(!!group_var) %>%
    group_split() %>%
    setNames(names)

  names.2 <- df2 %>%
    select(!!group_var) %>%
    unique() %>%
    pull() %>%
    sort()

  split.new <- df2 %>%
    #mutate_at(vars(!!col), function(x) x = as.numeric(paste(x))) %>%
    group_by(!!group_var) %>%
    group_split() %>%
    setNames(names.2)

  for(i in 1:N){

    x <- which(names(split.dat) == names(split.new)[i])

    split.dat[[x]] <- rbind((split.dat[x][[1]]),
                            (split.new[i][[1]]))

    colnames(split.dat[[x]]) <- colnames(df1)

  }

  return(do.call(rbind, split.dat))

}


#' This creates a vector of N whole numbers that add up to M.
#'
#' @param N number of values in vector
#' @param M total values should add up to
#' @param sd sd of values, default is 1
#' @param pos.only should the vector contain only positive values, default is TRUE
#' @importFrom stats rnorm
#' @keywords random, discrete
#' @export
#'

rand_intvect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

#' This creates a vector of N numbers that add up to M (can be decimals).
#'
#' @param N number of values in vector
#' @param M total values should add up to
#' @param sd sd of values, default is 1
#' @importFrom stats rlnorm
#' @keywords random, continuous
#' @export


rand_vect_cont <- function(N, M, sd = 1) {
  vec <- rlnorm(N, M/N, sd)
  vec / sum(vec) * M
}

#' Extract multiple numbers from a character string
#'
#' @param string the character string containing numbers to extract
#' @keywords numbers, character string
#' @export

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("\\-*[[:digit:]]+\\.*[[:digit:]]*",string)))
}

#' Copy assessment files to a new folder
#'
#' @param dat.list a list object containing the sequence of years
#' @param dir. current directory for scenario
#' @param new.dir the new destitnation to send files to
#' @keywords assessment files
#' @export


copy_files <- function(dat.list, dir., new.dir){
  current.dir <- dir.
  dir.create(new.dir)
  assess.files <- dat.list$files.to.copy
  file.copy(file.path(current.dir, assess.files), new.dir, overwrite = T)

}
