
#' General use functions for data generation and manipulation
#'
#' This is a generic function to split and recombine dataframes based on common attributes.
#' I use this for spliting sections of the data files from SS by fleet, adding in the new data, and then recombining everything into the original format.
#' @param df1 first dataframe you want to split
#' @param df2 second dataframe you want to split
#' @param ind column name of index attribute you want to group and split dataframes by
#' @param N number of individual indices
#' @keywords split, recombine
#' @export
#'

splt.recombine <- function(df1, df2, ind, N){

  names <- df2 %>% select_(ind) %>% unique() %>% pull() %>% as.character()

  split.dat <- df1 %>% group_by_(ind) %>% group_split() %>% setNames(names)

  split.new <- df2 %>% group_by_(ind) %>% group_split() %>% setNames(names)

  for(i in 1:N){

    x <- which(names(split.dat) == names(split.dat)[i])

    split.dat[[x]] <- rbind(as.data.frame(split.dat[x]),as.data.frame(split.new[i]))
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
#' @keywords random, continuous
#' @export
#'

rand_vect_cont <- function(N, M, sd = 1) {
  vec <- rlnorm(N, M/N, sd)
  vec / sum(vec) * M
}

#' Extract multiple numbers from a character string
#'
#' @param string the character string containing numbers to extract
#' @keywords numbers, character string
#' @export
#'
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("\\-*[[:digit:]]+\\.*[[:digit:]]*",string)))
}
