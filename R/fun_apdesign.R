#==============================================================================#
#' apdesign: AP coding
#' \code{apdesign} returns a data frame with additive polynomial coding
#' 
#' @param id_var A character that indicates the subject identifier in 
#' \code{data}.
#' @inheritParams apdesign_i
#' 
#' @return Output will be a data frame.
#' @examples
#' id <- c(rep(1,10), rep(2, 10))
#' y <- c(c(10, 15, 21, 20, 23, 25, 27, 25, 28, 29), 
#'      c(12, 16, 18, 20, 20, 22, 28, 27, 29, 31))
#' time <- c(c(0.2, 0.5, 0.7), c(0.3, 0.6, 0.75, 0.89), c(0.1, 0.3, 0.8), 
#'         c(0.3, 0.6, 0.7, 0.85), c(0.2, 0.7, 0.79), c(0.2, 0.5, 0.75))
#' cycle <- c(rep(1, 3), rep(2, 4), rep(3, 3), rep(1, 4), rep(2, 3), rep(3, 3))
#' df <- data.frame(id, y, time, cycle)
#' apdesign(data = df, id = "id", time_var = "time", cycle_var = "cycle", 
#'          center_cycle = 1, center_time = 0, max_degree = c(2,1))
#' @export
#==============================================================================#
#--- BEGIN apdesign -----------------------------------------------------------#
apdesign <- function(data, 
                     id_var, 
                     time_var, 
                     center_time, 
                     cycle_var, 
                     center_cycle, 
                     max_degree = c(1, 1)){

  cycle_test <- data[, paste(cycle_var)] 
  time_test <- data[, paste(time_var)]


  if (any(cycle_test %% 1 != 0) | is.character(cycle_test) | is.factor(cycle_test)){
    stop("Elements of cycle_var must be integers", call. = FALSE)
  }

  if (!is.numeric(time_test)){
    stop("Elements of time_var must be numeric", call. = FALSE)
  }

  df_split <- split(data, as.factor(data[, paste(id_var)]))

  df_ap_split <- lapply(df_split, 
                   function(x) apdesign_i(data         = x, 
                                          time_var     = time_var, 
                                          cycle_var    = cycle_var,
                                          center_time  = center_time,
                                          center_cycle = center_cycle,
                                          max_degree   = max_degree,
                                          matricies    = FALSE)
                        )
    
  df_ap <- do.call(rbind, df_ap_split)
  rownames(df_ap) <- NULL
  return(df_ap)
}
#--- END apdesign -------------------------------------------------------------#
#==============================================================================#