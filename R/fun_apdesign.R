#==============================================================================#
#' apdesign: AP coding
#' \code{apdesign} returns a data frame with additive polynomial coding
#' 
#' @param id_var A character that indicates the subject identifier in 
#' \code{data}.
#' @inheritParams apdesign_i
#' 
#' @return Output will be a data frame.
#' 
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