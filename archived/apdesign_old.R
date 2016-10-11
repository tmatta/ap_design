#' Addative polynomial design matrix.
#'
#' \code{apdesign} returns a data frame with an integrated design matrix.
#'
#' Produces a reparameterized design matrix required to fit an 
#' addative polynomial for data subject to seasonality in R. The resulting 
#' design matrix can be merged into a dataframe for direct use, or can be 
#' printed as a general matrix.
#'
#' @param data A data frame.
#' @param timevar A string contiaing the name of the time variable in the data 
#' frame to be expressed as \code{data$timevar} or \code{data[, "timevar"].
#' @param within An integer specifying the number of measures within a season.    
#' @param c_cycle An integer specifying the cycle to center on.    
#' @param c_within An integer specifying the within cycle measure to center on.   
#' @param time_vec A vector containing the intervals between each time.
#' @param btwn_maxord A numeric specifying the nth degree polynomial for the 
#' between trend.
#' @param wthin_maxord A numeric specifying the nth degree polynomial for the 
#' within trend.
#' @param get_all_mats A logical scalar to return a list of all matricies as a 
#' list.
#' @param print A Logical scalar to pring matricies to console.    
#' @return If \code{get_all_mats == FALSE} then output will be a data frame. If 
#' \code{get_all_mats == TRUE} then output will be a list of matricies.

apdesign <-
  function(data,                          
           timevar = "t",                 
           within=1,                    
           c_cycle=1,                   
           c_within=1,                  
           time_vec=seq(0, within-1, 1),
           btwn_maxord=1,               
           wthin_maxord=1,              
           get_all_mats=FALSE,          
           print=FALSE){                

  
  t_var <- c(factor(data[,paste(timevar)])) # ordered integer
  j_min <- min(t_var)
  j_max <- max(t_var)/within

  if(round(j_max)!=j_max) stop("timevar is not a multiple of within") 
  if(c_within > length(time_vec) | c_within < 1)  stop("ERROR: c_within is beyond the index of time_vec") 
  if(c_cycle > j_max | c_cycle < j_min) print("WARNING: Matrix is centered outside of the observed data")  
  if(mean(rank(time_vec) == rank(sort(time_vec)))!=1) stop("ERROR: time_var is misspecified")  

  #----------------------------------------------------------------------------#
  # AP Component 1: 
  #----------------------------------------------------------------------------#
  j_vec <- seq(j_min, j_max, by=1) - c_cycle
  Ij <- diag(length(j_vec))
  z_vec <- time_vec

  # vector of length 'within' starting at 0 and increasing by 'z_vec'
  b_mat <- matrix(c(rep(1,within), 
                    rep(1,within)*z_vec-z_vec[c_within]), ncol = 2) 
  b_mat <- kronecker(Ij, b_mat)
  
  #----------------------------------------------------------------------------#
  # AP Component 2: within and between cycle design matrices 
  #----------------------------------------------------------------------------#
  #--- between cycle design matrix ---#
  l_mat <- matrix(NA, j_max, btwn_maxord+1)

  for (i in 0:btwn_maxord){
    l_mat[,i+1] <- j_vec^i
  }

  #--- within cycle design matrix ---#
  k_mat <- matrix(NA, j_max, wthin_maxord+1)

  for (i in 0:wthin_maxord){
    k_mat[,i+1] <- j_vec^i
  }

  #----------------------------------------------------------------------------#
  # combined within and between cycles.
  #----------------------------------------------------------------------------#
  v_mat <- matrix(c(1, 0, 
                    0, 1), ncol = 2)

  x_mat <- cbind(kronecker(l_mat, v_mat[1,]), kronecker(k_mat, v_mat[2,]))

  #----------------------------------------------------------------------------#
  # AP design.
  #----------------------------------------------------------------------------#
  ap_mat <- b_mat %*% x_mat  

  #----------------------------------------------------------------------------#
  # column labels
  #----------------------------------------------------------------------------#
  c_labs1 <- NULL
  for (i in 1:(btwn_maxord+1)){
    c_labs1[i] <- paste("b", i-1, sep="_")
  }

  c_labs2 <- NULL
  for (i in 1:(wthin_maxord+1)){
    c_labs2[i] <- paste("w", i-1, sep="_")
  }

  colnames(ap_mat) <- c(c_labs1, c_labs2)
  ap_mat_df <- as.data.frame(ap_mat)

  # to merge based on measure number (t). 
  ap_mat_df[,paste(timevar)] <- row.names(ap_mat_df) 

  ap_mat_df <- merge(data, ap_mat_df, by=paste(timevar)) 

  cat("AP design matrix:", j_max, "cycles centered on cycle", c_cycle, "\n")
  cat("                 ", within, "measures per cycle centered on measure", c_cycle, "\n")

  if(print==TRUE){
    print("Matrix X"); print(x_mat)
    print("Matrix B"); print(b_mat)
    print("AP Design Matrix"); print(ap_mat)
  }

  if(get_all_mats==FALSE){
    return(ap_mat_df)
  }
  if(get_all_mats==TRUE){
    ap_list <- list(ap_mat_df, ap_mat, b_mat, x_mat)
    return(ap_list)
  }  
}
