
#==============================================================================#
#' apdesign_i: AP coding for a single subject
#' 
#' @param data A data frame.
#' @param time_var A character that indicates the within-cycle time indicator in 
#' \code{data}.
#' @param cycle_var A character that indicates the cycle indicator in 
#' \code{data}.  
#' @param center_time A numeric specifying the within-cycle time to center on.
#' @param center_cycle A numeric specifying the cycle to center on.
#' @param max_degree A vector of numerics specifying the highest degree for 
#' each polynomial.
#' @param matricies If \code{TRUE}, will print the AP, D1 and D2 matricies.
#'  
#' @return Output will be a matrix.
#' 
#' @export
#==============================================================================#
#--- BEGIN apdesign_i ---------------------------------------------------------#
apdesign_i <- function(data, 
                       cycle_var,  
                       center_cycle, 
                       time_var, 
                       center_time, 
                       max_degree = c(1,1),
                       matricies = FALSE) {
  
  cycle <- data[, paste(cycle_var)] 
  time <- data[, paste(time_var)]

  if (any(cycle %% 1 != 0) | is.character(cycle) | is.factor(cycle)){
    stop("Elements of cycle_var must be integers", call. = FALSE)
  }

  if (!is.numeric(time)){
    stop("Elements of time_var must be numeric", call. = FALSE)
  }

#------------------------------------------------------------------------------#
#--- Requirements for AP design -----------------------------------------------#
  
  # How many measures in each cycle
  within_lengths <- tapply(time, cycle, length)

  # How many cycles
  number_of_cycles <- length(within_lengths)

  # List the cycles
  cycle_vec <- as.numeric(names(within_lengths))  

  within_time <- split(time, cycle) 

  total_length <- sum(within_lengths)

#------------------------------------------------------------------------------#
#--- Build D1 Matrix ----------------------------------------------------------#

  # Function that builds a within-cycle design matrix
  build_within_cycle_matrix <- function(cycle){
    cycle_matrix <- matrix(NA, ncol = (length(max_degree)), 
                               nrow = within_lengths[[cycle]])  
    
    cycle_degrees <- seq(1:length(max_degree)) - 1

    for (p in 1 : length(cycle_degrees)){
          cycle_matrix[, p] <- (within_time[[cycle]] - center_time)^(cycle_degrees[p])
    }

    return(cycle_matrix)
  }


  # Initialize list to store all within cycle design matrices
  list_of_design_matricies <- list()

  # Create a within-cycle design matrix for each cycle and store them in a list
  for (i in 1:number_of_cycles) {
    matrix_i <- build_within_cycle_matrix(cycle = i)
    list_of_design_matricies[[i]] <- matrix_i
  }

  # Transform the list of matrices into a single block diagonal matrix
  d1_mat_i <- as.matrix(Matrix::bdiag(list_of_design_matricies))

#------------------------------------------------------------------------------#
#--- Build D2 Matrix ----------------------------------------------------------#

  # Function builds a matrix to describe the change in the linear component across cycles
  component_matrix <- function(degree){ 
    d2_mat_component_k <- matrix(NA, nrow = number_of_cycles, ncol = (degree+1))
      seq_k <- (cycle_vec - 1) - (center_cycle - 1)
    for (j in 0 : degree){
      d2_mat_component_k[,j+1] <- seq_k^j 
    }
    return(d2_mat_component_k)
  }

  list_of_component_matricies <- list()
  v_mat <- diag(length(max_degree))

  for (i in 1 : length(max_degree)){
    list_of_component_matricies[[i]] <- kronecker(component_matrix(max_degree[i]), v_mat[i,])
  }

  d2_mat_i <- do.call(cbind, list_of_component_matricies)
  
  degrees <- list()
  component_letters <- list()

  for (i in 1:length(max_degree)){
    degrees[[i]] <- seq(from = 0, to = max_degree[i], by = 1) 
    component_letters[[i]] <- rep(letters[i], (max_degree[i]+1))
  }

  ap_col_names <- paste0(unlist(component_letters), unlist(degrees))

#------------------------------------------------------------------------------#
#--- Build ap Matrix ----------------------------------------------------------#
  ap_mat_i <- d1_mat_i %*% d2_mat_i  
  colnames(ap_mat_i) <- ap_col_names

  ap_df <- data.frame(ap_mat_i)
  ap_df <- cbind(data, ap_df)  
  
  if (matricies == FALSE){
    return(ap_df)
  } 
  if (matricies == TRUE){
    return(list(AP = ap_mat_i, D1 = d1_mat_i, D2 = d2_mat_i))
  } 

}
#--- END apdesign -------------------------------------------------------------#
#==============================================================================#

