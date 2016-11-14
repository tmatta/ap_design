
#==============================================================================#
# Test the functionality of fun_apdesign_i() 
#==============================================================================#
require(Matrix)

rm(list = ls())

setwd("Dropbox/AP_design")
source("apdesign/R/fun_apdesign_i.R")

#==============================================================================#
#==== BEGIN TEST 1 ============================================================#
#---- Testing max_degree argument

#--- Two subjects
id <- c(rep(1,10), rep(2, 10))

#--- Outcome measures
y <- c(c(10, 15, 21, 20, 23, 25, 27, 25, 28, 29), 
       c(12, 16, 18, 20, 20, 22, 28, 27, 29, 31))

#--- Unstructured within-cycke time
time <- c(c(0.2, 0.5, 0.7), c(0.3, 0.6, 0.75, 0.89), c(0.1, 0.3, 0.8), 
          c(0.3, 0.6, 0.7, 0.85), c(0.2, 0.7, 0.79), c(0.2, 0.5, 0.75))
time <- factor(time)

#--- Total time
total_time <- c(c(0.2, 0.5, 0.7), c(1.3, 1.6, 1.75, 1.89), c(2.1, 2.3, 2.8), 
                 c(0.3, 0.6, 0.7, 0.85), c(1.2, 1.7, 1.79), c(2.2, 2.5, 2.75))
 
#--- Cycle number 
cycle <- c(rep(1,3), rep(2, 4), rep(3, 3),
           rep(1,4), rep(2, 3), rep(3, 3))

df <- data.frame(id, y, total_time, time, cycle)

# Split data by subject
df_split <- split(df, id)




apdesign_i(data         = df_split[[1]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1),
           matricies    = TRUE)

apdesign_i(data         = df_split[[1]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0,                   
           max_degree   = c(2,1,1),
           matricies    = TRUE)

apdesign_i(data         = df_split[[1]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0,                   
           max_degree   = c(3,2,1, 1),
           matricies    = TRUE)



#------------------------------------------------------------------------------#
# Regression for single individual
#------------------------------------------------------------------------------#
df2 <- apdesign_i(data         = df_split[[1]], 
                  time_var     = "time", 
                  cycle_var    = "cycle",
                  center_cycle = 3,
                  center_time  = .5,                                 
                  max_degree   = c(2,1))

m1 <- lm(y ~ -1 + a0 + a1 + a2 + b0 + b1, data = df2)

# Compute predicted y
betas <- coef(m1)
ap_matrix <- as.matrix(df2[, c("a0", "a1", "a2", "b0", "b1")])
pred_y <- ap_matrix %*% betas

# Plot data and predicted line
plot(y=df2$y, x=df2$total_time)
lines(y = pred_y, x = df2$total_time)


#==============================================================================#
#===  BEGIN TEST 2 ============================================================#
#--- Testing the integer requirement for cycle_var

#--- Two subjects
id <- c(rep(1,10), rep(2, 10))

#--- Outcome measures
y <- c(c(10, 15, 21, 20, 23, 25, 27, 25, 28, 29), 
       c(12, 16, 18, 20, 20, 22, 28, 27, 29, 31))

#--- Unstructured within-cycke time
time <- c(c(0.2, 0.5, 0.7), c(0.3, 0.6, 0.75, 0.89), c(0.1, 0.3, 0.8), 
          c(0.3, 0.6, 0.7, 0.85), c(0.2, 0.7, 0.79), c(0.2, 0.5, 0.75))

#--- Total time
total_time <- c(c(0.2, 0.5, 0.7), c(1.3, 1.6, 1.75, 1.89), c(2.1, 2.3, 2.8), 
                 c(0.3, 0.6, 0.7, 0.85), c(1.2, 1.7, 1.79), c(2.2, 2.5, 2.75))
 
#--- Cycle number 
cycle <- c(rep(1.1,3), rep(2, 4), rep(3, 3),
           rep(1,4), rep(2, 3), rep(3, 3))

df <- data.frame(id, y, total_time, time, cycle)

# Split data by subject
df_split <- split(df, id)


!is.numeric(time)
apdesign_i(data         = df_split[[1]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1),
           matricies    = TRUE)

apdesign_i(data         = df_split[[2]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1),
           matricies    = TRUE)
#------------------------------------------------------------------------------#






#==============================================================================#
# COPIED FROM fun_apdesign_i for testing (11/14/2016) 
# MAY NOT BE UP TO DATE!
#==============================================================================#
#--- Function arguments 
center_time <- 0
center_cycle <- 0
max_degree <- c(2,1)


#--- fun_apdesign_i functions
within_lengths <- tapply(df_split[[1]][, "time"], 
                         df_split[[1]][, "cycle"], length)

number_of_cycles <- length(within_lengths)

cycle_vec <- as.numeric(names(within_lengths))  

within_time <- split(df_split[[1]][, "time"], df_split[[1]][, "cycle"]) 

total_length <- sum(within_lengths)

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

