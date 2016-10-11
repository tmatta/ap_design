
#==============================================================================#
# Test the functionality of ap_unstructured_i() and ap_unstructured()
#   using mock data 
#==============================================================================#

rm(list = ls())

setwd("C:\\Users\\tyler.matta\\Dropbox\\AP_design")
require(Matrix)

source("apdesign\\R\\apdesign.R")

#==== TEST 1 ==================================================================#
#------------------------------------------------------------------------------# 
# Build hypothetical data 
#------------------------------------------------------------------------------#
# Two subjects
id <- c(rep(1,10), rep(2, 10))

# Outcome measures
y <- c(c(10, 15, 21, 20, 23, 25, 27, 25, 28, 29), 
       c(12, 16, 18, 20, 20, 22, 28, 27, 29, 31))

# Unstructured within-cycke time
time <- c(c(0.2, 0.5, 0.7), c(0.3, 0.6, 0.75, 0.89), c(0.1, 0.3, 0.8), 
          c(0.3, 0.6, 0.7, 0.85), c(0.2, 0.7, 0.79), c(0.2, 0.5, 0.75))


total_time <- c(c(0.2, 0.5, 0.7), c(1.3, 1.6, 1.75, 1.89), c(2.1, 2.3, 2.8), 
                 c(0.3, 0.6, 0.7, 0.85), c(1.2, 1.7, 1.79), c(2.2, 2.5, 2.75))
 
#time <- c(c(2, 5, 7), c(3, 6, 7, 8), c(1, 3, 8), 
#          c(3, 6, 7, 8), c(2, 7, 8), c(2, 5, 7))


# Cycle number 
cycle <- c(rep(1,3), rep(2, 4), rep(3, 3),
           rep(1,4), rep(2, 3), rep(3, 3))

df <- data.frame(id, y, total_time, time, cycle)

# Split data by subject
df_split <- split(df, id)

#------------------------------------------------------------------------------#
# This section tests the function ap_unstructured_i()
#   ap_unstructured_i() is used inside ap_unstructured()
#------------------------------------------------------------------------------#

# Apply function for both subjects
apdesign_i(data         = df_split[[1]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1))

apdesign_i(data         = df_split[[2]], 
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0,                   
           max_degree   = c(2,1))


#------------------------------------------------------------------------------#
# This section tests the function ap_unstructured_i()
#------------------------------------------------------------------------------#
ap_design(data = df, 
          id_var = "id", 
          time_var = "time", 
          cycle_var = "cycle", 
          center_cycle = 1,
          center_time  = 0,                          
          max_degree = c(2,1))

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



#===  Test 2 ==================================================================#


#------------------------------------------------------------------------------#
# Generic subject-specific time- structuted design matrix 
#------------------------------------------------------------------------------#

t <- rep(seq(1:2), 4) - 1
yr <- rep(seq(1:4), each=2) 

ttt <- apdesign_i(data         = data.frame(t, yr), 
                  time_var     = "t", 
                  cycle_var    = "yr",
                  center_cycle = 7,
                  center_time  = 2,                                 
                  max_degree   = c(2,1),
                  matricies = TRUE)

as.matrix(ttt)