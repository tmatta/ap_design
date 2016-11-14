
#==============================================================================#
# Test the functionality of fun_apdesign() 
#==============================================================================#
require(Matrix)

rm(list = ls())

setwd("Dropbox/AP_design")
source("apdesign/R/fun_apdesign_i.R")
source("apdesign/R/fun_apdesign.R")

#==============================================================================#
#==== BEGIN TEST 2.1 ==========================================================#
#---- Testing max_degree argument

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
cycle <- c(rep(1,3), rep(2, 4), rep(3, 3),
           rep(1,4), rep(2, 3), rep(3, 3))

df <- data.frame(id, y, total_time, time, cycle)


apdesign(data           = df, 
         id             = "id",
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1))

apdesign(data           = df, 
         id             = "id",
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1, 1))

apdesign(data           = df, 
         id             = "id",
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,2,1,1))

#==============================================================================#
#==== BEGIN TEST 2.2 ==========================================================#
#---- Testing cycle_var requirements

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


apdesign(data           = df, 
         id             = "id",
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1))

# + Error: Elements of cycle_var must be integers

#==============================================================================#
#==== BEGIN TEST 2.3 ==========================================================#
#---- Testing time_var requirements

#--- Two subjects
id <- c(rep(1,10), rep(2, 10))

#--- Outcome measures
y <- c(c(10, 15, 21, 20, 23, 25, 27, 25, 28, 29), 
       c(12, 16, 18, 20, 20, 22, 28, 27, 29, 31))

#--- Unstructured within-cycke time
time <- c(c(0.2, 0.5, 0.7), c(0.3, 0.6, 0.75, 0.89), c(0.1, 0.3, 0.8), 
          c(0.3, 0.6, 0.7, 0.85), c(0.2, 0.7, 0.79), c(0.2, 0.5, 0.75))
time <- as.factor(time)
#--- Total time
total_time <- c(c(0.2, 0.5, 0.7), c(1.3, 1.6, 1.75, 1.89), c(2.1, 2.3, 2.8), 
                 c(0.3, 0.6, 0.7, 0.85), c(1.2, 1.7, 1.79), c(2.2, 2.5, 2.75))
 
#--- Cycle number 
cycle <- c(rep(1,3), rep(2, 4), rep(3, 3),
           rep(1,4), rep(2, 3), rep(3, 3))

df <- data.frame(id, y, total_time, time, cycle)


apdesign(data           = df, 
         id             = "id",
           time_var     = "time", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0, 
           max_degree   = c(2,1))

#  + Error: Elements of time_var must be numeric