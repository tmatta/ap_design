
#==============================================================================#
# Test the functionality of apdesign() with variables spaced MAP data
#==============================================================================#
rm(list = ls())

setwd("C:\\Users\\tyler.matta\\Dropbox\\AP_design")
require(Matrix)

source("apdesign\\R\\apdesign_functions.R")

load("testing_temp\\map_3s.RData")
str(map_3s)

by(map_3s[, "iday"], interaction(map_3s$grd, map_3s$season, drop = T), range)
by(map_3s[, "iweek"], interaction(map_3s$grd, map_3s$season, drop = T), range)

ids <- unique(map_3s$id)

#==============================================================================#
#--- subject specific AP ------------------------------------------------------#
# Subjects 3 and 28 are very good examples 

#--- time unstructured
apdesign_i(data         = map_3s[which(map_3s$id == ids[3]), ], 
           time_var     = "iweek", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0,                                 
           max_degree   = c(2,1),
           matricies    = TRUE)

#--- time structured
map_3s$season_num <- (as.numeric(map_3s$season) - 1)

apdesign_i(data         = map_3s[which(map_3s$id == ids[3]), ], 
           time_var     = "season_num", 
           cycle_var    = "cycle",
           center_cycle = 1,
           center_time  = 0,                                 
           max_degree   = c(2,1),
           matricies    = TRUE)

#==============================================================================#
#--- Data AP ------------------------------------------------------------------#
# This assumes that iday = 0 is the first day of school 
# If iday = 1 is the first dat of school, correct centering and rerun.
df_ap <- apdesign(data         = map_3s, 
                  id_var       = "id",
                  time_var     = "iweek", 
                  cycle_var    = "cycle",
                  center_cycle = 1,
                  center_time  = 0,                                 
                  max_degree   = c(2,1))
str(df_ap)

#--- Fit mixed model to data --------------------------------------------------#
library(lme4)
m1 <- lmer(rit ~ -1 + a0 + a1 + a2 + b0 + b1 + (-1 + a0 + a1 + b0 | id), data = df_ap)

summary(m1)

m1_fixed <- fixef(m1)

t1 <- c(2, 17, 30, 2, 17, 30, 2, 17, 30)
c1 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

design_mat <- apdesign_i(data         = data.frame(t1,c1), 
                         time_var     = "t1", 
                         cycle_var    = "c1",
                         center_cycle = 1,
                         center_time  = 0,                                 
                         max_degree   = c(2,1),
                         matricies = TRUE)

ap_design_mat <- design_mat[[1]]

marg <- ap_design_mat %*% m1_fixed

# Assumes a 10 week summer vacation.  
t2 <- c(2, 17, 30, 2+40, 17+40, 30+40, 3+80, 17+80, 30+80)

plot(y = marg, x = t2, type= "b")





#==============================================================================#


load("apdesign\\data\\three_season_cycle.RData")

str(three_season_cycle)


#--- Check range of times
by(df[, "day"], df[, "term"], range)
by(df[, "week"], df[, "term"], range)


#--- Data AP ------------------------------------------------------------------#
# This assumes that iday = 0 is the first day of school 
# If iday = 1 is the first dat of school, correct centering and rerun.
df_ap <- apdesign(data         = df, 
                  id_var       = "id",
                  time_var     = "week", 
                  cycle_var    = "cycle",
                  center_cycle = 1,
                  center_time  = 0,                                 
                  max_degree   = c(2,1))

str(df_ap)



#--- Fit mixed model to data --------------------------------------------------#
library(lme4)
m1 <- lmer(score ~ -1 + a0 + a1 + a2 + b0 + b1 + (-1 + a0 + a1 + b0 | id), data = df_ap)

summary(m1)

m1_fixed <- fixef(m1)
m1_cov <- vcov(m1)

t1 <- c(2, 17, 30, 2, 17, 30, 2, 17, 30)
c1 <- c(1,1,1,2,2,2,3,3,3)

design_mat <- apdesign_i(data         = data.frame(t1,c1), 
                         time_var     = "t1", 
                         cycle_var    = "c1",
                         center_cycle = 1,
                         center_time  = 0,                                 
                         max_degree   = c(2,1),
                         matricies = TRUE)
ap_design_mat <- design_mat[[1]]
marg <- ap_design_mat %*% m1_fixed

# Assumes a 10 week summer vacation.  
t2 <- c(2, 17, 30, 2+40, 17+40, 30+40, 3+80, 17+80, 30+80)

plot(y = marg, x = t2, type= "b")

