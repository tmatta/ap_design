
#==============================================================================#
# Test the functionality of apdesign()
#   using three_season_cycle.Rdata 
#==============================================================================#
rm(list = ls())

setwd("Dropbox/AP_design")

options("scipen"=100, "digits"=4)
require(Matrix)

#--- Load functions
source("apdesign/R/fun_apdesign.R")
source("apdesign/R/fun_apdesign_i.R")

#--- Load data
load("apdesign/data/three_season_cycle.Rdata")
str(three_season_cycle)
length(unique(three_season_cycle$id))
head(three_season_cycle, 25)


# Split data by subject
dat_split <- split(three_season_cycle, three_season_cycle["id"])

#------------------------------------------------------------------------------#
# Regression for single individual
#------------------------------------------------------------------------------#
# Use 15, 16, 19
id_i <- 15
dat_split[[id_i]]

id <- apdesign_i(data = dat_split[[id_i]], 
                  time_var   = "week", 
                  cycle_var  = "cycle",
                  center_cycle = 0,
                  center_time  = 0,                      
                  max_degree = c(1,1),
                  matricies = FALSE)

id$t_week <- ifelse(id$cycle == 2, id$week + 52,
                ifelse(id$cycle == 3, id$week + 104, id2$week))
id$t_week2 <- id$t_week*id$t_week

m_ap_id <- lm(score ~ -1 + a0 + a1 + b0 + b1, data = id)
m_lm_id <- lm(score ~ 1 + t_week + t_week2, data = id)

# Compute predicted y
b_ap <- coef(m_ap_id)
x_ap <- as.matrix(id[, c("a0", "a1", "b0", "b1")])
yhat_ap <- x_ap %*% b_ap

# Compute predicted y
b_lm <- coef(m_lm_id)
x_lm <- as.matrix(cbind(1, id[, c("t_week", "t_week2")]))
yhat_lm <- x_lm %*% b_lm

# Plot data and predicted line
plot(y=id$score, x=id$t_week, ylim = c(25, 35))
lines(y=id$score, x=id$t_week)
lines(y = yhat_ap, x = id$t_week, col = "red")
lines(y = yhat_lm, x = id$t_week, col = "blue")


#------------------------------------------------------------------------------#
# Mixed effect regression for all individuals
#------------------------------------------------------------------------------#
dat_ap <- apdesign(data = three_season_cycle, 
                   id_var = "id", 
                   time_var = "week", 
                   cycle_var = "cycle", 
                   center_cycle = 0,
                   center_time  = 0,                             
                   max_degree = c(1,1))


dat_ap$t_week <- ifelse(dat_ap$cycle == 2, dat_ap$week + 52,
                ifelse(dat_ap$cycle == 3, dat_ap$week + 104, dat_ap$week))

dat_ap$t_week2 <- dat_ap$t_week * dat_ap$t_week

