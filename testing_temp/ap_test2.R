
#==============================================================================#
# Test the functionality of ap_unstructured()
#   using MAP data 
#==============================================================================#
rm(list = ls())

setwd("C:\\Users\\tyler.matta\\Dropbox\\AP_design")
require(Matrix)

source("apdesign\\R\\apdesign.R")
#------------------------------------------------------------------------------#
# Subset data for two subjects
#------------------------------------------------------------------------------#
load("testing_temp\\horry_math.Rda")
str(horry_math)

dat <- horry_math[, c(1, 3,4,5,8)]
colnames(dat) <- c('cohort', 'id', 'grade', 'term', 'score') 

dat <- dat[order(dat$id, dat$time),] 
dat <- dat[which(dat$cohort == 9), ]

#dat <- dat[which(dat$id == 559525 | dat$id == 658406), ]

#------------------------------------------------------------------------------#
# Subset data for two subjects
#------------------------------------------------------------------------------#
dat$time <- ifelse(dat$grade == 4 & dat$term == "Fall", 0.2, 
             ifelse(dat$grade == 4 & dat$term == "Spring", 0.7, 
              ifelse(dat$grade == 5 & dat$term == "Fall", 0.3, 
               ifelse(dat$grade == 5 & dat$term == "Spring", 0.7, 
                ifelse(dat$grade == 6 & dat$term == "Fall", 0.2, 
                 ifelse(dat$grade == 6 & dat$term == "Spring", 0.8, 
                  ifelse(dat$grade == 7 & dat$term == "Fall", 0.25, 
                   ifelse(dat$grade == 7 & dat$term == "Spring", 0.75, 
                    ifelse(dat$grade == 8 & dat$term == "Fall", 0.3, 
                     ifelse(dat$grade == 8 & dat$term == "Spring", 0.67, 
                      ifelse(dat$grade == 9 & dat$term == "Fall", 0.25, 
                       ifelse(dat$grade == 9 & dat$term == "Spring", 0.75, 
                         NA))))))))))))

dat$total_time <- ifelse(dat$grade == 4 & dat$term == "Fall", 0.2, 
                   ifelse(dat$grade == 4 & dat$term == "Spring", 0.7, 
                    ifelse(dat$grade == 5 & dat$term == "Fall", 1.3, 
                     ifelse(dat$grade == 5 & dat$term == "Spring", 1.7, 
                      ifelse(dat$grade == 6 & dat$term == "Fall", 2.2, 
                       ifelse(dat$grade == 6 & dat$term == "Spring", 2.8, 
                        ifelse(dat$grade == 7 & dat$term == "Fall", 3.25, 
                         ifelse(dat$grade == 7 & dat$term == "Spring", 3.75, 
                          ifelse(dat$grade == 8 & dat$term == "Fall", 4.3, 
                           ifelse(dat$grade == 8 & dat$term == "Spring", 4.67, 
                            ifelse(dat$grade == 9 & dat$term == "Fall", 5.25, 
                             ifelse(dat$grade == 9 & dat$term == "Spring", 5.75, 
                               NA))))))))))))

dat$cycle <- ifelse(dat$grade == 4, 1, 
                   ifelse(dat$grade == 5, 2, 
                    ifelse(dat$grade == 6, 3, 
                     ifelse(dat$grade == 7, 4, 
                      ifelse(dat$grade == 8, 5, 
                       ifelse(dat$grade == 9, 6,  
                        NA))))))

# Split data by subject
dat_split <- split(dat, dat["id"])


# Apply subjects- specific matrix
# Check 12, 58
apdesign_i(data = dat_split[[58]], 
                  time_var   = "time", 
                  cycle_var  = "cycle",
                  center_cycle = 6,
                  center_time  = 3,                      
                  max_degree = c(2,1),
                  matricies = TRUE)

dat_ap <- apdesign(data = dat, 
                   id_var = "id", 
                   time_var = "time", 
                   cycle_var = "cycle", 
                   center_cycle = 6,
                   center_time  = 3,                             
                   max_degree = c(2,1))


#------------------------------------------------------------------------------#
# Regression for single individual
#------------------------------------------------------------------------------#
df2 <- apdesign_i(data = dat_split[[58]], 
                  time_var   = "time", 
                  cycle_var  = "cycle",
                  center_cycle = 6,
                  center_time  = 3,                                 
                  max_degree = c(2,1))

m1 <- lm(score ~ -1 + a0 + a1 + a2 + b0 + b1, data = df2)

# Compute predicted y
betas <- coef(m1)
ap_matrix <- as.matrix(df2[, c("a0", "a1", "a2", "b0", "b1")])
marg_y <- ap_matrix %*% betas

# Plot data and predicted line
plot(y=df2$score, x=df2$total_time)
lines(y = marg_y, x = df2$total_time)