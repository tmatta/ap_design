#==============================================================================#
# Test the functionality of apdesign()
#   using MAP data 
#==============================================================================#

setwd("C:\\Users\\tyler.matta\\Dropbox\\AP_design")
source("R\\apdesign.R")
#------------------------------------------------------------------------------#
# Data
#------------------------------------------------------------------------------#
load("horry_math.Rda")
str(horry_math)
table(horry_math$Test)
table(horry_math$Term)

dat <- horry_math[, c(1, 3,4,5,8)]
colnames(dat) <- c('cohort', 'id', 'grade', 'term', 'score') 
head(dat)

range(dat$grade)
dat$time <- ifelse(dat$grade == 4 & dat$term == "Fall", 1, 
             ifelse(dat$grade == 4 & dat$term == "Spring", 2, 
              ifelse(dat$grade == 5 & dat$term == "Fall", 3, 
               ifelse(dat$grade == 5 & dat$term == "Spring", 4, 
                ifelse(dat$grade == 6 & dat$term == "Fall", 5, 
                 ifelse(dat$grade == 6 & dat$term == "Spring", 6, 
                  ifelse(dat$grade == 7 & dat$term == "Fall", 7, 
                   ifelse(dat$grade == 7 & dat$term == "Spring", 8, 
                    ifelse(dat$grade == 8 & dat$term == "Fall", 9, 
                     ifelse(dat$grade == 8 & dat$term == "Spring", 10, 
                      ifelse(dat$grade == 9 & dat$term == "Fall", 11, 
                       ifelse(dat$grade == 9 & dat$term == "Spring", 12, 
                        NA))))))))))))

dat <- dat[which(dat$cohort == 9), ]

table(dat$time)
head(dat, 15)

#------------------------------------------------------------------------------#
# AP Deign Matrix
#------------------------------------------------------------------------------#

dat_ap <- apdesign(dat,                          
                 timevar = "time",                 
                 within = 3,                    
                 c_cycle = 3,                   
                 c_within = 2,                  
                 btwn_maxord = 2,               
                 wthin_maxord = 1,              
                 get_all_mats = TRUE,          
                 print = TRUE)


dat_ap_df <- dat_ap[[1]]

dat_ap[[2]]
dat_ap_df <- dat_ap_df[order(dat_ap_df$id, dat_ap_df$time),] 

head(dat_ap_df, 15)
#------------------------------------------------------------------------------#
# Fit Model
#------------------------------------------------------------------------------#
library(lme4)

t <- lmer(score ~ -1 + b_0 + b_1 + b_2 + w_0 + w_1 + 
                 (-1 + b_0 + b_1 + w_0 | id), data = dat_ap_df, REML = FALSE)
summary(t)


#------------------------------------------------------------------------------#
# Obtain marginals
#------------------------------------------------------------------------------#

design_mat <- as.matrix(dat_ap[[2]]) 
f_eff <- fixef(t)
vcm <- vcov(t)
marg <- design_mat %*% f_eff
marg_cov <- design_mat %*% vcm %*% t(design_mat)
marge_se <- diag(marg_cov)

marge_hi <- marg + 1.96*marge_se
marge_lo <- marg - 1.96*marge_se

x_ax <- 1:12
plot(NULL, xlim=c(0, 12), ylim=c(200, 240), ylab=" ", xlab=" ", bty="n")
lines(y = marg, x = x_ax)
segments(x0 = x_ax, y0 = marge_lo, 
         x1 = x_ax, y1 = marge_hi)


