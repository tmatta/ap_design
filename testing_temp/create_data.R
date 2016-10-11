
#==============================================================================#
# Create 3-season data
#==============================================================================#
rm(list = ls())

setwd("C:\\Users\\tyler.matta\\Dropbox\\AP_design")
require(Matrix)

source("apdesign\\R\\apdesign_functions.R")
df <- read.csv(file = "testing_temp\\data\\APTestInstructionalWeeks.csv", stringsAsFactors = FALSE)
str(df)

#--- Operationalize -----------------------------------------------------------#
names(df) <- c("id", "grd", "term", "date", "iday", "rit")
str(df)

length(unique(df$id))
nrow(df)


#=== Subset chort =============================================================#
table(df$term)
table(df$grd, df$term)

#--- Only cohort 1
ids_c1 <- unique(df$id[which(
     df$grd == 3 & df$term == 73 | df$grd == 3 & df$term == 74 | df$grd == 3 & df$term == 75 |
     df$grd == 4 & df$term == 77 | df$grd == 4 & df$term == 78 | df$grd == 4 & df$term == 79 |
     df$grd == 5 & df$term == 81 | df$grd == 5 & df$term == 82 | df$grd == 8 & df$term == 83)])
length(ids_c1)

df_c1 <- df[which(df$id %in% ids_c1), ]
length(unique(df_c1$id))

head(df_c1)
#--- Season variables ---------------------------------------------------------#
df_c1$iweek <- df_c1$iday / 5

df_c1$cycle <- ifelse(df_c1$grd == 3, 1, 
                ifelse(df_c1$grd == 4, 2, 
                 ifelse(df_c1$grd == 5, 3, NA)))
table(df_c1$cycle)


df_c1$season <- ifelse(df_c1$term == 73 | df_c1$term == 77 | df_c1$term == 81, "fall",
                  ifelse(df_c1$term == 74 | df_c1$term == 78 | df_c1$term == 82, "winter",
                   ifelse(df_c1$term == 75 | df_c1$term == 79 | df_c1$term == 83, "spring", NA)))

table(df_c1$season)

table(df_c1$cycle, df_c1$season)

df_c1$season_f <- factor(df_c1$season, levels = c("fall", "winter", "spring"))
table(df_c1$season_f, as.numeric(df_c1$season_f), exclude= NULL)

df_c1$grd_f <- factor(df_c1$grd, levels = c("3", "4", "5"))
table(df_c1$grd_f, as.numeric(df_c1$grd_f), exclude= NULL)

table(df_c1$grd_f, df_c1$season_f)



#--- Subset data --------------------------------------------------------------#
df_c1 <- df_c1[order(df_c1$id, df_c1$term),] 
df_c1 <- df_c1[, c("id", "grd_f", "season_f", "cycle", "iweek", "iday", "rit")]
map_3s <- df_c1 
names(map_3s) <- c("id", "grd", "season", "cycle", "iweek", "iday", "rit")
#--- Save for internal testing 
save(map_3s, file = "testing_temp/map_3s.RData")


#--- Create data for apdesign pacakge
uid <- unique(df_c1$id)
id_num <- 1:length(uid)
id_df <- data.frame(uid, id_num)

df_fin <- merge(df_c1, id_df, by.x = "id", by.y = "uid") 
head(df_fin)

df_fin$score <- round((df_fin$rit / 10) + 10, 0)

df_fin <- df_fin[, c("id_num", "cycle", "grd_f", "season_f", "iweek", "score")]
colnames(df_fin) <- c("id", "cycle", "grd", "season", "week", "score")

df <- df_fin
str(df)
#--- Check range of times


three_season_cycle <- df
save(three_season_cycle, file = "apdesign/data/three_season_cycle.RData")

