######################### PACKAGE ######################################
library(jsonlite)
library(dplyr)


######################### RETRIEVING DATA ##############################
url <- "https://fantasy.premierleague.com/api/bootstrap-static"
fpl_ret <- fromJSON(url)
summary(fpl_ret)


######################### SELECTING LIST ###############################
player <- fpl_ret$elements
team <- fpl_ret$teams
pos <- fpl_ret$element_types


######################### JOINING DATAFRAME ############################
## join player and pos using player$id and pos$id
names(pos)[1] <- "position"
names(player)[9] <- "position"
df_pos <- full_join(player, pos, by = "position")

#player$element_types <- ifelse(player$element_types==1, "goalkeeper",
#                        ifelse(player$element_types==2, "defender",
#                        ifelse(player$element_types==3, "midfielder",
#                                                        "forward")))


## join df and team using player$team_code and team$code
names(team)[1] <- "team_code"
df_team <- full_join(df_pos, team, by = "team_code")
str(df_team)


######################### SELECTING VAR USED ###########################
#setwd("D:\\KULIAH\\Project\\1. FPL\\Data")
#data <- read.csv("FPLData_231229_1448.csv", header=TRUE)

data <- df_team
data$player <- paste(data$first_name, data$second_name)

data2 <- data[,c(119, 103, 91, 26, 20, 12, 14, 19, 21, 23, 29, 37:48, 
                 50:53, 73:78, 87, 88)]
names(data2)[2] <- "team"
names(data2)[3] <- "position"
names(data2)[7] <- "form"


######################### NA AND DUPLICATE CHECKING ####################
##### NA CHECKING
colSums(is.na(data2))

##### DUPLICATE CHECKING
sum(duplicated(data2$player)) #NO DUPLICATION


######################### DATA ELEMENT CHANGING ########################
data2$now_cost <- round(data2$now_cost*0.1, 1)
data2$status <- ifelse(data$status=="u", "unavailable",
                ifelse(data$status=="a", "available",
                ifelse(data$status=="d", "doubtful",
                ifelse(data$status=="i", "injured/international duty",
                ifelse(data$status=="n", "not available", 
                                         "suspended")))))

data2$photo <- gsub("jpg", "png", data2$photo)
data2$photo <- paste(rep("https://resources.premierleague.com/premierleague/photos/players/250x250/p", dim(data)[1]), 
                     data2$photo, sep="")


######################### DATA CLEANING ################################
## deleting players with 0 min played
data3 <- data2[data2$status != "unavailable",]
data3 <- data3[data3$minutes>0,]


######################### DATA TYPE CHANGING ###########################
## CHANGING DATA TYPE
#form, influence, creativity, threat, ict index, xA, xG as numeric
#team, pos, status as factor
data3 <- data3 %>% mutate_at(c(2:4), as.factor)
data3 <- data3 %>% mutate_at(c(6:35), as.numeric)


######################### DATA TYPE CHANGING ###########################
### XLSX
library(xlsx)
write.xlsx(data3, file = "FPLData_date_time.xlsx",
           sheetName = "data", append = FALSE)

### CSV
write.csv(data3, file = "FPLData_date_time.xlsx",
          row.names = FALSE)

### TXT
write.table(gk, file = "FPLData_date_time.txt")
