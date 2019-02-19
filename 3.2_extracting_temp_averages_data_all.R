######setup#######
library(ggplot2)
library(data.table)
library(readr)
library(ggplot2) 
library(ggfortify) 
library(dplyr)
library(datasets)
library(car) # for vif, crPlots
library(scatterplot3d)
library(tidyr)

setwd("~/Documents/zjk desktop/grad school/Rutgers/LabWork/Col-Ext/R")

######loaddata########
load("/Users/zoekitchel/Documents/zjk desktop/grad school/Rutgers/LabWork/Col-Ext/R/col_ext_organized/rawdata/data_all") #this gives one row to every species/trawl combo

######action#########

data_all2 <- data_all[complete.cases(data_all$btemp), ]
data_all3 <- data_all2[complete.cases(data_all2$datetime), ]
data_all_filtered <- data_all3 %>%
  group_by(reg, datetime) %>%
  summarise(mean(btemp))

data_all_filtered2 <- separate(data_all_filtered, datetime, c("DATE", "TIME"), sep=" ", remove = TRUE)
data_all_filtered2$DATE2 <- as.Date(data_all_filtered2$DATE, "%Y-%m-%d")
data_all_filtered2$YEAR <- strftime(data_all_filtered2$DATE, format = "%Y")

colnames(data_all_filtered2)[4] <- "BTEMP"

data_all_filtered3 <- data_all_filtered2 %>%
  group_by(reg, YEAR) %>%
  summarise(mean(BTEMP))

colnames(data_all_filtered3)[3] <- "mean_BTEMP"
data_all_filtered3$YEAR <- as.numeric(data_all_filtered3$YEAR)
data_all_filtered3$reg <- as.factor(data_all_filtered3$reg)

#now I have to get mean bottom temperature per region over all years
meantemp_reg <- data_all_filtered3 %>%
  group_by(reg) %>%
  summarise(mean(mean_BTEMP))
#get rid of wcann
meantemp_reg2 <- meantemp_reg[-9,]
meantemp_reg2$`mean(mean_BTEMP)` <- round(meantemp_reg2$`mean(mean_BTEMP)`, digits = 2)
meantemp_round <- meantemp_reg2

#now I want to get minimum and maximum bottom temp per region over all years
minmaxtemp_reg <- data_all_filtered3 %>%
  group_by(reg) %>%
  summarise(min(mean_BTEMP), max(mean_BTEMP))
#again get rid of wcann
minmaxtemp_reg2 <- minmaxtemp_reg[-9,]
minmaxtemp_reg2[2:3] <- round(minmaxtemp_reg2[,2:3], digits=1)
minmaxtemp_round <- minmaxtemp_reg2

tempdetails_reg <- full_join(meantemp_round, minmaxtemp_round, by="reg")

#rename those ugly names
colnames(tempdetails_reg) <- c("reg", "mean", "min", "max")

######export#########

save(tempdetails_reg, file = "/Users/zoekitchel/Documents/zjk desktop/grad school/Rutgers/LabWork/Col-Ext/R/col_ext_organized/manipdata/tempdetails_reg.RData")
