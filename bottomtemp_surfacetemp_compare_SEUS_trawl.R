library(data.table)
library(ggplot2)
library(dplyr)

load("/Users/zoekitchel/Documents/grad school/Rutgers/LabWork/Col_Ext/R/col_ext_organized/rawdata/master_hauls_March7_2017.RData")

hauls <- data.table(hauls)
hauls_reduced <- hauls[region == "SCDNR_SEUS",][year > 1989][year < 2015]

hauls_reduced[, mean_bottemp := mean(bottemp, na.rm = T), by = year][, mean_surftemp := mean(surftemp, na.rm = T), by = year]

ggplot(data = hauls_reduced, aes(x=mean_bottemp, y = mean_surftemp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,30) +
  ylim(0,30) +
  theme_classic()
  
#Average difference?
mean(hauls_reduced$mean_surftemp-hauls_reduced$mean_bottemp) #47
min(hauls_reduced$mean_surftemp-hauls_reduced$mean_bottemp)
max(hauls_reduced$mean_surftemp-hauls_reduced$mean_bottemp)

ggplot(data = hauls_reduced, aes(x=year)) +
  geom_point(aes(y = mean_surftemp), col = "blue") +
  geom_smooth(aes(y = mean_surftemp), method = lm, col = "blue") +
  geom_point(aes(y = mean_bottemp), col = "green") +
  geom_smooth(aes(y = mean_bottemp),method = lm, col = "green") + 
  theme_classic()

cor(hauls_reduced$mean_surftemp,hauls_reduced$mean_bottemp)
mod <- lm(data = hauls_reduced, mean_surftemp~mean_bottemp)

summary(mod)
______
hauls_reduced2 <- hauls[region == "AFSC_WCTri",]

hauls_reduced2[, mean_bottemp := mean(bottemp, na.rm = T), by = year][, mean_surftemp := mean(surftemp, na.rm = T), by = year]

ggplot(data = hauls_reduced2, aes(x=mean_bottemp, y = mean_surftemp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,30) +
  ylim(0,30) +
  theme_classic()

summary(as.factor(hauls$region))


----
  hauls_reduced4 <- hauls[region == "SEFSC_GOMex",]

hauls_reduced4[, mean_bottemp := mean(bottemp, na.rm = T), by = year][, mean_surftemp := mean(surftemp, na.rm = T), by = year]

ggplot(data = hauls_reduced4, aes(x=mean_bottemp, y = mean_surftemp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,30) +
  ylim(0,30) +
  theme_classic()

---
