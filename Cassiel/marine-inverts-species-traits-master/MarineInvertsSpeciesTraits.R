library(dplyr)
library(tidyr)
library(agricolae)
library(ggfortify)
library(ggplot2)
library(ggfortify)
library(GGally)
MarineInvertsTraits <- read.csv("Documents/MarineInvertebratesTraits.csv")
MarineInvertsTraits <- MarineInvertsTraits[1:154,]

#creating subset with only data columns
traits.subset <- MarineInvertsTraits %>%
select(c(6:17))
summary(traits.subset)

#changing character to factor
traits.subset$hbt <- as.factor(traits.subset$hbt)
traits.subset$lvl_dvp <- as.factor(traits.subset$lvl_dvp)

#calculating log for fecundity 
traits.subset <- traits.subset %>% mutate(fcd_log = log(fcd))
summary(traits.subset)

#calculating log for length max length
traits.subset <- traits.subset %>% mutate(lgtmax_log = log(lgt_max))
traits.subset$lgtmax_log <- as.numeric(traits.subset$lgtmax_log)

#pairs plot for data columns
ggpairs(data = traits.subset)

#looking at some plots
ggplot(data = traits.subset, aes(x = lat, y = fcd_log)) + geom_point()
ggplot(data = traits.subset, aes(x = lat_s, y = fcd_log)) + geom_point()
ggplot(data = traits.subset, aes(x = hbt, y = fcd_log)) + geom_boxplot()
ggplot(data = traits.subset, aes(x = lvl_dvp, y = lat)) + geom_boxplot()
ggplot(data = traits.subset, aes(x = lvl_dvp, y = lat_s)) + geom_boxplot()
ggplot(data = traits.subset, aes(x = lvl_dvp, y = lat_n)) + geom_boxplot()
ggplot(data = traits.subset, aes(x = lat_n, y = fcd_log)) + geom_point()
ggplot(data = traits.subset, aes(x = lgt_max, y = fcd_log)) + geom_point()
ggplot(data = traits.subset, aes(x = hbt, y = lat)) + geom_boxplot()
ggplot(data = traits.subset, aes(x = lvl_dvp, y = fcd_log)) + geom_boxplot()
levels(traits.subset$lvl_dvp)

#box plot for log fecundity and larval development
ggplot(data=subset(traits.subset, !is.na(lvl_dvp)), aes(x=lvl_dvp, y=fcd_log)) + 
  geom_boxplot(fill="darkseagreen1", color="springgreen4")  + 
  xlab("Larval development") +
  ylab("Log fecundity") +  
  theme_classic()+
  theme(axis.text.x = element_text(size=20), axis.text.y= element_text(size=20), 
  axis.title = element_text(size=30))
# ANOVA and Tukey test for log fecundity and larval development
res.aov <- aov(fcd_log ~ lvl_dvp, data = traits.subset)
summary(res.aov)
TukeyHSD(res.aov)

#linear regression for latitudinal range and log fecundity (used figure in poster and ppt presentation)
ggplot(data=subset(traits.subset, !is.na(lat)), aes(x = lat, y = fcd_log)) +
  theme_classic() +
  geom_point(color = "darkorchid3", size=3) +
  geom_smooth(color="blue3", method="lm", fullrange=TRUE, size=1)+
  xlab("Latitudinal range (degrees)") +
  ylab("Log fecundity (number of eggs)")+
  theme(axis.text.x = element_text(size=30), axis.text.y= element_text(size=20), 
        axis.title = element_text(size=40))
ggsave("plot6.png", width=16.28, height=10, unit="in")
#linear model for lat and fcd, fecundity increases with latitudinal range (p=0.04)
lm1 <- lm(fcd_log ~ lat, data = traits.subset)
summary(lm1)
#for every degree of latitude, fecundity increases by 3.56%
(exp(0.03496)-1)*100


#linear regression for northern range boundary and max length (used this figure in ppt presentation)
ggplot(data = traits.subset, aes(x = lat_n, y = lgtmax_log)) + 
  geom_point(color = "red1", size=3) + 
  geom_smooth(color="red4", method="lm", fullrange=TRUE, size=1)+
  xlab("Northern range boundary (degrees)") +
  ylab("Body size (mm)")+
  theme_classic()+
  theme(axis.text.x = element_text(size=20), axis.text.y= element_text(size=20), 
  axis.title = element_text(size=30))
#body size decreases as latitude increases ? (not significant, p=0.5)
lm2 <- lm(lgtmax_log ~ lat_n, data = traits.subset)
summary(lm2)

#linear regression for fecundity and max length
ggplot(data = traits.subset, aes(x = fcd_log, y = lgtmax_log)) + 
  geom_point(color = "palegreen2", size=3) + 
  geom_smooth(color="palegreen4", method="lm", fullrange=TRUE, size=1)+
  xlab("Fecundity (number of eggs)") +
  ylab("Body size (mm)")+
  theme_classic()+
  theme(axis.text.x = element_text(size=20), axis.text.y= element_text(size=20), 
        axis.title = element_text(size=30))
#lm for max length and fecundity (p=0.05)
lm3 <- lm(fcd_log ~ lgtmax_log, data = traits.subset)
summary(lm3)

#linear regression for fecundity and max length
ggplot(data = traits.subset, aes(x = fcd_log, y = lgtmax_log)) + 
  geom_point(color = "palegreen2", size=3) + 
  geom_smooth(color="palegreen4", method="lm", fullrange=TRUE, size=1)+

#linear regression for northern range boundary and avg length
ggplot(data = traits.subset, aes(x = lat_n, y = lgt_avg)) + 
  geom_point(color = "palegreen2", size=3) + 
  geom_smooth(color="palegreen4", method="lm", fullrange=TRUE, size=1)+
  xlab("Northern range boundary") +
  ylab("Body size (mm)")+
  theme_classic()+
  theme(axis.text.x = element_text(size=20), axis.text.y= element_text(size=20), 
        axis.title = element_text(size=30))
#lm for avg length and northern range boundary
lm4 <- lm(lgt_avg ~ lat_n, data = traits.subset)
summary(lm4)

#linear regression for latitudinal range and max length
ggplot(data = traits.subset, aes (x=lat, y=lgtmax_log)) +
  geom_point(color="midnightblue", size=3) +
  geom_smooth(color="purple4",method="lm",fullrange=TRUE, size=1)+
  xlab("Latitudinal range(degrees)") +
  ylab("Body size (mm)")+
  theme_classic()+
  theme(axis.text.x = element_text(size=20), axis.text.y= element_text(size=20), 
        axis.title = element_text(size=30))
#lm for lat and lgt_max (p=0.03)
lm5 <- lm(lgt_max ~ lat, data = traits.subset)
summary(lm5)

#linear regression for latitudinal range and avg length
ggplot(data = traits.subset, aes (x=lat, y=lgt_avg)) +
  geom_point() +
  geom_smooth(method="lm",fullrange=TRUE)
#lm for lat and avg length
lm9 <- lm(lgt_avg ~ lat, data = traits.subset)
summary(lm9)

#linear regression for max length and lat_s
ggplot(data = traits.subset, aes (x=lat_s, y=lgt_max)) +
  geom_point()+
  geom_smooth(method="lm", fullrange=TRUE)
#p=0.06
lm6 <- lm(lgt_max ~ lat_s, data=traits.subset)
summary(lm6)?

#fcd and lvldvp
ggplot(data=subset(traits.subset, !is.na(lvl_dvp)), aes(x=lvl_dvp, y=fcd_log)) + 
  geom_boxplot() 
#anova, tukey (not significant)
res.aov3 <- aov(fcd_log ~ lvl_dvp, data = traits.subset)
summary(res.aov3)
TukeyHSD(res.aov3)

#hbt, fcd
ggplot(data=subset(traits.subset, !is.na(hbt)), aes(x=hbt, y=fcd_log)) + 
  geom_boxplot() 
#anova, tukey (not significant)
res.aov4 <- aov(fcd_log ~ hbt, data = traits.subset)
summary(res.aov4)
TukeyHSD(res.aov4)

#hbt, lat (used this figure in poster)
ggplot(data=subset(traits.subset, !is.na(hbt)), aes(x=hbt, y=lat)) + 
geom_boxplot(fill="thistle", color="mediumpurple4")  + 
  xlab("Habitat") +
  ylab("Latitudinal range (degrees)") +  
  theme_classic()+
  theme(axis.text.x = element_text(size=30), axis.text.y= element_text(size=20), 
        axis.title = element_text(size=40))
ggsave("plot5.png", width=16.28, height=10, unit="in")
#anova, tukey
res.aov5 <- aov(lat ~ hbt, data = traits.subset)
summary(res.aov5)
TukeyHSD(res.aov5)

#box plot for latitudinal range and larval development
ggplot(data=subset(traits.subset, !is.na(lvl_dvp)), aes(x=lvl_dvp, y=lat)) + 
  geom_boxplot()  + 
  xlab("Larval development") +
  ylab("Latitudinal range") +
  scale_color_brewer(palette="plum3")+
  theme_classic()
# ANOVA and Tukey test for latitudinal range and larval development
res.aov2 <- aov(lat ~ lvl_dvp, data = traits.subset)
summary(res.aov2)
TukeyHSD(res.aov2)

#lat and lat_s
ggplot(data = traits.subset, aes(x = lat, y = lat_s)) + 
  geom_point() + 
  geom_smooth(method="lm", fullrange=TRUE)
#lm
lm7 <- lm(lat ~ lat_s, data = traits.subset)
summary(lm7)

#lat and lat_n
ggplot(data = traits.subset, aes(x = lat, y = lat_n)) + 
  geom_point() + 
  geom_smooth(method="lm", fullrange=TRUE)
#lm
lm8 <- lm(lat ~ lat_n, data = traits.subset)
summary(lm8)

