#code for terminal
conda activate /local/home/zoek/enter/envs/myRenv3_5
R

library(dplyr)
library(data.table)
library(raster)
library(ncdf4)
library(lubridate)
library(maptools)
library(rgdal)
library(tidyr)
library(ggplot2)


load("col_ext/hauls_catch_Dec2017 1.RData")

latlongyear <- hauls %>%
  dplyr::select(lat, lon, year, region)

#add years back to 1953
#for each group, find first year, then generate new observations with repeated GPS sequence going back 10 years

#regions
regions <- levels(as.factor(hauls$region))

newlatlongyear <- data.table(matrix(ncol = 5, nrow = 0))

colnames(newlatlongyear) <- c("lat", "lon", "year", "month", "region")

for (i in 1:length(regions)) {
  subset <- latlongyear[latlongyear$region == regions[i],] #subset to a region, ct 4588
  minyear <- min(subset$year) #minimum year for this region
  newminyear <- minyear - 10 #minimum year minus 10 (to allow for lags)
  secondtomin <- min(subset$year[subset$year!=min(subset$year)]) #year of second earliest tow
  maxyear <- max(subset$year) #year of latest tow
  #secondtomax <- max(subset$year[subset$year!=max(subset$year)]) #year of second latest tow
  
  allyears <- seq(newminyear, maxyear) #years we need to simulate GPS points for, 19
  
  #subsetlatlong <- subset[subset$year <= secondtomin | subset$year >= secondtomax,] #subset lat lon values used in first 2 years and last two years of survey
  subsetlatlong.noreg <- subset[,c(1:2)] #don't need column with location or year anymore, 1488
  #randomly choose 1/5 of these
  twentieth <- 0.2*nrow(subsetlatlong.noreg)
  subsetlatlong.reduced <- data.table(subsetlatlong.noreg[sample(nrow(subsetlatlong.noreg), twentieth), ]) #229 GPS points
  
  allmonths <- c(1,2,7,8) #we want temp values for every month
  
  subsetlatlong.m <- subsetlatlong.reduced[ , latlon := do.call(paste, c(.SD, sep = "_"))] #merge lat lon in order to expand grid (essentially, lat_lon becomes a key)
  
  subsetlatlong.m.vector <- subsetlatlong.m$latlon #extract vector
  

  #we now have three vectors we need to expand grid for, subsetlatlong.m.vector, allyears, and allmonths, length of expanded grid should equal
  invisible(length(subsetlatlong.m.vector)*length(allyears)*length(allmonths)) #this checks out
  
  tempdf <- data.table(expand.grid(subsetlatlong.m.vector, allyears, allmonths)) #matches every year value with a lat_lon_month
  
  tempdf.sep <- tempdf[, c("lat", "lon") := tstrsplit(Var1, "_", fixed=TRUE)]
  colnames(tempdf.sep)[colnames(tempdf.sep)=="Var2"] <- "year"
  colnames(tempdf.sep)[colnames(tempdf.sep)=="Var3"] <- "month"
  tempdf.sep$region <- regions[i]
  tempdf.sep <- tempdf.sep[,Var1:=NULL] #delete column we split from
  
  newlatlongyear <- rbind(newlatlongyear, tempdf.sep) #this file now has ALL possible year, lat, lon, month points we need to extract
  
} # this for loop generates latitude year combinations for 10 years before sampling began to allow us to look at lags up to 10 years with no NA's

#it's possible at some point I may need to randomly sample these, because 8736684 extractions may be too many... and I was correct
#we know it works okay with 136044 hauls, let's take a sub-sample of this, and then multiply by years (~70) and months (~12) (840)

cp <- nc_open("col_ext/CARTON-GIESE_SODA_v2p1p6_sstemp.nc")
print(cp)

# =========================================
# = Function to Read in SODA, Grab Surface =
# =========================================
get.soda.sst <- function(file){
  
  soda.info <- nc_open(file)
  name.soda.sizes <- sapply(soda.info$var$temp$dim, function(x)x$name)
  soda.sizes <- soda.info$var$temp$size
  dim.units <- sapply(soda.info$var$temp$dim, function(x)x$units)
  print(dim.units)
  stopifnot(grepl("months since ", dim.units[4])) # make sure time is in correct units and in right place
  names(soda.sizes) <- name.soda.sizes
  ntime <- soda.sizes["time"]
  ndepth <- soda.sizes["depth"]
  
  soda.time0 <- soda.info$var$temp$dim[[4]]$vals
  ref.date <- as.Date(gsub("months since ", "", dim.units[4]))
  start.before.ref <- grepl("-", soda.time0[1]) # is the first date before ref.date?
  n.month.before <- ceiling(abs(soda.time0[1])) + as.integer(start.before.ref)
  start.increment <- ifelse(start.before.ref, "-1 month", "1 month")
  time.start <- rev(seq.Date(ref.date, by=start.increment, length.out=n.month.before))[1]
  soda.time <- seq.Date(time.start, by="1 month", length.out=ntime)
  
  soda.sst <- brick(file)
  names(soda.sst) <- soda.time
  
  
  return(soda.sst)
  
}

## Using Ryan's code
soda_sst <- get.soda.sst("col_ext/CARTON-GIESE_SODA_v2p1p6_sstemp.nc")

saveRDS(soda_sst, "SODA2.1.6_SST.rds")



newlatlongyear[, month_digits := ifelse(month < 10, paste0("0", month), month)][, day := paste0("0", c(1))][, year_digits := paste0("X", newlatlongyear$year)]
newlatlongyear[, ID := paste(newlatlongyear$year_digits, newlatlongyear$month_digits, newlatlongyear$day, sep=".")]

newlatlongyear[, lat := as.numeric(lat)][, lon := as.numeric(lon)]

vec <- c("ID", "lat", "lon")
newlatlongyearID <- newlatlongyear[, vec, with=FALSE]

save(newlatlongyear, newlatlongyearID, file = "newlatlongyear.Rdata")

locations <- SpatialPoints(cbind(newlatlongyear$lon, newlatlongyear$lat), 
                           proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(locations)

system.time(temp_values <- raster::extract(soda_sst, locations))
save(temp_values, file = "extracted_temp_values_SODA.Rdata")
#load("col_ext/extracted_temp_values_SODA.Rdata")


justlatlong <- newlatlongyear[, vec[2:3], with = FALSE]

system.time(df <- cbind(justlatlong, temp_values))
save(df, file = "latlong_temp_values.Rdata")

system.time(df_long <- melt(df, measure.vars = 3:ncol(df),
                variable.name = "dateID", value.name = "temp"))

df_long[, c("year", "month", "day") := tstrsplit(dateID, ".", fixed=TRUE)]

df_long$year <- substr(as.character(df_long$year), 2, 5)
save(df_long, file = "SODA_monthly_data.R")

latlongreg.key <- latlongyear[,c(1,2,4)] #ended here on amphiprion


date_temp_reg <- df_long[latlongreg.key, nomatch = 0, on = c("lat","lon")]

date_temp_reg <- date_temp_reg[,list(mean_month_temp=mean(temp)),by=c("region", "year", "month")][,list(region_year_mean_sst = mean(mean_month_temp), region_year_max_sst = max(mean_month_temp), region_year_min_sst = min(mean_month_temp)), by = c("region", "year")][,list(region_year_seas_sst = region_year_max_sst - region_year_min_sst), b = c("region", "year")]

save(date_temp_reg, file = "date_ssttemp_avg_SODA.Rdata") #save this so i can start here!


#adding lag values (1, 2, 3, 4, 5) for seasonality and change in temperature
nm1 <- grep("*sst*", colnames(date_temp_reg), value=TRUE)
nm2 <- paste("lag1", nm1, sep=".")
nm3 <- paste("lag2", nm1, sep=".")
nm4 <- paste("lag3", nm1, sep=".")
nm5 <- paste("lag4", nm1, sep=".")
nm6 <- paste("lag5", nm1, sep=".")
nm7 <- paste("lag6", nm1, sep=".")
nm8 <- paste("lag7", nm1, sep=".")
nm9 <- paste("lag8", nm1, sep=".")
nm10 <- paste("lag9", nm1, sep=".")
nm11 <- paste("lag10", nm1, sep=".")
date_temp_reg_avg_withlags <- data.table(date_temp_reg)
date_temp_reg_avg_withlags[, (nm2) :=  data.table::shift(.SD, n=1, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm3) :=  data.table::shift(.SD, n=2, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm4) :=  data.table::shift(.SD, n=3, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm5) :=  data.table::shift(.SD, n=4, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm6) :=  data.table::shift(.SD, n=5, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm7) :=  data.table::shift(.SD, n=6, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm8) :=  data.table::shift(.SD, n=7, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm9) :=  data.table::shift(.SD, n=8, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm10) :=  data.table::shift(.SD, n=9, type = "lag"), by=region, .SDcols=nm1]
date_temp_reg_avg_withlags[, (nm11) :=  data.table::shift(.SD, n=10, type = "lag"), by=region, .SDcols=nm1]

date_temp_reg_avg_withlags[,c("sst_mean_change") := (region_year_mean_sst - lag1.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change") := (region_year_max_sst - lag1.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change") := (region_year_min_sst - lag1.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change") := (region_year_seas_sst - lag1.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag1") := (lag1.region_year_mean_sst - lag2.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag1") := (lag1.region_year_mean_sst - lag2.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag1") := (lag1.region_year_mean_sst - lag2.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag1") := (lag1.region_year_mean_sst - lag2.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag2") := (lag2.region_year_mean_sst - lag3.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag2") := (lag2.region_year_mean_sst - lag3.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag2") := (lag2.region_year_mean_sst - lag3.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag2") := (lag2.region_year_mean_sst - lag3.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag3") := (lag3.region_year_mean_sst - lag4.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag3") := (lag3.region_year_mean_sst - lag4.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag3") := (lag3.region_year_mean_sst - lag4.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag3") := (lag3.region_year_mean_sst - lag4.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag4") := (lag4.region_year_mean_sst - lag5.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag4") := (lag4.region_year_mean_sst - lag5.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag4") := (lag4.region_year_mean_sst - lag5.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag4") := (lag4.region_year_mean_sst - lag5.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag5") := (lag5.region_year_mean_sst - lag6.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag5") := (lag5.region_year_mean_sst - lag6.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag5") := (lag5.region_year_mean_sst - lag6.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag5") := (lag5.region_year_mean_sst - lag6.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag6") := (lag6.region_year_mean_sst - lag7.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag6") := (lag6.region_year_mean_sst - lag7.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag6") := (lag6.region_year_mean_sst - lag7.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag6") := (lag6.region_year_mean_sst - lag7.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag7") := (lag7.region_year_mean_sst - lag8.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag7") := (lag7.region_year_mean_sst - lag8.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag7") := (lag7.region_year_mean_sst - lag8.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag7") := (lag7.region_year_mean_sst - lag8.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag8") := (lag8.region_year_mean_sst - lag9.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag8") := (lag8.region_year_mean_sst - lag9.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag8") := (lag8.region_year_mean_sst - lag9.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag8") := (lag8.region_year_mean_sst - lag9.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag9") := (lag9.region_year_mean_sst - lag10.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag9") := (lag9.region_year_mean_sst - lag10.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag9") := (lag9.region_year_mean_sst - lag10.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag9") := (lag9.region_year_mean_sst - lag10.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_abs") := abs(sst_mean_change)]
date_temp_reg_avg_withlags[,c("sst_max_change_abs") := abs(sst_max_change)]
date_temp_reg_avg_withlags[,c("sst_min_change_abs") := abs(sst_min_change)]
date_temp_reg_avg_withlags[,c("sst_seas_change_abs") := abs(sst_seas_change)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag1_abs") := abs(lag1.region_year_mean_sst - lag2.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag1_abs") := abs(lag1.region_year_max_sst - lag2.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag1_abs") := abs(lag1.region_year_min_sst - lag2.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag1_abs") := abs(lag1.region_year_seas_sst - lag2.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag2_abs") := abs(lag2.region_year_mean_sst - lag3.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag2_abs") := abs(lag2.region_year_max_sst - lag3.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag2_abs") := abs(lag2.region_year_min_sst - lag3.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag2_abs") := abs(lag2.region_year_seas_sst - lag3.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag3_abs") := abs(lag3.region_year_mean_sst - lag4.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag3_abs") := abs(lag3.region_year_max_sst - lag4.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag3_abs") := abs(lag3.region_year_min_sst - lag4.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag3_abs") := abs(lag3.region_year_seas_sst - lag4.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag4_abs") := abs(lag4.region_year_mean_sst - lag5.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag4_abs") := abs(lag4.region_year_max_sst - lag5.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag4_abs") := abs(lag4.region_year_min_sst - lag5.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag4_abs") := abs(lag4.region_year_seas_sst - lag5.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag5_abs") := abs(lag5.region_year_mean_sst - lag6.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag5_abs") := abs(lag5.region_year_max_sst - lag6.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag5_abs") := abs(lag5.region_year_min_sst - lag6.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag5_abs") := abs(lag5.region_year_seas_sst - lag6.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag6_abs") := abs(lag6.region_year_mean_sst - lag7.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag6_abs") := abs(lag6.region_year_max_sst - lag7.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag6_abs") := abs(lag6.region_year_min_sst - lag7.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag6_abs") := abs(lag6.region_year_seas_sst - lag7.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag7_abs") := abs(lag7.region_year_mean_sst - lag8.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag7_abs") := abs(lag7.region_year_max_sst - lag8.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag7_abs") := abs(lag7.region_year_min_sst - lag8.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag7_abs") := abs(lag7.region_year_seas_sst - lag8.region_year_seas_sst)]

date_temp_reg_avg_withlags[,c("sst_mean_change_lag8_abs") := abs(lag8.region_year_mean_sst - lag9.region_year_mean_sst)]
date_temp_reg_avg_withlags[,c("sst_max_change_lag8_abs") := abs(lag8.region_year_max_sst - lag9.region_year_max_sst)]
date_temp_reg_avg_withlags[,c("sst_min_change_lag8_abs") := abs(lag8.region_year_min_sst - lag9.region_year_min_sst)]
date_temp_reg_avg_withlags[,c("sst_seas_change_lag8_abs") := abs(lag8.region_year_seas_sst - lag9.region_year_seas_sst)]

#save all these goodies!!
date_temp_reg_avg_withlags.naomit <- na.omit(date_temp_reg_avg_withlags)
save(date_temp_reg_avg_withlags.naomit, file = "date_ssttemp_avg_SODA_withlags.Rdata")

########
library(MuMIn)
library(lme4)
library(faraway)
load("col_ext/spp_master.RData")

#match region names between two data tables
reg <- levels(as.factor(spp_master$reg))
region <- c("AFSC_Aleutians","AFSC_EBS", "SEFSC_GOMex", "AFSC_GOA", "NEFSC_NEUS", "DFO_Newfoundland", "SCDNR_SEUS", "DFO_ScotianShelf", "AFSC_WCTri")
regions.dt <- data.table(reg, region)

date_temp_reg_avg_withlags.naomit <- date_temp_reg_avg_withlags.naomit[regions.dt, nomatch = 0, on = c("region")]

date_temp_reg_avg_withlags.naomit$reg <- as.factor(date_temp_reg_avg_withlags.naomit$reg)
#colnames(date_temp_reg_avg_withlags.naomit)[colnames(date_temp_reg_avg_withlags.naomit) == "region"] <- "reg"

date_temp_reg_avg_withlags.naomit$year <- as.numeric(date_temp_reg_avg_withlags.naomit$year)

#r combine species data with temp data
spp_master_ztemp <- spp_master[date_temp_reg_avg_withlags.naomit, nomatch = 0, on = c("reg", "year")]

