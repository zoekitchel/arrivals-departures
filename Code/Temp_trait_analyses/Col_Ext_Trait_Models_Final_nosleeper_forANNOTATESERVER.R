library(MuMIn)
library(lme4)
library(data.table)
library(ggplot2)
library(plyr)
library(glmmTMB)

spp_master_ztemp_seus_buoy.traits_scaled <- readRDS("spp_master_ztemp_seus_buoy.traits_scaled.rds")

spp_key <- fread("spp_key.csv")

spp_master_ztemp_seus_buoy.traits_scaled <- spp_key[spp_master_ztemp_seus_buoy.traits_scaled, on = "spp", nomatch = NULL]

#delete first year each region is sampled (because gains and losses are based on transitions between years, so we need 2 years of observations)

spp_master_ztemp_seus_buoy.traits_scaled[,min_year := min(year),.(reg)]

spp_master_ztemp_seus_buoy.traits_scaled <- 
  spp_master_ztemp_seus_buoy.traits_scaled[year != min_year] #make sure this only runs once!

#temp scaled vars only
#names of scaled columns
tempvars <- grep(".*temp.*scaled", names(spp_master_ztemp_seus_buoy.traits_scaled), value = T)

#other reference and trait columns we need
othervars_arrivals <- c("reg", "year", "col", "phylum","class","order","family","genus","spp", "age.max_ocean_scaled", "age.maturity_ocean_scaled", "tl_ocean_scaled", "length.max_ocean_scaled")

colstosaveforarrivals <- c(othervars_arrivals, tempvars)

#Arrival table
arrival.reduced_nosleeper <- spp_master_ztemp_seus_buoy.traits_scaled[,colstosaveforarrivals, with = F][(spp != "Somniosus pacificus")] #reduces to columns we need, excludes sleeper shark (outlier)

#Departure table

othervars_departure <- c("reg", "year", "now_ext",  "phylum","class","order","family","genus","spp", "age.max_ocean_scaled", "age.maturity_ocean_scaled", "tl_ocean_scaled", "length.max_ocean_scaled")
colstosavefordeparture <- c(othervars_departure, tempvars)

departure.reduced_nosleeper <- spp_master_ztemp_seus_buoy.traits_scaled[,colstosavefordeparture, with = F][(spp != "Somniosus pacificus")] #reduces to columns we need, excludes sleeper shark (outlier)

spp_master_ztemp_seus_buoy.traits_scaled.nosleeper <- spp_master_ztemp_seus_buoy.traits_scaled[(spp != "Somniosus pacificus")]

#save for plotting later
saveRDS(spp_master_ztemp_seus_buoy.traits_scaled.nosleeper, "spp_master_ztemp_seus_buoy.traits_scaled.nosleeper.rds")



#to assist with convergence: year as factor
arrival.reduced_nosleeper[,year := as.factor(year)]

#delete any observations without full trait data
arrival.reduced_nosleeper.r <- arrival.reduced_nosleeper[complete.cases(arrival.reduced_nosleeper[ , .(age.max_ocean_scaled, age.maturity_ocean_scaled, length.max_ocean_scaled, tl_ocean_scaled)]),]

#save for plotting in separate script
saveRDS(arrival.reduced_nosleeper.r, "arrival.reduced_nosleeper.r.rds")

#for now, going with two traits and one temp variable
#going to make loop to make all models I need to look at with single variables
all.temp.variables <- colnames(arrival.reduced_nosleeper.r[,14:261]) #CHECKKK
all.temp.variables.0 <- c(0, all.temp.variables) #add 0 as option so models can just include traits
all.temp.variables.0 <- all.temp.variables.0[!grepl("*mean*", all.temp.variables.0)]#get rid of MEAN temp variables
all.trait.variables <- c(0, "tl_ocean_scaled", "age.max_ocean_scaled","age.maturity_ocean_scaled", "length.max_ocean_scaled",
                         "tl_ocean_scaled + age.max_ocean_scaled", "age.max_ocean_scaled + tl_ocean_scaled",
                         "tl_ocean_scaled + age.maturity_ocean_scaled", "age.maturity_ocean_scaled + tl_ocean_scaled",
                         "tl_ocean_scaled + length.max_ocean_scaled", "length.max_ocean_scaled + tl_ocean_scaled",
                         "age.max_ocean_scaled + age.maturity_ocean_scaled", "age.maturity_ocean_scaled + age.max_ocean_scaled",
                         "age.max_ocean_scaled + length.max_ocean_scaled", "length.max_ocean_scaled + age.max_ocean_scaled",
                         "age.maturity_ocean_scaled + length.max_ocean_scaled", "length.max_ocean_scaled + age.maturity_ocean_scaled")

sign <- c("+", "*")

full_var_set <- as.data.table(expand.grid(all.temp.variables.0, sign, all.trait.variables))

#delete repeats (just temperature and both + and *)

full_var_set.r <- full_var_set[!which((Var1 == "0" & Var2 =="*")|(Var3 == "0" & Var2 == "*")),] #this excludes repeats with *

#repeated rows with traits
repeats <- c("age.maturity_ocean_scaled + age.max_ocean_scaled","tl_ocean_scaled + age.maturity_ocean_scaled", "length.max_ocean_scaled + age.maturity_ocean_scaled", "age.maturity_ocean_scaled + length.max_ocean_scaled", "tl_ocean_scaled + length.max_ocean_scaled", "tl_ocean_scaled + age.max_ocean_scaled")

#delete rows with + and same combination of 2nd and 3rd traits
full_var_set.r2 <- full_var_set.r[!which(Var2 == "+"& Var3 %in% repeats)]


#build datatable
arrival_model_comparison_traits_nosleeper <- as.data.table(matrix(nrow = nrow(full_var_set.r2))) 
arrival_model_comparison_traits_nosleeper[, temp_variable:=as.factor(V1)][, sign:=as.factor(V1)][, trait_variable:=as.factor(V1)][, AICc:=as.numeric(V1)][, converge:= as.logical(V1)]
arrival_model_comparison_traits_nosleeper[, V1 := NULL]

for (i in 1:nrow(full_var_set.r2)){
  
  temp <- full_var_set.r2$Var1[i]
  sign <- full_var_set.r2$Var2[i]
  trait <- full_var_set.r2$Var3[i]
  
  raw_form <- ifelse(temp == 0 & trait != 0,
                     paste("col ~", trait, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                     ifelse(temp != 0 & trait == 0,
                            paste("col ~", temp, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                            ifelse(temp == 0 & trait == 0,
                                   paste("col ~", "(1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                                   paste("col ~",temp,sign,trait, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"))))
  
  formula <- as.formula(raw_form)
  
  mod <- glmer(formula, family = binomial, data = arrival.reduced_nosleeper.r, nAGQ = 0)
  
  arrival_model_comparison_traits_nosleeper[i,temp_variable := full_var_set.r2$Var1[i]]
  arrival_model_comparison_traits_nosleeper[i,sign := full_var_set.r2$Var2[i]]
  arrival_model_comparison_traits_nosleeper[i,trait_variable := full_var_set.r2$Var3[i]]
  arrival_model_comparison_traits_nosleeper[i,AICc := AICc(mod)]
  
  warning <- mod@optinfo$conv$lme4$messages[2]
  converges <- ifelse(is.null(warning) == TRUE, T, F)
  
  arrival_model_comparison_traits_nosleeper[i, converge := converges]
  
  print(paste(i, nrow(arrival_model_comparison_traits_nosleeper), sep = "/"))
  
}

saveRDS(arrival_model_comparison_traits_nosleeper, "arrival_model_comparison_traits_nosleeper.rds")

fwrite("arrival_model_comparison_traits_nosleeper.csv")

#for now, going with two traits and one temp variable
#going to make loop to make all models I need to look at with single variables
#don't want mean

#to assist with convergence: year as factor
departure.reduced_nosleeper[,year := as.factor(year)]

#delete any observations without full trait data
departure.reduced_nosleeper.r <- departure.reduced_nosleeper[
  complete.cases(departure.reduced_nosleeper[ 
    ,.(age.max_ocean_scaled, age.maturity_ocean_scaled, length.max_ocean_scaled, tl_ocean_scaled)]),]

#save for plotting in separate script
saveRDS(departure.reduced_nosleeper.r,"departure.reduced_nosleeper.r.rds")


departure_model_comparison_traits_nosleeper <- as.data.table(matrix(nrow = nrow(full_var_set.r2))) 
departure_model_comparison_traits_nosleeper[, temp_variable:=as.factor(V1)][, sign:=as.factor(V1)][, trait_variable:=as.factor(V1)][, AICc:=as.numeric(V1)][, converge:= as.logical(V1)]
departure_model_comparison_traits_nosleeper[, V1 := NULL]

for (i in 1:nrow(full_var_set.r2)){
  
  temp <- full_var_set.r2$Var1[i]
  sign <- full_var_set.r2$Var2[i]
  trait <- full_var_set.r2$Var3[i]
  
  raw_form <- ifelse(temp == 0 & trait != 0,
                     paste("now_ext ~", trait, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                     ifelse(temp != 0 & trait == 0,
                            paste("now_ext ~", temp, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                            ifelse(temp == 0 & trait == 0,
                                   paste("now_ext ~", "(1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"),
                                   paste("now_ext ~",temp,sign,trait, "+ (1|reg)  + (1|class) + (1|order) + (1|family) + (1|genus) + (1|spp)"))))
  
  formula <- as.formula(raw_form)
  
  mod <- glmer(formula, family = binomial, data = departure.reduced_nosleeper.r, nAGQ = 0)
  
  departure_model_comparison_traits_nosleeper[i,temp_variable := full_var_set.r2$Var1[i]]
  departure_model_comparison_traits_nosleeper[i,sign := full_var_set.r2$Var2[i]]
  departure_model_comparison_traits_nosleeper[i,trait_variable := full_var_set.r2$Var3[i]]
  departure_model_comparison_traits_nosleeper[i,AICc := AICc(mod)]
  
  warning <- mod@optinfo$conv$lme4$messages[2]
  converges <- ifelse(is.null(warning) == TRUE, T, F)
  
  departure_model_comparison_traits_nosleeper[i, converge := converges]
  
  print(paste(i, nrow(departure_model_comparison_traits_nosleeper), sep = "/"))
  
}


saveRDS(departure_model_comparison_traits_nosleeper, "departure_model_comparison_traits_nosleeper.rds")

fwrite("departure_model_comparison_traits_nosleeper.csv")

