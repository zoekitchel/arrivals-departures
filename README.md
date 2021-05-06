# colonization-extinction

"Regional marine departures lag behind arrivals across North American shelf regions"

Zoë J. Kitchel<sup>1</sup>* (zoe.kitchel@rutgers.edu)
Malin L. Pinsky<sup>2</sup> (malin.pinsky@rutgers.edu)

Affiliations: 

1 Ecology and Evolution Graduate Program, Rutgers University, New Brunswick, New Jersey, USA

2 Department of Ecology, Evolution, and Natural Resources, Rutgers University, New Brunswick, New Jersey, USA

*Corresponding author address: Department of Ecology, Evolution, & Natural Resources, 14 College Farm Road, Rutgers University, New Brunswick, New Jersey, USA, 08901


This repository provides code to replicate analyses in the above publication. Raw species observation data from US and Canada trawl surveys can be accessed from [Ocean Adapt](https://oceanadapt.rutgers.edu/). Temperature data are from the [Simple Ocean Data Assimilation 2.2.4 and 3.3.1](https://www.soda.umd.edu/).

Road Map

1. Access spp observation data from [Ocean Adapt](https://oceanadapt.rutgers.edu/). Clean and process using methods from [trawlDiversity](https://github.com/rBatt/trawlDiversity). Begin with spp_master.RData file. 
2. Access temperature data from [Simple Ocean Data Assimilation 2.2.4 (pre 1980) and 3.3.1 (1980 onward)](https://www.soda.umd.edu/). Use nco package which is run in bash script. Extract sst and sbt, convert from nc to raster format in R. [Code/Data_prep/Temp/SODA/Import_SODA_web_convert_raster.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SODA/Import_SODA_web_convert_raster.Rmd)
3. Convert SODA temperature raster to data table format. [Code/Data_prep/Temp/SODA/GRD_raster_SODA_to_datatable.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SODA/GRD_raster_SODA_to_datatable.Rmd)
4. SODA doesn't capture temperature in SEUS well, so pull in [buoy data for SEUS buoys](https://www.ndbc.noaa.gov/) and model temperature values for this region. Code: [Code/Data_prep/Temp/SEUS_buoydata.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SEUS_buoydata.Rmd). 
5. Link SODA temp to species observation data. [Code/Data_prep/Link_SODA_spp_master.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Link_SODA_spp_master.Rmd).
6. Scale temperature values to SD 1 Mean = 0. [Code/Data_prep/Temp/Scaling_temperature.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/Scaling_temperature.Rmd).
7. Load and prepare species trait data from [Beukhof et al. 2019](https://doi.pangaea.de/10.1594/PANGAEA.900866). [Code/Data_prep/Traits/Prepping_trait_data.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Traits/Prepping_trait_data.Rmd).
8. Run logistic regressions predicting arrivals and departures from temperature predictors. [Code/Temperature_analyses/Col_Ext_Models_Final.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Temperature_analyses/Col_Ext_Models_Final.Rmd).
9. Run logistic regressions predicting arrivals and departures from temperature and trait predictors. [Code/Temp_trait_analyses/Col_Ext_Trait_Models_Final.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Temp_trait_analyses/Col_Ext_Trait_Models_Final.Rmd).

Figures

- Figure 1
- Figure 2
- Figure 3: [figures/Figure3/Figure3_code.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/figures/Figure3/Figure3_code.Rmd).
- Figure 4: [figures/Figure4/Figure4_code.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/figures/Figure4/Figure4_code.Rmd).
- Figure 5: 

