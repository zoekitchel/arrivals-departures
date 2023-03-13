# gains-losses
This is the repository, data, and code for:

"Regional species losses lag behind species gains and thermal conditions across the North American continental shelf"

A research article published in Global Ecology and Biogeography, 2023

Zoë J. Kitchel<sup>1</sup>* (zoe.kitchel@rutgers.edu)
Malin L. Pinsky<sup>2</sup> (malin.pinsky@rutgers.edu)

Affiliations: 

1 Ecology and Evolution Graduate Program, Rutgers University, New Brunswick, New Jersey, USA

2 Department of Ecology, Evolution, and Natural Resources, Rutgers University, New Brunswick, New Jersey, USA

*Corresponding author address: Department of Ecology, Evolution, & Natural Resources, 14 College Farm Road, Rutgers University, New Brunswick, New Jersey, USA, 08901


This repository provides code to replicate analyses in the above publication. Raw species observation data from US and Canada trawl surveys can be accessed from [Ocean Adapt](https://oceanadapt.rutgers.edu/). Temperature data are from the [Simple Ocean Data Assimilation 2.2.4 and 3.3.1](https://www.soda.umd.edu/). Some files (listed in .gitignore file) are too big for github, but you can find them on Box: https://rutgers.box.com/s/ez9zxt26nori9w7nx12lwzqly31o8fdn.

Most analyses were run on macOS 11.7, using R version 4.2.1. A few computations were performed on a linux server running R version 3.5.2, but could easily be performed on a PC.

These analyes explore whether temperature can be used to predict species gains in new regions, and losses from previously inhabited regions for a diverse array of finfish and invertebrates in North American shelf regions. Additionally, these analyses consider whether gains and losses are responding similarly to temperature, and at the same time. 

Road Map

1. Access spp observation data from [Ocean Adapt](https://oceanadapt.rutgers.edu/). Clean and process using methods from [trawlDiversity](https://github.com/rBatt/trawlDiversity). Begin with spp_master.RData file. 
2. Access temperature data from [Simple Ocean Data Assimilation 2.2.4 (pre 1980) and 3.3.1 (1980 onward)](https://www.soda.umd.edu/). Use nco package which is run in bash script. Extract sst and sbt, convert from nc to raster format in R. [Code/Data_prep/Temp/SODA/Import_SODA_web_convert_raster.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SODA/Import_SODA_web_convert_raster.Rmd)
3. Convert SODA temperature raster to data table format. [Code/Data_prep/Temp/SODA/GRD_raster_SODA_to_datatable.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SODA/GRD_raster_SODA_to_datatable.Rmd)
4. SODA doesn't capture temperature in SEUS well, so pull in [buoy data for SEUS buoys](https://www.ndbc.noaa.gov/) and model temperature values for this region. Code: [Code/Data_prep/Temp/SEUS_buoydata.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/SEUS_buoydata.Rmd). 
5. Link SODA temp to species observation data. [Code/Data_prep/Link_SODA_spp_master.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Link_SODA_spp_master.Rmd).
6. Scale temperature values to SD 1 Mean = 0. [Code/Data_prep/Temp/Scaling_temperature.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Data_prep/Temp/Scaling_temperature.Rmd).
7. Run logistic regressions predicting gains and losses from temperature predictors. [Code/Temperature_analyses/Col_Ext_Models_Final.Rmd](https://github.com/zoekitchel/colonization-extinction/blob/master/Code/Temperature_analyses/Col_Ext_Models_Final.Rmd).

Figures

- [Figure 1](https://github.com/zoekitchel/trawl_gains_losses/tree/master/figures/Figure1)
- Figure 2: [Map](https://github.com/zoekitchel/trawl_gains_losses/blob/master/figures/Figure2/map_sites.Rmd) & [Species Presence/Absence](https://github.com/zoekitchel/trawl_gains_losses/tree/master/figures/Figure2/Figure2_spp_presabs.Rmd)
- Figure 3: [a](https://github.com/zoekitchel/trawl_gains_losses/tree/master/figures/Figure3-3_4_merge/Figure3/Figure3a_code.Rmd),[b/c](https://github.com/zoekitchel/trawl_gains_losses/tree/master/figures/Figure3-3_4_merge/Figure4/Figure4_code.Rmd)
- [Figure 4](https://github.com/zoekitchel/trawl_gains_losses/tree/master/figures/Figure4_new/Figure4_August_New_TempGainLoss.Rmd)

Don't  hesitate to get in touch with Zoë Kitchel with any questions or concerns. (zoe.kitchel[at]rutgers[dot]edu

