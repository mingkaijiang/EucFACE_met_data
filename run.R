###########################################################################
###########################################################################
###                                                                     ###
###                   EucFACE met data code repository                  ###
###                                                                     ###
###########################################################################
###########################################################################
###########################################################################
###                Step 1: Set up the basics                            ###
###                                                                     ###
###########################################################################
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")


###########################################################################
###                        Step 2: canopy T data                        ###
###                                                                     ###
###########################################################################
#### prepare above and below canopy air T profile at hourly timestep
### above canopy air T
atDF <- prepare_above_canopy_met_data()

### below canopy air T
btDF <- prepare_below_canopy_met_data()


#### read data from saved files
atDF <- read.csv("output/met_data_hourly_above_canopy.csv")
btDF <- read.csv("output/met_data_hourly_below_canopy.csv")

#### merge two datasets
airTDF <- merge_above_below_canopy_data(atDF, btDF)


#### make basic plots
plot_canopy_temperature_profile(airTDF)

