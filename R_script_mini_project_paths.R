# ------------------------------------------------
# Dr Peter Hopcroft (based on earlier versions by Dr Tom Pugh and Dr Tom Matthews)
# February 2021
# ------------------------------------------------

# ------------------------------------------------
# 0.  Technical
# ------------------------------------------------

# uncomment these two lines if these packages are not working:
# install.packages("rworldmap")
# install.packages("RColorBrewer")
# install.packages("ncdf4")
# install.packages("fields")

library(rworldmap)
data(coastsCoarse)
library("ncdf4")
library("fields")
library("RColorBrewer")


# See RColorBrewer examples here:
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# In plots we use the BuGn scale for absolute plots (i.e. looking at one time-period)
col = brewer.pal(7, "BuGn"),
# and we use the BrBG colour scale for differences between two time periods:
#  col = brewer.pal(7, "BrBG"),


# ------------------------------------------------
# 1.  A quick look at observations to get started
# ------------------------------------------------
setwd("~/shared/data/Agriculture/HarvestedArea_Yields_Monfreda")

#load in harvested area for maize
harvest_maize <- nc_open('maize_AreaYieldProduction_1deg_harvarea.nc')

# have a look at the the nc file object
harvest_maize

# extract latitude, longitude and the data on harvest amount (in ha)
lat_har_maize <- ncvar_get(nc=harvest_maize, varid='lat')
lon_har_maize <- ncvar_get(nc=harvest_maize,varid='lon')
dat_har_maize <- ncvar_get(nc=harvest_maize, varid='maizeData')
nc_close(harvest_maize)

# the data are all upside down so we need to flip them:
dat_har_maize_flip <- dat_har_maize[,180:1]
lat_har_maize_flip <- lat_har_maize[180:1]

#let's try and plot it
image.plot(lon_har_maize, lat_har_maize_flip, dat_har_maize_flip, 
            col = brewer.pal(7, "BuGn"),
            xlab="", ylab="", 
           main="Maize Harvested Area", 
           legend.lab="ha", legend.line=4, legend.mar=7,aspect=1)

plot(coastsCoarse,add=T)

##The above is actual harvest data for maize (there is the equivalent data file
#for wheat in the data folder). 

# ------------------------------------------------
# 2.  A look at model projections
# ------------------------------------------------
# Now, lets look at model simulations projections
# of potential yield in kg of dry matter (DM) per m2 for the same crop. Each
# simulation uses climate for the period 1980-2010. You are provided with the
# mean values for potential over that period from the LPJmL model. Sensitivity
# simulations have been conducted in which temperature, precipitation, nitrogen
# fertilization or atmospheric CO2 mixing ratio are perturbed by a fixed amount
# to give and indication of how climate changes may affect the yields.


## let's try looking at the model projection for Maize with very low N fertilization. We will follow 
the same workflow as for the 
#harvest area data above.
setwd("~/shared/data/Agriculture/Crop_models/LPJmL/maize/A0")

file_model_maize<-nc_open('lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N10_A0_1deg_mean.nc4')
lat_model_maize <- ncvar_get(nc=file_model_maize, varid='lat')
lon_model_maize <- ncvar_get(nc=file_model_maize,varid='lon')
data_model_maize <- ncvar_get(nc=file_model_maize, varid='yield_mai')
nc_close(file_model_maize)
lat_model_maize_flip<-lat_model_maize[180:1]
data_model_maize_flip <- data_model_maize[,180:1]
image.plot(lon_model_maize, lat_model_maize_flip, 
           data_model_maize_flip, 
           xlab="", ylab="", 
           main="Predicted yield (T1) - Maize", 
           legend.lab="kg DM m-2", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BuGn"),
           zlim=c(0,10))
plot(coastsCoarse,add=T)

# With a model we can also look at projections of yield etc given a warmer climate.
# So let's load in a model projection with 2 C of warming:

file_model_maize_plus2C<nc_open('lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N10_A0_1deg_mean.nc4')
data_model_maize_plus2C <- ncvar_get(nc=file_model_maize_plus2C, varid='yield_mai')
nc_close(file_model_maize_plus2C)
data_model_maize_plus2C_flip <- data_model_maize_plus2C[,180:1]

# and now we're going to create an anomaly map i.e. the change in the yield due to the temperature.
# To do this we subtract one simulation (i.e. the warmer one) from the baseline:

data_model_maize_plus2C_minus_baseline<-data_model_maize_plus2C_flip -data_model_maize_flip
# then plot
image.plot(lon_model_maize, lat_model_maize_flip, 
           data_model_maize_plus2C_minus_baseline, 
           xlab="", ylab="", 
           main="Predicted yield (T1) - Maize", 
           legend.lab="kg DM m-2", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BrBG"),
           zlim=c(-2,2))
plot(coastsCoarse,add=T)

# OK so yield has increased in the high-latitudes a bit and decreased elsewhere. 
# How important are these changes?

# Let's calculate the global mean change:

 glob_yield_change_plus_2C<- mean(data_model_maize_pluse2C_minus_baseline,na.rm=TRUE)
 print(glob_yield_change_plus_2C)

# How about computing this as a % change?
    glob_yield_baseline<-mean(data_model_maize_flip,na.rm=TRUE)
    glob_yield_plus2C<-mean(data_model_maize_plus2C_flip,na.rm=TRUE)

 glob_yield_change_plus_2C_PERCENT<- 100*(glob_yield_plus2C-glob_yield_baseline)/glob_yield_baseline
 print(glob_yield_change_plus_2C_PERCENT)

#  So we get a 5.4 % decline.

# ------------------------------------------------
# 3.  Further analyses of model projections
# ------------------------------------------------
# Now your first task is to repeat the above but for some different climates, try at least 3.
# Assess which is worse of the ones chosen and think about which you think are more or less likely.

# You don't need to re-read in baselin it is already in the memory as:
# data_model_maize_flip

# We read in the plus 2 Celsius run which is labled by the 'T2':
# lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N10_A0_1deg_mean.nc4

# The full set of options for you to look at is:
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W30_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W30_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W30_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W30_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W30_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T2_W0_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C510_T0_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C510_T0_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C510_T0_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C510_T0_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C510_T0_W0_N60_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C660_T0_W0_N10_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C660_T0_W0_N120_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C660_T0_W0_N200_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C660_T0_W0_N250_A0_1deg_mean.nc4
#lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C660_T0_W0_N60_A0_1deg_mean.nc4

# C360 or C510 or C660 are the CO2 concentration in ppmv (today's level is ~ 410 ppmv)
# T0, T1, T2, T4 is the temperature increase in Celsius.
# W0, W30 is a % change in precipitation (rainfall). So W30 is a 30% increase in rain.
# N10,120,200,250 etc are different levels are applied fertilizer.

# So for example: lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T4_W0_N250_A0_1deg_mean.nc4
# has CO2=360 pppm, is 4 Celsius warmer, has nitrogen in put at 250, but no change in rainfall.

# You can also investigate the influence of assumptions about drop adaptation by using the A1 directory:
# These are equivalent simulations, but with adaptation to warming included in the model:
# You need to change to the 'A1' directory using:
 setwd("~/shared/data/Agriculture/Crop_models/LPJmL/maize/A1")


# ------------------------------------------------
# 4.  Thinking more generally - land-use
# ------------------------------------------------

# We're going to apply some of the above to look at land-use in the past and future.

# Observations of land-use are generally partly based on satellite data and partly on models. Future
# land-use is entirely based on models. We'll call past data 'observed' and future data 'projected'.
# First get to the right directory:

setwd("~/shared/data/Land-use/Historical_land-use")

file_obs_crop_2005<-nc_open('lu_1860_2015_luh2_2005_cropland.nc')
lat_obs_crop_2005 <- ncvar_get(nc=file_obs_crop_2005, varid='lat')
lon_obs_crop_2005 <- ncvar_get(nc=file_obs_crop_2005,varid='lon')
data_obs_crop_2005 <- ncvar_get(nc=file_obs_crop_2005, varid='cropland')
nc_close(file_obs_crop_2005)
image.plot(lon_obs_crop_2005, lat_obs_crop_2005, 
           data_obs_crop_2005, 
           xlab="", ylab="", 
           main="Cropland fraction 2005", 
           legend.lab="[0-1]", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BuGn"),
           zlim=c(0,1))
plot(coastsCoarse,add=T)

# What about earlier in time - let's look at 1915:
file_obs_crop_1915<-nc_open('lu_1860_2015_luh2_1915_cropland.nc')
data_obs_crop_1915 <- ncvar_get(nc=file_obs_crop_1915, varid='cropland')
nc_close(file_obs_crop_1915)

# But we'll plot the difference (i.e. the anomaly):

data_obs_crop_2005_minus_1915<-data_obs_crop_2005  - data_obs_crop_1915

image.plot(lon_obs_crop_2005, lat_obs_crop_2005, 
           data_obs_crop_2005_minus_1915, 
           xlab="", ylab="", 
           main="Cropland fraction 1915", 
           legend.lab="[0-1]", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BrBG"),
           zlim=c(-0.5,0.5))
plot(coastsCoarse,add=T)

# So there is less cropland in central/Western Europe and Eastern N. America, but more elsewhere.


# This can be also repeated with the projected future cropland areas. These are divided into RCPs:
# RCP (representative concentration pathways) are projected pathways of economic/social activity
# and comprise projected changes in land-use, fossil fuel emissions and so on.
# RCP 2.6 is a mitigation scenario.
# RCP 4.6 and 6  are intermediate scenarios in terms of rate of warming.
# RCP 8.5 is a fossil fuel-intensive scenario (near- worst case).

# The data for these are found for e.g. (for RCP8.5) by using:
setwd("~/shared/data/Land-use/Future_land-use/RCP85")
# then the files for e.g. the year 2085, are loaded with:
file_projected_crop_2085<-nc_open('landuse_hurtt_rcp85_2085_cropland.nc')
data_projected_crop_2085 <- ncvar_get(nc=file_projected_crop_2085, varid='cropland')
nc_close(file_projected_crop_2085)

# Your next task is to repeat the above for at least 2 of the RCP scenarios.
# How do the scenarios compare? Is there a challenge from the land-use in terms of food production?

# ------------------------------------------------
# 5.  Land-use policy conflicts
# ------------------------------------------------
#  Read in one version of RCP 2.6 future cropland fraction from a model known as MAgPIE:  
  setwd("~/shared/data/Policy_scenarios/baseline_LU_scenarios")
  file_proj_crop_2085<-nc_open('MAgPIE_NoLuMIT-RCP26_2085_cropland.nc')
lat_proj_crop_2085 <- ncvar_get(nc=file_proj_crop_2085, varid='lat')
lon_proj_crop_2085 <- ncvar_get(nc=file_proj_crop_2085,varid='lon')
data_proj_crop_2085 <- ncvar_get(nc=file_proj_crop_2085, varid='cropland')
nc_close(file_proj_crop_2085)
image.plot(lon_proj_crop_2085, lat_proj_crop_2085, 
           data_proj_crop_2085, 
           xlab="", ylab="", 
           main="Cropland fraction 2085", 
           legend.lab="[0-1]", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BuGn"),
           zlim=c(0,1))
plot(coastsCoarse,add=T)
  

# Now read in the equivalent but this tiome with BECCS policy added in to the model:
setwd("~/shared/data/Policy_scenarios/BECCS_LU_scenarios")
file_projBECCS_crop_2085<-nc_open('MAgPIE_MIT-BIO-RCP26_2085_cropland.nc')
data_projBECCS_crop_2085 <- ncvar_get(nc=file_projBECCS_crop_2085, varid='cropland')
nc_close(file_projBECCS_crop_2085)

data_obs_crop_BECCS_minus_RCP26<-data_projBECCS_crop_2085  - data_proj_crop_2085

image.plot(lon_obs_crop_2005, lat_obs_crop_2005, 
           data_obs_crop_BECCS_minus_RCP26, 
           xlab="", ylab="", 
           main="Cropland fraction 2085 BECCS minus no BECCS", 
           legend.lab="[0-1]", 
           legend.line=4, legend.mar=7,
           col = brewer.pal(7, "BrBG"),
           zlim=c(-1,1))
plot(coastsCoarse,add=T)

# This model results shows some fairly drastic changes in crop area due to BECCS.
# What about change in natural area.
# These are in files 

 setwd("~/shared/data/Policy_scenarios/baseline_LU_scenarios")
 file_proj_natural_2085<-nc_open('MAgPIE_NoLuMIT-RCP26_2085_natural.nc')
 
# and 

 setwd("~/shared/data/Policy_scenarios/BECCS_LU_scenarios")
 file_proj_natural_2085<-nc_open('MAgPIE_NoLuMIT-RCP26_2085_natural.nc')

# You final task is to explore the impact of BECCS on natural areas.
# Which time period is worse? (choose from 2005, 2035, 2085)
# Which regions are worst hit?


# There are also outputs from another equivalent model IMAGE. 
# Are these results consistent with the MAgPIE model that you've looked at above?

# For this you'll need: 

 setwd("~/shared/data/Policy_scenarios/baseline_LU_scenarios")
 file_proj_natural_2085<-nc_open('IMAGE_NoLuMIT-RCP26_2085_natural.nc')
 
# and 

 setwd("~/shared/data/Policy_scenarios/BECCS_LU_scenarios")
 file_proj_natural_2085<-nc_open('IMAGE_MIT-BIO-RCP26_2085_natural.nc')


# ------------------------------------------------
# 6.  Finish and write-up
# ------------------------------------------------
#  Well done. You've completed the R component. Now please collect together all of the results
#  you have generated and write this up as a short policy-briefing.

