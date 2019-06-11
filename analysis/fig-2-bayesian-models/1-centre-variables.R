#This file centres and standardizes variables for modelling
#converted from rjags code written by Georgina Gurney

library(here)
source(here("analysis", "00-source.R"))

#centre and divide by 2 SDs
??arm:: rescale

#order variables by order they will go into model (and be plotted)
head(d.models)
names(d.models)

table(d.models$Source)

#CLIMATE
hist(d.models$past.maxDHW)
hist(d.models$years.sinceDHW)

#HUMAN
hist(d.models$Grav_NearPop.max)
hist(d.models$Grav_Markets.max)
hist(d.models$perc_crop2012)
hist(d.models$perc_crop2002.2012)
hist(d.models$hdi2015)
table(d.models$Management)
unique(d.models$Management)

#ENV
table(d.models$Habitat)
hist(d.models$Depth_m)
hist(d.models$npp_mean)
hist(d.models$wave_mean)
hist(d.models$maxTCdays.mean)
hist(d.models$reef_area_100km)
hist(d.models$Latitude)

#METHODS
table(d.models$Method)
hist(d.models$n_points)

#RESCALE and TRANSFORM as necessary 

d.vars <- d.models %>% 
  dplyr::select(Source:Year, Province,
                sumcover, 
                perc_competitive:perc_weedy,
                past.maxDHW, years.sinceDHW, 
                Grav_NearPop.max, Grav_Markets.max, 
                perc_crop2012, perc_crop2002.2012, 
                hdi2015, Management, 
                Habitat, Depth_m, 
                npp_mean, wave_mean, maxTCdays.mean, 
                reef_area_100km, Latitude, 
                Method, n_points) %>% 
  mutate(socialsite = as.factor(socialsite),
         past.maxDHW = arm::rescale(log(past.maxDHW + 1)), 
         years.sinceDHW = arm::rescale(years.sinceDHW), 
         Grav_NearPop.max = arm::rescale(log(Grav_NearPop.max + 1)), 
         Grav_Markets.max = arm::rescale(log(Grav_Markets.max + 1)), 
         perc_crop2012 = arm::rescale(log(perc_crop2012 + 1)), 
         perc_crop2002.2012 = arm::rescale(perc_crop2002.2012), 
         hdi2015 = arm::rescale(hdi2015 + 1), 
         Management = stats::relevel(as.factor(Management), ref = "Fished"),
         Habitat = stats::relevel(as.factor(Habitat), ref = "Slope"), 
         Depth_m = arm::rescale(log(Depth_m + 1)), 
         npp_mean = arm::rescale(log(npp_mean + 1)),
         wave_mean = arm::rescale(log(wave_mean + 1)),
         maxTCdays.mean = arm::rescale(log(maxTCdays.mean + 1)),
         reef_area_100km = arm::rescale(log(reef_area_100km + 1)),
         Latitude = arm::rescale(Latitude), 
         Method = stats::relevel(as.factor(Method), ref = "PIT"), 
         n_points = arm::rescale(log(n_points + 1)))

summary(d.vars)         
