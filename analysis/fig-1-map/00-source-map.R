#This file sources additional packages for Fig 1 map
library(here)
source(here("analysis", "00-source.R"))

library(car)
library(gdata)
library(gridExtra)
library(grid)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(oce)
library(corrplot)
library(matrixStats)

#update Sean ggsidekick
# install.packages("devtools")
#devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)

#check out d.models
names(d.models)
d.map <- d.models

hist(d.map$Latitude)
hist(d.map$Longitude)

#add X and Y for mapping
d.map$X <-  ifelse(d.map$Longitude < 0, d.map$Longitude+360, d.map$Longitude)
d.map$Y <- d.map$Latitude
hist(d.map$X)
hist(d.map$Y)

#clean up some provinces
d.map$Province <- recode_factor(d.map$Province, 
                                   Indonesia = "Indonesian",
                                   "Andaman-Nicobar Islands" = "Andaman-Nicobar")
unique(d.map$Province)

#find which columns have highest life-history values, and that value
head(d.map)
names(d.map)

d.map <- d.map %>% 
  mutate(lh.max = colnames(d.models[14:17])[max.col(d.models[14:17],
                                                    ties.method="first")], 
         lh.max.value = rowMaxs(as.matrix(d.models[14:17])))
unique(d.map$lh.max)

#check matrix math
check <- d.map %>% 
  select(perc_competitive:perc_weedy, lh.max, lh.max.value)
