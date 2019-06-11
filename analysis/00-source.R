#install libraries
library(here)
library(tidyverse)
library(dplyr)
library(ggsidekick)
library(rstanarm)
library(bayesplot)
library(skimr)
library(betareg)
library(brms)
library(lme4)
library(reshape2)
library(Hmisc)
library(data.table)

#load data for models
#data is available on request from primary data holders
#a list of data holders and contact information is provided in Supplementary  Table 8
d.models <- read.csv(here("data", "IP corals - models for Georgie_2Oct.csv"),
              header = TRUE, strip.white = TRUE, stringsAsFactors = TRUE)
head(d.models)
unique(d.models$Source)

#--------------------
#update management with 2018 new values
mgmt2018 <- read.csv(here("data", "Management update 2018_coauthors.csv"), 
                     header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
names(mgmt2018)

d.models <- d.models %>% 
  left_join(mgmt2018[,c("Source", "Site", "Management_2018update")], 
            by = c("Source", "Site")) %>% 
  select(Source:Grav_Markets.max, Management_2018update, 
         perc_crop2012:Habitat) %>% 
  rename("Management" = "Management_2018update")

names(d.models)

fwrite(d.models, here("data", "coral-model-data.csv"))
#--------------------

#archival data.. 
#load original genus data from surveys
d.genus <- read.csv(here("data", "genera perc_cover_8Sept2016.csv"), 
              header = TRUE, strip.white = TRUE, stringsAsFactors = TRUE)
head(d.genus)

#load lh data
lh <- read.csv(here("data", "allCoral LH_MASTER_10Sept.csv"), 
                    header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)      
head(lh)
unique(lh$LifeHistory)


