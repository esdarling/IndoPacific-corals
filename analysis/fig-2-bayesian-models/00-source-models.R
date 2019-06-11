#This script is a source for the taxa and lh coef plots

### ---------------------------------------------------------------------
#COEFFICIENT DATASETS

##read in all .csv coefficient files, add response variable name, join together

setwd(here("analysis", "outputs", "coef-tables"))
paths <- dir(path = here("analysis", "outputs", "coef-tables"), 
             pattern = 'coef')
paths

coef <- paths %>%
  lapply(FUN = function(x) read.csv(x) %>% 
           mutate(response = sapply(strsplit(x, split = "\\.")  , function(x) x[2]))) %>%
  bind_rows() %>% 
  as.tibble()
warnings()

#deal with PIT coef plots
setwd(here("analysis", "outputs", "coef-tables", "pit-only"))
paths.pit <- dir(path = here("analysis", "outputs", "coef-tables", "pit-only"), 
             pattern = 'coef')
paths.pit

coef.pit <- paths.pit %>%
  lapply(FUN = function(x) read.csv(x) %>% 
           mutate(response = sapply(strsplit(x, split = "\\.")  , function(x) x[2]))) %>%
  bind_rows() %>% 
  as.tibble() %>% 
  mutate(response = "pit")


head(coef.pit)
unique(coef.pit$lh)

coef <- bind_rows(coef, coef.pit)

### ---------------------------------------------------------------------
#join with variable names and types (climate-social-env-method)
vars <- read.csv(here("data", "bayes variable labels.csv"),
                 header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>% 
  as.tibble() %>% 
  rename(var = "Variable", 
         group = "Group", 
         label = "Label") %>% 
  mutate(group = dplyr::recode(group, 
                        human = "Social", 
                        climate = "Climate", 
                        env = "Environment", 
                        sampling = "Methods"))

head(vars)
names(vars)

coef <- left_join(coef, vars)
#unique(test$var)
#unique(coef$var)

unique(coef$lh)
unique(coef$group)
coef$group <- factor(coef$group,
                     levels = c("Climate", "Social", "Environment", "Methods"))

#order variables for plots
## order by label in covariate table - test
unique(coef$label)
coef$label <- factor(coef$label, levels = 
                       c("Restricted management", #social
                         "No-take closures",
                         "National HDI",
                         "Croplands growth", 
                         "Total croplands", 
                         "Market gravity",
                         "Population gravity",
                         "Maximum DHW", #climate
                         "Years since maximum DHW",
                         "Habitat, crest", #env
                         "Habitat, flat",
                         "Depth",
                         "Primary productivity",
                         "Wave exposure",
                         "Cyclone exposure",
                         "Reef area", 
                         "Latitude, from equator",
                         "Sampling points", #methods
                         "Photo quadrats",
                         "Line intercept transects"))

unique(coef$var)

check <- coef %>% 
  filter(var == "ManagementNoMtake") %>% 
  arrange(lh, response)
