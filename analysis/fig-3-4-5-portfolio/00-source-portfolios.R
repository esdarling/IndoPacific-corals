#This file sources additional packages for Fig 3 plots
library(here)
source(here("analysis", "00-source.R"))

#d loads original model dataset with posterior predictions
d <- fread(here("data", "coral-data-with-posteriors.csv")) %>% 
  as_tibble()
names(d)
#----
#maybe add sumcover posteriors, look at increase in lh with increase in sumcover
#show that more cover, more functional lh (not just a 10% threshold to drive down to)
#----

#recalculate strategy based on posteriors
#do we want to calculate stratgies on median? 50% risk? more, less risk? 

d <- d %>% 
  mutate(pp.strategy = ifelse(comp.stQ25 >= 0.1 & 
                                CoralTemp.maxDHW < 4, "refuge", 
                              ifelse(comp.stQ25 >= 0.1 & 
                                       CoralTemp.maxDHW > 4, "recover", "transform"))) %>% 
  mutate(pp.strategy = fct_relevel(pp.strategy, 
                                   "refuge", 
                                   "recover", 
                                   "transform")) %>% 
  mutate(strategy = fct_relevel(strategy, 
                                   "refuge", 
                                   "recover", 
                                   "transform"))

table(d$pp.strategy)
table(d$strategy)


#what level of risk should we accept for strategies? 
#median is 50% risk
summary(d$comp.stQ2.5)
summary(d$comp.stQ25)
summary(d$comp.st.median)
summary(d$comp.stQ75)
summary(d$comp.stQ97.5)

# test <- d %>% 
#   mutate(pp.strategy = ifelse(comp.stQ2.5 >= 0.1 & 
#                                 CoralTemp.maxDHW < 4, "refuge", 
#                               ifelse(comp.stQ2.5 >= 0.1 & 
#                                        CoralTemp.maxDHW > 4, "recover", "transform"))) %>% 
#   mutate(pp.strategy = fct_relevel(pp.strategy, 
#                                    "refuge", 
#                                    "recover", 
#                                    "transform"))
# table(test$pp.strategy)

#-- 
#how many stategies are different? 
test <- d %>% 
  filter(strategy != pp.strategy) %>% 
  mutate(change = paste(strategy, pp.strategy, sep = "-to-"))
nrow(test) 
nrow(test) /nrow(d)
test

table(test$change)
