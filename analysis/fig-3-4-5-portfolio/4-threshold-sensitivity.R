#This file conducts a sensitivity analysis of the 10% threshold
library(here)
source(here("analysis", "fig-3", "00-source-portfolios.R"))

head(bind)

#proportions of sites by each strategy - with 10% threshold
bind %>% 
  group_by(strategy) %>% 
  tally() %>% 
  mutate(perc = n / 2584*100)

#reclass with 8% threshold 
names(bind)

bind <- bind %>% 
  mutate(strategy8 = ifelse(comp_plus_st >= 8 & CoralTemp.maxDHW <=4, "protect", 
                        ifelse(comp_plus_st >= 8 & CoralTemp.maxDHW >=4, "recover", 
                               "transform")),
         strategy10 = ifelse(comp_plus_st >= 10 & CoralTemp.maxDHW <=4, "protect", 
                            ifelse(comp_plus_st >= 10 & CoralTemp.maxDHW >=4, "recover", 
                                   "transform")), 
         strategy12 = ifelse(comp_plus_st >= 12 & CoralTemp.maxDHW <=4, "protect", 
                            ifelse(comp_plus_st >= 12 & CoralTemp.maxDHW >=4, "recover", 
                                   "transform")))

table8 <- bind %>% 
  group_by(strategy8) %>% 
  tally() %>% 
  mutate(perc = n / 2584*100) %>% 
  mutate(threshold = "8perc")

table10 <- bind %>% 
  group_by(strategy10) %>% 
  tally() %>% 
  mutate(perc = n / 2584*100) %>% 
  mutate(threshold = "10perc")

table12 <- bind %>% 
  group_by(strategy12) %>% 
  tally() %>% 
  mutate(perc = n / 2584*100) %>% 
  mutate(threshold = "12perc")

sens.table <- bind_cols(table10, table8, table12)
sens.table
write.csv(sens.table, here("analysis", "outputs", 
                           "threshold-sensitivity.csv"), 
          row.names = FALSE)




