#This file summarizes analysis for MS
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "fig-1-map", "00-source-map.R"))


#d.map is dataset 
head(d.map)

table(d.map$lh.max)

d.map %>% 
  select(lh.max) %>% 
  group_by(lh.max) %>% 
  dplyr::summarize(n.reefs = n()) %>% 
  mutate(total = 2584, 
         prop.reefs = n.reefs/total)

#sum of comp and st
0.424 + 0.433

#total coral cover summary
head(d.map)
test <- d.map %>% 
  filter(sumcover > 0)

summary(d.map$sumcover); sd(d.map$sumcover)
summary(test$sumcover)

filter(d.map, sumcover == 0)
filter(d.map, sumcover == 100)



#summary methods for revision
names(d.map)

d.map %>% 
  select(Method) %>% 
  group_by(Method) %>% 
  dplyr::summarize(n.reefs = n()) %>% 
  mutate(total = 2584, 
         prop.reefs = n.reefs/total)

0.216+0.63
557+1628

#who collected LITs? 
lit <- filter(d.map, Method == "LIT")
unique(lit$Source)
