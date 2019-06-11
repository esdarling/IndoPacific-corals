#this code predicts comp and ST models to posteriors
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))
source(here("analysis", "bayesian-models", "4-readRDS-lh-models.R"))

summary(fit_competitive_brms)
summary(fit_stresstolerant_brms)

#predict posteriors from both models 
#use brms predict 

#competitive
#report out all 4000 chain outcomes, to add comp+st 
?gather
pp.comp <- predict(fit_competitive_brms, 
                   summary = FALSE) %>% #keep all 4000 samples
  as_tibble() %>% 
  gather(key = "site.order", value  = "predict.comp") %>% 
  mutate(order = as.numeric(str_extract(site.order, "\\d+"))) 

pp.comp
table(pp.comp$site.order) #4000 posteriors of comp for each site
nrow(pp.comp)

#stress tolerant
pp.st <- predict(fit_stresstolerant_brms, 
                 summary = FALSE) %>% #keep all 4000 samples
  as_tibble() %>% 
  gather(key = "site.order", value  = "predict.st") %>% 
  mutate(order = as.numeric(str_extract(site.order, "\\d+"))) 

table(pp.st$site.order) #4000 posteriors of comp for each site
4000*2584
nrow(pp.st)

#randomly sample from each, add together
#then summarize to get median + credible intervals for each site
comp.random <- pp.comp %>% 
  group_by(order, site.order) %>% 
  dplyr::sample_n(1000, replace = TRUE)

st.random <- pp.st %>% 
  group_by(order, site.order) %>% 
  dplyr::sample_n(1000, replace = TRUE)

nrow(comp.random)
nrow(st.random)
2584*1000

#add posteriors together
comp.random
st.random

pp.combined <- comp.random %>% 
  bind_cols(st.random[,"predict.st"]) %>% 
  select(order, site.order, predict.comp, predict.st) %>% 
  mutate(predict.comp.st = predict.comp + predict.st) %>% #predictions >1; SUPER RARE <0.1% 
  filter(predict.comp.st < 1)
pp.combined

# check <- pp.combined %>% 
#   filter(predict.comp.st > 1)
# nrow(check) / nrow(pp.combined)

pp.summary <- pp.combined %>% 
  group_by(order) %>% 
  dplyr::summarize(comp.median = median(predict.comp), 
                   compQ2.5 = quantile(predict.comp, 0.025), 
                   compQ25 = quantile(predict.comp, 0.25),
                   compQ75 = quantile(predict.comp, 0.75),
                   compQ97.5 = quantile(predict.comp, 0.975),
                   st.median = median(predict.st), 
                   stQ2.5 = quantile(predict.st, 0.025), 
                   stQ25 = quantile(predict.st, 0.25),
                   stQ75 = quantile(predict.st, 0.75),
                   stQ97.5 = quantile(predict.st, 0.975),
                   comp.st.median = median(predict.comp.st), 
                   comp.stQ2.5 = quantile(predict.comp.st, 0.025), 
                   comp.stQ25 = quantile(predict.comp.st, 0.25),
                   comp.stQ75 = quantile(predict.comp.st, 0.75),
                   comp.stQ97.5 = quantile(predict.comp.st, 0.975))
summary(pp.summary)
pp.summary

#add estimates back into d.models
d.models
nrow(d.models)

d.models <- bind_cols(d.models, pp.summary) %>% 
  as_tibble()
d.models

#plot posterior comp with empirical 
ggplot(data = d.models, 
       aes(x = perc_competitive/100, y = comp.median)) + 
  geom_abline(colour = "grey80", lty = 2) +
  geom_linerange(colour = "red", alpha = 0.1, 
                  aes(ymin = compQ2.5, ymax = compQ97.5)) + 
  geom_point(colour = "red", alpha = 0.75) + 
  theme_sleek() 

ggsave(here("analysis", "outputs", "figs", "pp-comp-comparison.pdf"), 
       height = 5, width = 5)

summary(lm(d.models$comp.median ~ d.models$perc_competitive))
#R2 = 0.5327

#plot posterior comp with empirical 
ggplot(data = d.models, 
       aes(x = perc_stresstolerant/100, y = st.median)) + 
  geom_abline(colour = "grey80", lty = 2) +
  geom_linerange(colour = "darkblue", alpha = 0.1, 
                 aes(ymin = stQ2.5, ymax = stQ97.5)) + 
  geom_point(colour = "darkblue", alpha = 0.75) + 
  theme_sleek() 

ggsave(here("analysis", "outputs", "figs", "pp-st-comparison.pdf"), 
       height = 5, width = 5)

summary(lm(d.models$st.median ~ d.models$perc_stresstolerant))
#R2 = 0.536

#add into bind
#bind is dataset with 2014-2017 DHW
#and strategies
bind <- read.csv(here("data", "Fig3 bind data-3July2018.csv"), 
                 header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
names(bind)

bind.crop <- bind %>% 
  select(Source, Site, 
         X,Y, 
         Nation,
         CoralTemp.maxDHW, #max DHW 2014-2017
         CoralTemp.maxDHW.year, #year of max DHW
         strategy, comp_plus_st)

d.models <- d.models %>% 
  left_join(bind.crop)
head(d.models)

#plot posterior comp + st with empirical 
ggplot(data = d.models, 
       aes(x = comp_plus_st/100, y = comp.st.median)) + 
  geom_abline(colour = "grey80", lty = 2) +
  geom_linerange(colour = "purple", alpha = 0.1, 
                 aes(ymin = comp.stQ2.5, ymax = comp.stQ97.5)) + 
  geom_point(colour = "purple", alpha = 0.75) + 
  theme_sleek() 

ggsave(here("analysis", "outputs", "figs", "pp-comp.st-comparison.pdf"), 
       height = 5, width = 5)

summary(lm(d.models$comp.st.median ~ d.models$comp_plus_st))
#R2 = 0.5767

#save file 
fwrite(d.models, 
       here("data", "coral-data-with-posteriors.csv"))

     