#This script runs brms STAN models for life histories
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))

#some model summaries 

#--------------------------------------
#stress tolerant and population gravity
summary(fit_stresstolerant_brms)
parnames(fit_stresstolerant_brms)[1:20]

# pop.grav <- rstan::extract(fit_stresstolerant_brms, 
#                            pars = "b_Grav_NearPop.max")[[1]]
post <- as_tibble(as.data.frame(fit_stresstolerant_brms)) %>% 
  select(b_Intercept:b_n_points)
head(post)
names(post)
nrow(post)
summary(post$b_Grav_NearPop.max)
quantile(post$b_Grav_NearPop.max, c(0.05, 0.95))

post %>% 
  filter(b_Grav_NearPop.max < 0) %>% 
  nrow()
15362/16000 #draws are negative slope
#166000 draws from 4 chains x 4000 iterations

#--------------------------------------
#generalist and population gravity
fit_generalist_brms

gen.post <- as_tibble(as.data.frame(fit_generalist_brms)) %>% 
  select(b_Intercept:b_n_points)
head(gen.post)
names(gen.post)
nrow(gen.post)

summary(gen.post$b_Grav_NearPop.max)
quantile(gen.post$b_Grav_NearPop.max, c(0.05, 0.95))

gen.post %>% 
  filter(b_Grav_NearPop.max < 0) %>% 
  nrow()
3921/4000 #draws are negative slope

#--------------------------------------
#competitive and market gravity
#summary(fit_competitive_brms)
#parnames(fit_competitive_brms)[1:20]

# pop.grav <- rstan::extract(fit_stresstolerant_brms, 
#                            pars = "b_Grav_NearPop.max")[[1]]
post <- as_tibble(as.data.frame(fit_competitive_brms)) %>% 
  select(b_Intercept:b_n_points)
head(post)
names(post)
nrow(post)
summary(post$b_Grav_Markets.max)
quantile(post$b_Grav_Markets.max, c(0.05, 0.95))

post %>% 
  filter(b_Grav_Markets.max < 0) %>% 
  nrow()
14424/16000 #draws are negative slope
#166000 draws from 4 chains x 4000 iterations
