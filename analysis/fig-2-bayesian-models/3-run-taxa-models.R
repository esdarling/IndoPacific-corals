#This script runs brms STAN models for coral genera
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "life-histories", "1-CommComposition.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))

#pull top genera into d.vars dataset
head(d.genus.wide)

d.genus.models <- d.genus.wide %>% 
  select(Source, Site, 
         Acropora, Porites, Montipora, Pocillopora, 
         Stylophora) %>% 
  as.tibble()
head(d.genus.models)
nrow(d.genus.models)

#join to d.vars
head(as.tibble(d.vars))

d.genus.models <- left_join(d.genus.models, d.vars, 
                            by = c("Source", "Site")) %>% 
  as.tibble() %>% 
  rename(perc_acropora = "Acropora", 
         perc_porites = "Porites", 
         perc_montipora = "Montipora", 
         perc_pocillopora = "Pocillopora",
         perc_stylophora = "Stylophora")
head(d.genus.models)
#not sure why 2429 sites, but gonna roll with it for now

#set up cores
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)

beta.priors <- c(set_prior("student_t(3,0,25)", class = "phi"), 
                 set_prior("normal(0,2)", class = "b"),
                 set_prior("normal(0,10)", class = "Intercept"))

#Source the top-4 dominant genera
# Acropora
source(here("analysis", "bayesian-models", "3a-Acropora-models.R"))
prior_summary(fit_acropora_brms)

# Porites
source(here("analysis", "bayesian-models", "3a-porites-models.R"))

# Montipora
source(here("analysis", "bayesian-models", "3a-montipora-models.R"))

# Pocillopora
source(here("analysis", "bayesian-models", "3a-pocillopora-models.R"))
prior_summary(fit_pocillopora_brms)

# Stylophora
#source(here("analysis", "bayesian-models", "3a-stylophora-models.R"))


