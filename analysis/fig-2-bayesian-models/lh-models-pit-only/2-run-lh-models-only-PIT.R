#This script runs brms STAN models for life histories
#ONLY for PIT methods - reviewer 1
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))

#set up cores
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)

head(d.vars)
d.vars <- d.vars %>% 
  filter(Method == "PIT")
nrow(d.vars) #1628 rows of PIT
1628/2584

source(here("analysis", "bayesian-models", "2a-brms-competitive.R"))
source(here("analysis", "bayesian-models", "2a-brms-stresstolerant.R"))
source(here("analysis", "bayesian-models", "2a-brms-generalist.R"))
source(here("analysis", "bayesian-models", "2a-brms-weedy.R"))
source(here("analysis", "bayesian-models", "2a-brms-comp+st.R"))

#save models at RDS files for later analysis
# saveRDS(fit_competitive_brms,
#         here("analysis", "bayesian-models", "fit_competitive_brms.rds"))
# 
# saveRDS(fit_stresstolerant_brms,
#         here("analysis", "bayesian-models", "fit_stresstolerant_brms.rds"))
# 
# saveRDS(fit_generalist_brms,
#         here("analysis", "bayesian-models", "fit_generalist_brms.rds"))
# 
# saveRDS(fit_weedy_brms,
#         here("analysis", "bayesian-models", "fit_weedy_brms.rds"))
# 
# saveRDS(fit_comp_st_brms,
#         here("analysis", "bayesian-models", "fit_comp-st_brms.rds"))
# 
# saveRDS(fit_threshold,
#         here("analysis", "bayesian-models", "fit_threshold.rds"))


