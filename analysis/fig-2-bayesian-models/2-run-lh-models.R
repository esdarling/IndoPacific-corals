#This script runs brms STAN models for life histories
library(here)
here()
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))

#set up cores
options(mc.cores = parallel::detectCores())
#options(mc.cores = 1)

#set priors
beta.priors <- c(set_prior("student_t(3,0,25)", class = "phi"), 
                 set_prior("normal(0,2)", class = "b"),
                 set_prior("normal(0,10)", class = "Intercept"))

# bernouilli.priors <- c(set_prior("student_t(3,0,25)", class = "phi"), 
#                        set_prior("normal(0,2)", class = "b"),
#                        set_prior("normal(0,10)", class = "Intercept"))

source(here("analysis", "bayesian-models", "2a-brms-00-sumcover.R"))
source(here("analysis", "bayesian-models", "2a-brms-01-competitive.R"))
source(here("analysis", "bayesian-models", "2a-brms-02-stresstolerant.R"))
source(here("analysis", "bayesian-models", "2a-brms-03-generalist.R"))
source(here("analysis", "bayesian-models", "2a-brms-04-weedy.R"))

prior_summary(fit_weedy_brms)

#set bernoulli priors
#source(here("analysis", "bayesian-models", "2a-brms-05-comp+st.R"))


#save models at RDS files for later analysis
#RDS model files stored in Dropbox folder for large file size
DROPBOX <- file.path("/Users/emilydarling/Dropbox/1-On the go", 
                     "Coral Database", "GLOBAL CORAL PAPERS",
                     "Paper1 - IP coral communities", "Paper", 
                     "Submission3-REVISION-NatureEcoEvo", 
                     "RDS-models")

# saveRDS(fit_sumcover_brms,
#         file.path(DROPBOX, "fit_sumcover.rds"))
# 
# saveRDS(fit_competitive_brms,
#         file.path(DROPBOX, "fit_competitive_brms.rds"))
# 
# saveRDS(fit_stresstolerant_brms,
#         file.path(DROPBOX, "fit_stresstolerant_brms.rds"))
# 
# saveRDS(fit_generalist_brms,
#         file.path(DROPBOX, "fit_generalist_brms.rds"))
# 
# saveRDS(fit_weedy_brms,
#         file.path(DROPBOX, "fit_weedy_brms.rds"))

# saveRDS(fit_comp_st_brms,
#         file.path(DROPBOX, "fit_comp-st_brms.rds"))
# 
# saveRDS(fit_threshold,
#         file.path(DROPBOX, "fit_threshold.rds"))

saveRDS(fit_competitive_brms_5000,
        here("fit_competitive_brms_5000.rds"))

saveRDS(fit_stresstolerant_brms_5000,
        here("fit_stresstolerant_brms.rds"))


