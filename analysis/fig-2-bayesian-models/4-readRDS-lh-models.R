#This file loads RDS models
#possible code duplicate, can't find others

#RDS model files stored in Dropbox folder for large file size
DROPBOX <- file.path("/Users/emilydarling/Dropbox/1-On the go", 
                     "Coral Database", "GLOBAL CORAL PAPERS",
                     "Paper1 - IP coral communities", "Paper", 
                     "previous submissions",
                     "NEE-REVISION-March2019", 
                     "RDS-models")

fit_competitive_brms <- readRDS(file.path(DROPBOX, "fit_competitive_brms_5000.rds"))

fit_stresstolerant_brms <- readRDS(file.path(DROPBOX, "fit_stresstolerant_brms_5000.rds"))

# fit_generalist_brms <- readRDS(file.path(DROPBOX,"fit_generalist_brms.rds"))
# 
# fit_weedy_brms <- readRDS(file.path(DROPBOX,"fit_weedy_brms.rds"))
# 
# fit_comp_st_brms <- readRDS(file.path(DROPBOX,"fit_comp-st_brms.rds"))
# 
# fit_threshold <- readRDS()ile.path(DROPBOX,"fit_threshold.rds"))

# fit_sumcover_brms <- readRDS(file.path(DROPBOX,"fit_sumcover.rds"))
# summary(fit_sumcover_brms)
# prior_summary(fit_sumcover_brms)

