#This script sets up brms multilevel model for stress-tolerant corals

#d.vars is centred-standardized dataset
names(d.vars)
summary(d.vars$perc_stresstolerant)

d.vars$prop_stresstolerant <- d.vars$perc_stresstolerant / 100
summary(d.vars$prop_stresstolerant)

#check response - use proportion for beta models
hist(d.vars$prop_stresstolerant)
summary(d.vars$prop_stresstolerant)
skim(d.vars)

#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.vars$prop_stresstolerant_transform <- y.transf.betareg(d.vars$prop_stresstolerant)
hist(d.vars$prop_stresstolerant_transform)
summary(d.vars$prop_stresstolerant_transform)

#BAYESIAN MODELS
#fit brms model
fit_stresstolerant_brms <- brm(prop_stresstolerant_transform ~ past.maxDHW + years.sinceDHW + 
                              Grav_NearPop.max + Grav_Markets.max +
                              perc_crop2012 + perc_crop2002.2012 +
                              hdi2015 + 
                              Management + Habitat + 
                              Depth_m + npp_mean + 
                              wave_mean + maxTCdays.mean + 
                              reef_area_100km + Latitude + 
                              Method + 
                              n_points + 
                              (1|Province) + (1|Country) + (1|socialsite),
                            family = Beta(link = "logit"),
                            prior = beta.priors,
                            control = list(adapt_delta = 0.9),
                            chains = 4, #default niter = 2000, 1000 warmup, 1000 sampling
                            data = d.vars)

# summary(fit_stresstolerant_brms)
# #marginal_effects(fit_stresstolerant_brms)
# 
View(bayes_R2(fit_stresstolerant_brms))
# 
# #model diagnostics
# #launch_shinystan(fit_stresstolerant_brms)
# 
# #posterior checks to save
# #density overlay
color_scheme_set("blue")
pp_check(fit_stresstolerant_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "st-ppcheck.pdf"))
# 
# #histogram with manually setting variables
y <- d.vars$prop_stresstolerant
yrep <- posterior_predict(fit_stresstolerant_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "st-ppcheck2.pdf"))

#--------------------------------------------------
#Extract credible intervals and export coef table
stresstolerant.coef <- fixef(fit_stresstolerant_brms, summary = TRUE, 
                          probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var") %>% 
  mutate(lh = "stresstolerant")
stresstolerant.coef

write.csv(stresstolerant.coef, here("analysis", "outputs",
                                    "coef-tables", "coef-stresstolerant.csv"),
          row.names = FALSE)

# write.csv(stresstolerant.coef, here("analysis", "outputs", 
#                                     "coef-tables", "coef-stresstolerant-pit.csv"), 
#           row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_stresstolerant_intercept <- brm(prop_stresstolerant_transform ~ 1 + 
#                                    (1|Province) + (1|Country) + (1|socialsite),
#                                  family = Beta(link = "logit"),
#                                  chains = 2, #default niter = 2000, 1000 warmup, 1000 sampling
#                                  data = d.vars)
# bayes_R2(fit_stresstolerant_intercept)
# 
# waic(fit_stresstolerant_intercept, fit_stresstolerant_brms)

# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_stresstolerant_brms)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: stress-tolerant") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-stresstolerant.pdf"))

# #check out predictions <10%
# ## posterior predictive checks
# pp <- predict(fit_stresstolerant_brms)
# pp <- as.tibble(pp)
# head(pp)
# summary(pp$Estimate)
# 
# nrow(filter(pp, Estimate<0.10)) / 2584
# #more than half under 50% threshold from competitive
# #only 9.4% under 10% cover for combined comp + st

#--------------------------------------------------
#run model with more posteriors for better predictions
?brm
fit_stresstolerant_brms_5000 <- brm(prop_stresstolerant_transform ~ 
                                   past.maxDHW + years.sinceDHW + 
                                   Grav_NearPop.max + Grav_Markets.max +
                                   perc_crop2012 + perc_crop2002.2012 +
                                   hdi2015 + 
                                   Management + Habitat + 
                                   Depth_m + npp_mean + 
                                   wave_mean + maxTCdays.mean + 
                                   reef_area_100km + Latitude + 
                                   Method + 
                                   n_points + 
                                   (1|Province) + (1|Country) + (1|socialsite),
                                 family = Beta(link = "logit"),
                                 prior = beta.priors,
                                 control = list(adapt_delta = 0.9),
                                 chains = 4, #default niter = 2000, 1000 warmup, 1000 sampling
                                 iter = 5000, 
                                 warmup = 1000,
                                 data = d.vars)

#posterior checks to save
#density overlay
color_scheme_set("blue")
pp_check(fit_stresstolerant_brms_5000, nsamples = 10)
ggsave(here("analysis", "outputs", "st-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.vars$prop_stresstolerant
yrep <- posterior_predict(fit_stresstolerant_brms_5000, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "st-ppcheck2.pdf"))


#--------------------------------------------------
#Extract credible intervals and export coef table
st.coef <- fixef(fit_stresstolerant_brms_5000, summary = TRUE, 
                          probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var")  %>% 
  mutate(lh = "stresstolerant-5000")
st.coef

write.csv(st.coef, here("analysis", "outputs",
                                 "coef-tables", "coef-stresstolerant-5000.csv"),
          row.names = FALSE)
#--------------------------------------------------


# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_stresstolerant_brms_5000)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: stress-tolerant") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-stresstolerant.pdf"))
