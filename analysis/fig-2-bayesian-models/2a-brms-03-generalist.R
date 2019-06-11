#This script sets up brms multilevel model for stress-tolerant corals as test

#d.vars is centred-standardized dataset
names(d.vars)
summary(d.vars$perc_generalist)

d.vars$prop_generalist <- d.vars$perc_generalist / 100

#check response - use proportion for beta models
hist(d.vars$prop_generalist)
summary(d.vars$prop_generalist)

#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.vars$prop_generalist_transform <- y.transf.betareg(d.vars$prop_generalist)
hist(d.vars$prop_generalist_transform)
summary(d.vars$prop_generalist_transform)

#BAYESIAN MODELS
#fit brms model
fit_generalist_brms <- brm(prop_generalist_transform ~ past.maxDHW + years.sinceDHW + 
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

#summary(fit_generalist_brms)
#marginal_effects(fit_generalist_brms)

#waic(fit_generalist_brms)
#View(bayes_R2(fit_generalist_brms))
#copy into Excel 
#?bayes_R2

#model diagnostics
#launch_shinystan(fit_generalist_brms)

# #posterior checks to save
# #density overlay
color_scheme_set("orange")
pp_check(fit_generalist_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "generalist-ppcheck.pdf"))

# #histogram with manually setting variables
y <- d.vars$prop_generalist
yrep <- posterior_predict(fit_generalist_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "generalist-ppcheck2.pdf"))

#--------------------------------------------------
#Extract credible intervals and export coef table
generalist.coef <- fixef(fit_generalist_brms, summary = TRUE, 
                             probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var") %>% 
  mutate(lh = "generalist")
generalist.coef

write.csv(generalist.coef, here("analysis", "outputs",
                                "coef-tables", "coef-generalist.csv"),
          row.names = FALSE)
# 
# write.csv(generalist.coef, here("analysis", "outputs", 
#                                 "coef-tables", "coef-generalist-pit.csv"), 
#           row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_generalist_intercept <- brm(prop_generalist_transform ~ 1 + 
#                                       (1|Province) + (1|Country) + (1|socialsite),
#                                     family = Beta(link = "logit"),
#                                     chains = 2, #default niter = 2000, 1000 warmup, 1000 sampling
#                                     data = d.vars)
# waic(fit_generalist_intercept)
# bayes_R2(fit_generalist_intercept)
# 
# waic(fit_generalist_intercept, fit_generalist_brms)

# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_generalist_brms)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: generalist ") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-generalist.pdf"))


