#This script sets up brms multilevel model for stress-tolerant corals as test

#d.vars is centred-standardized dataset
names(d.vars)
summary(d.vars$perc_weedy)

d.vars$prop_weedy <- d.vars$perc_weedy / 100

#check response - use proportion for beta models
hist(d.vars$prop_weedy)
summary(d.vars$prop_weedy)


#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.vars$prop_weedy_transform <- y.transf.betareg(d.vars$prop_weedy)
hist(d.vars$prop_weedy_transform)
summary(d.vars$prop_weedy_transform)

#BAYESIAN MODELS
#fit brms model
fit_weedy_brms <- brm(prop_weedy_transform ~ past.maxDHW + years.sinceDHW + 
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

#summary(fit_weedy_brms)
#marginal_effects(fit_weedy_brms)
View(bayes_R2(fit_weedy_brms))

#model diagnostics
#launch_shinystan(fit_weedy_brms)

# #posterior checks to save
# #density overlay
color_scheme_set("green")
pp_check(fit_weedy_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "weedy-ppcheck.pdf"))

# #histogram with manually setting variables
y <- d.vars$prop_weedy
yrep <- posterior_predict(fit_weedy_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "weedy-ppcheck2.pdf"))

#--------------------------------------------------
#Extract credible intervals and export coef table
weedy.coef <- fixef(fit_weedy_brms, summary = TRUE, 
                         probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var") %>% 
  mutate(lh = "weedy")
weedy.coef

write.csv(weedy.coef, here("analysis", "outputs",
                           "coef-tables", "coef-weedy.csv"),
          row.names = FALSE)


# write.csv(weedy.coef, here("analysis", "outputs", 
#                            "coef-tables", "coef-weedy-pit.csv"), 
#           row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_weedy_intercept <- brm(prop_weedy_transform ~ 1 + 
#                                   (1|Province) + (1|Country) + (1|socialsite),
#                                 family = Beta(link = "logit"),
#                                 chains = 2, #default niter = 2000, 1000 warmup, 1000 sampling
#                                 data = d.vars)
# waic(fit_weedy_intercept)
# bayes_R2(fit_weedy_intercept)
# 
# waic(fit_weedy_intercept, fit_weedy_brms)

# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_weedy_brms)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: weedy") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-weedy.pdf"))



