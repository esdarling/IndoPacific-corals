#This script sets up brms multilevel model for competitive corals as test
#citation()
#sessionInfo()


#d.vars is centred-standardized dataset
names(d.vars)
nrow(d.vars)

summary(d.vars$perc_competitive)

#check response - use proportion for beta models
d.vars$prop_competitive <- d.vars$perc_competitive / 100

summary(d.vars$prop_competitive)
hist(d.vars$prop_competitive)

#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.vars$prop_competitive_transform <- y.transf.betareg(d.vars$prop_competitive)
hist(d.vars$prop_competitive_transform)
summary(d.vars$prop_competitive_transform)

#get priors
# get_prior(prop_competitive_transform ~ 
#     past.maxDHW + years.sinceDHW + 
#     Grav_NearPop.max + Grav_Markets.max +
#     perc_crop2012 + perc_crop2002.2012 +
#     hdi2015 + 
#     Management + Habitat + 
#     Depth_m + npp_mean + 
#     wave_mean + maxTCdays.mean + 
#     reef_area_100km + Latitude + 
#     #Method + 
#     n_points + 
#     (1|Province) + (1|Country) + (1|socialsite),
#     family = Beta(link = "logit"),
#   data = d.vars)

#set priors
# beta.priors <- c(set_prior("student_t(3,0,25)", class = "phi"), 
#                  set_prior("normal(0,2)", class = "b"),
#                  set_prior("normal(0,10)", class = "Intercept"))
                      
#check priors are into STAN code
## verify that the priors indeed found their way into Stan's model code
#make_stancode(prop_competitive_transform ~ 
              #   past.maxDHW + years.sinceDHW + 
              #   Grav_NearPop.max + Grav_Markets.max +
              #   perc_crop2012 + perc_crop2002.2012 +
              #   hdi2015 + 
              #   Management + Habitat + 
              #   Depth_m + npp_mean + 
              #   wave_mean + maxTCdays.mean + 
              #   reef_area_100km + Latitude + 
              #   Method + 
              #   n_points + 
              #   (1|Province) + (1|Country) + (1|socialsite),
              # family = Beta(link = "logit"),
              # prior = beta.priors,
              # control = list(adapt_delta = 0.9),
              # data = d.vars)

#BAYESIAN MODELS
#fit brms model
?brm
fit_competitive_brms <- brm(prop_competitive_transform ~ 
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
                     chains = 4, #default iter = 2000, 1000 warmup, 1000 sampling
                     data = d.vars)

summary(fit_competitive_brms) #Rhat, neff, estimates
#marginal_effects(fit_competitive_brms) #plots of parameters

View(bayes_R2(fit_competitive_brms))
#copy into Excel 

#model diagnostics
#launch_shinystan(fit_competitive_brms)

#posterior checks to save
#density overlay
color_scheme_set("red")
pp_check(fit_competitive_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "comp-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.vars$prop_competitive
yrep <- posterior_predict(fit_competitive_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "comp-ppcheck2.pdf"))


#--------------------------------------------------
#Extract credible intervals and export coef table
competitive.coef <- fixef(fit_competitive_brms, summary = TRUE, 
                  probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var")  %>% 
  mutate(lh = "competitive")
competitive.coef

write.csv(competitive.coef, here("analysis", "outputs",
                                 "coef-tables", "coef-competitive.csv"),
          row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_competitive_intercept <- brm(prop_competitive_transform ~ 1 + 
#                        (1|Province) + (1|Country) + (1|socialsite),
#                      family = Beta(link = "logit"),
#                      chains = 4, #default niter = 2000, 1000 warmup, 1000 sampling
#                      data = d.vars)
# #waic(fit_competitive_intercept)
# bayes_R2(fit_competitive_intercept)
# 
# waic(fit_competitive_intercept, fit_competitive_brms)


# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_competitive_brms)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: competitive") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-competitive.pdf"))

# #check out predictions <10%
# ## posterior predictive checks
# pp <- predict(fit_competitive_brms)
# head(pp)
# nrow(pp)
# 
# pp <- as.tibble(pp)
# head(pp)
# summary(pp$Estimate)
# 
# nrow(filter(pp, Estimate<0.10)) / 2584
# #more than half under 50% threshold

#--------------------------------------------------
#run model with more posteriors for better predictions
fit_competitive_brms_5000 <- brm(prop_competitive_transform ~ 
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
color_scheme_set("red")
pp_check(fit_competitive_brms_5000, nsamples = 10)
ggsave(here("analysis", "outputs", "comp-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.vars$prop_competitive
yrep <- posterior_predict(fit_competitive_brms_5000, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "comp-ppcheck2.pdf"))


#--------------------------------------------------
#Extract credible intervals and export coef table
competitive.coef <- fixef(fit_competitive_brms_5000, summary = TRUE, 
                          probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var")  %>% 
  mutate(lh = "competitive-5000")
competitive.coef

write.csv(competitive.coef, here("analysis", "outputs",
                                 "coef-tables", "coef-competitive-5000.csv"),
          row.names = FALSE)
#--------------------------------------------------

# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_competitive_brms_5000)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: competitive") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-competitive.pdf"))
