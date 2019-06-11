#This script sets up brms multilevel model for stress-tolerant corals as test

#d.genus.models is centred-standardized dataset
names(d.genus.models)
summary(d.genus.models$perc_acropora)

d.genus.models$prop_acropora <- d.genus.models$perc_acropora / 100

#check response - use proportion for beta models
hist(d.genus.models$prop_acropora)
summary(d.genus.models$prop_acropora)

#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.genus.models$prop_acropora_transform <- y.transf.betareg(d.genus.models$prop_acropora)
hist(d.genus.models$prop_acropora_transform)
summary(d.genus.models$prop_acropora_transform)

#BAYESIAN MODELS
#fit brms model
fit_acropora_brms <- brm(prop_acropora_transform ~ past.maxDHW + years.sinceDHW + 
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
                      data = d.genus.models)

#summary(fit_acropora_brms)
#marginal_effects(fit_acropora_brms)
View(bayes_R2(fit_acropora_brms))

#model diagnostics
#launch_shinystan(fit_acropora_brms)

#posterior checks to save
#density overlay
color_scheme_set("red")
pp_check(fit_acropora_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "acropora-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.genus.models$prop_acropora
yrep <- posterior_predict(fit_acropora_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "acropora-ppcheck2.pdf"))

#--------------------------------------------------
#Extract credible intervals and export coef table
acropora.coef <- fixef(fit_acropora_brms, summary = TRUE, 
                    probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var") %>% 
  mutate(lh = "acropora")
acropora.coef

write.csv(acropora.coef, here("analysis", "outputs", 
                           "coef-tables", "coef-acropora.csv"), 
          row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_acropora_intercept <- brm(prop_acropora_transform ~ 1 + 
#                                   (1|Province) + (1|Country) + (1|socialsite),
#                                 family = Beta(link = "logit"),
#                                 chains = 2, #default niter = 2000, 1000 warmup, 1000 sampling
#                                 data = d.genus.models)
# waic(fit_acropora_intercept)
# bayes_R2(fit_acropora_intercept)
# 
# waic(fit_acropora_intercept, fit_acropora_brms)


