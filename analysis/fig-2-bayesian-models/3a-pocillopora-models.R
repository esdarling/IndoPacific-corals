#This script sets up brms multilevel model for stress-tolerant corals as test

#d.genus.models is centred-standardized dataset
names(d.genus.models)
summary(d.genus.models$perc_pocillopora)

d.genus.models$prop_pocillopora <- d.genus.models$perc_pocillopora / 100

#check response - use proportion for beta models
hist(d.genus.models$prop_pocillopora)
summary(d.genus.models$prop_pocillopora)


#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.genus.models$prop_pocillopora_transform <- y.transf.betareg(d.genus.models$prop_pocillopora)
hist(d.genus.models$prop_pocillopora_transform)
summary(d.genus.models$prop_pocillopora_transform)

#BAYESIAN MODELS
#fit brms model
fit_pocillopora_brms <- brm(prop_pocillopora_transform ~ past.maxDHW + years.sinceDHW + 
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

#summary(fit_pocillopora_brms)
#marginal_effects(fit_pocillopora_brms)
View(bayes_R2(fit_pocillopora_brms))

#model diagnostics
#launch_shinystan(fit_pocillopora_brms)

#posterior checks to save
#density overlay
?color_scheme_set
color_scheme_set("teal")
pp_check(fit_pocillopora_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "pocillopora-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.genus.models$prop_pocillopora
yrep <- posterior_predict(fit_pocillopora_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "pocillopora-ppcheck2.pdf"))

#--------------------------------------------------
#Extract credible intervals and export coef table
pocillopora.coef <- fixef(fit_pocillopora_brms, summary = TRUE, 
                        probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var") %>% 
  mutate(lh = "pocillopora")
pocillopora.coef

write.csv(pocillopora.coef, here("analysis", "outputs", 
                               "coef-tables", "coef-pocillopora.csv"), 
          row.names = FALSE)
#--------------------------------------------------

#compare WAIC to intercept-only model 
# fit_pocillopora_intercept <- brm(prop_pocillopora_transform ~ 1 + 
#                                   (1|Province) + (1|Country) + (1|socialsite),
#                                 family = Beta(link = "logit"),
#                                 chains = 2, #default niter = 2000, 1000 warmup, 1000 sampling
#                                 data = d.genus.models)
# waic(fit_pocillopora_intercept)
# bayes_R2(fit_pocillopora_intercept)
# 
# waic(fit_pocillopora_intercept, fit_pocillopora_brms)


