#This script sets up brms multilevel model for sumcover corals as test
#citation()
#sessionInfo()

#d.vars is centred-standardized dataset
names(d.vars)
nrow(d.vars)

summary(d.vars$sumcover)

#check response - use proportion for beta models
d.vars$prop_sumcover <- d.vars$sumcover / 100

summary(d.vars$prop_sumcover)
hist(d.vars$prop_sumcover)

#beta regression may need transformation
#transformation for 0s..adds a small amount to 0s for models to run
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

d.vars$prop_sumcover_transform <- y.transf.betareg(d.vars$prop_sumcover)
hist(d.vars$prop_sumcover_transform)
summary(d.vars$prop_sumcover_transform)

#BAYESIAN MODELS
#fit brms model
fit_sumcover_brms <- brm(prop_sumcover_transform ~ past.maxDHW + years.sinceDHW + 
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

#summary(fit_sumcover_brms) #Rhat, neff, estimates
#marginal_effects(fit_sumcover_brms) #plots of parameters

View(bayes_R2(fit_sumcover_brms))
#copy into Excel 

#model diagnostics
#launch_shinystan(fit_sumcover_brms)

#posterior checks to save
#density overlay
color_scheme_set("red")
pp_check(fit_sumcover_brms, nsamples = 10)
ggsave(here("analysis", "outputs", "sumcover-ppcheck.pdf"))

#histogram with manually setting variables
y <- d.vars$prop_sumcover
yrep <- posterior_predict(fit_sumcover_brms, draws = 500)
ppc_hist(y, yrep[1:5,])
ggsave(here("analysis", "outputs", "sumcover-ppcheck2.pdf"))


#--------------------------------------------------
#Extract credible intervals and export coef table
sumcover.coef <- fixef(fit_sumcover_brms, summary = TRUE, 
                          probs = c(0.05, 0.1, 0.2, 0.8, 0.9, 0.95)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var")  %>% 
  mutate(lh = "sumcover")
sumcover.coef

write.csv(sumcover.coef, here("analysis", "outputs",
                                 "coef-tables", "coef-sumcover.csv"),
          row.names = FALSE)

#--------------------------------------------------
# #save chain mcmc_trace
color_scheme_set("mix-brightblue-gray")
posterior_cp <- as.array(fit_sumcover_brms)
mcmc_trace(posterior_cp, pars = "b_Intercept") +
  xlab("Post-warmup iteration") +
  ggtitle("Trace plot: total % hard coral cover") +
  theme_sleek(base_size = 16)
ggsave(here("analysis", "outputs", "mcmc-trace-sumcover.pdf"))

# #check out predictions <10%
# ## posterior predictive checks
# pp <- predict(fit_sumcover_brms)
# head(pp)
# nrow(pp)
# 
# pp <- as.tibble(pp)
# head(pp)
# summary(pp$Estimate)
# 
# nrow(filter(pp, Estimate<0.10)) / 2584
# #more than half under 50% threshold
