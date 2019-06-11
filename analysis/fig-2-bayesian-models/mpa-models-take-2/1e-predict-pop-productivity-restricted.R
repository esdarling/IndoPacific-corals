#This file sets up a dummy dataframe for brms prediction
# library(here)
# source(here("analysis", "00-source.R"))
# source(here("analysis", "bayesian-models", "1-centre-variables.R"))
# source(here("analysis", "bayesian-models", "4-readRDS-lh-models.R"))

#readRDS of two key models
#summary(fit_competitive_brms)
#summary(fit_stresstolerant_brms)

#d.vars is original data for model fits
head(d.vars)
names(d.vars)

#practice predictions
#choose two variables

var1 <- "Grav_NearPop.max"
var2 <- "npp_mean"

summary(d.vars[,var1])
summary(d.vars[,var2])

x_min <- -0.3;
x_max <- 2.25;
N_x <- 50; #number of values in the x covariate
x <- seq(x_min,x_max,length.out=N_x)
x

y_min <- -1.15;
y_max <- 1.7;
N_y <- 50; # number of values in the y covariate
y <- seq(y_min,y_max,length.out=N_y)
y

X <- rep(x, N_y)
Y <- c(); for(i in 1:N_x) Y<-c(Y,rep(y[i],N_x))

X
Y

# data frame that has all the X and Y combinations
dummy_data <- data.frame(X,Y) %>% 
  as_tibble() %>% 
  rename("Grav_NearPop.max" = X, 
         "npp_mean" = Y) 
head(dummy_data)

# for the other drivers, set them always at zero
# for categorical, set at base level
d.newdf <- dummy_data %>% 
  mutate(past.maxDHW = 0, 
         years.sinceDHW = 0, 
         #Grav_NearPop.max = 0, 
         Grav_Markets.max = 0, 
         perc_crop2012 = 0,
         perc_crop2002.2012 = 0, 
         hdi2015 = 0, 
         Management = "Restricted",
         Habitat = "Slope",
         Depth_m = 0, 
         #npp_mean = 0, 
         wave_mean = 0, 
         maxTCdays.mean = 0, 
         reef_area_100km = 0, 
         Latitude = 0, 
         Method = "PIT", 
         n_points = 0)
d.newdf         
nrow(d.newdf)
#(1|Province) + (1|Country) + (1|socialsite),

#predict competitive model over newdf
#SUMMARY = FALSE, all 4000 chains provided
comp <- predict(fit_competitive_brms, newdata = d.newdf, 
                summary = FALSE,
                #robust = TRUE, 
                nsamples = 10000,
                re_formula = NA) %>% #avg group-level random effects
  as_tibble() %>% 
  gather(run, predict.comp) %>% 
  mutate(order = as.numeric(str_extract(run, "\\d+"))) %>% 
  group_by(order, run) %>% 
  arrange(order) %>% 
  dplyr::summarise(comp.median = median(predict.comp))
comp

#Repeat for stress tolerant
st <- predict(fit_stresstolerant_brms, newdata = d.newdf, 
              summary = FALSE, 
              #robust = TRUE, 
              nsamples = 10000,
              re_formula = NA) %>% #avg group-level random effects
  as_tibble() %>% 
  gather(run, predict.st) %>% 
  mutate(order = as.numeric(str_extract(run, "\\d+"))) %>% 
  group_by(order, run) %>% 
  arrange(order) %>% 
  dplyr::summarise(st.median = median(predict.st))
st

#add comp and st together
comp.st <- comp %>% 
  bind_cols(st) %>% 
  #mutate(comp.st = predict.comp + predict.st) 
  mutate(median = comp.median + st.median) 

length(unique(comp.st$order))

summary(comp.st$median)

#join back into d.newdf
pop.prod <- d.newdf %>% 
  select(1:2) %>% 
  #bind_cols(test[,c("median", "Q25", "Q75")]) %>% 
  bind_cols(comp.st[,c("median")]) %>% 
  mutate(threshold.median = as.factor(ifelse(median >= 0.1, "above", "below"))) 

summary(pop.prod$median)
table(pop.prod$threshold.median)
pop.prod

#backtransform variables to true values
names(d.models)
var1 <- "Grav_NearPop.max"
var2 <- "npp_mean"

#d.models is raw values
summary(d.models[,var1])
summary(d.models[,var2])

#gravity is logged
mean.var1 <- mean(log(d.models[,var1]+ 1))
mean.var2 <- mean(d.models[,var2])

sd.var1 <- sd(log(d.models[,var1]+ 1))
sd.var2 <- sd(d.models[,var2])


pop.prod <- pop.prod %>% 
  mutate(Grav_NearPop.max.raw = (Grav_NearPop.max * 2*sd.var1) + mean.var1, 
         npp_mean.raw = (npp_mean * 2*sd.var2) + mean.var2)

summary(pop.prod$Grav_NearPop.max.raw)
summary(pop.prod$npp_mean.raw)
summary(pop.prod$median)

table(markets.prod$threshold.median)

#ggplot test - adjust print size to fit
pop.prod

#try distance variable for shading
pop.prod <- pop.prod %>% 
  mutate(dist = median - 0.1, 
         median.colour = ifelse(median < 0.1, 0, median))
summary(pop.prod$dist)
summary(pop.prod$median.colour)

gg.pop.prod.restricted <- ggplot(data = pop.prod, 
                         aes(x = Grav_NearPop.max.raw, y = npp_mean.raw)) + 
  geom_point(aes(fill = median.colour,
                 colour = median.colour, 
                 alpha = median.colour), 
             alpha = 1, 
             #colour = "black",
             stroke = 0.25,
             shape = 22, size = 1) + 
  theme_sleek(base_size = 10) + 
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_colour_distiller(palette = "Spectral", direction = 1) +
  # scale_colour_continuous_sequential(palette = "Plasma", rev = FALSE) +
  # scale_fill_continuous_sequential(palette = "Plasma", rev = FALSE) +
  xlab("Population gravity") +
  ylab("Primary productivity") + 
  theme(legend.position = "none") #+ 
#coord_flip()

gg.pop.prod.restricted




