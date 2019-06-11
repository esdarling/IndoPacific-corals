#This file sets up a dummy dataframe for brms prediction
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "1-centre-variables.R"))
source(here("analysis", "bayesian-models", "4-readRDS-lh-models.R"))

#readRDS of two key models
#summary(fit_competitive_brms)
#summary(fit_stresstolerant_brms)

#d.vars is original data for model fits
head(d.vars)
names(d.vars)

#practice predictions
#choose two variables

var1 <- "years.sinceDHW"
var2 <- "Grav_Markets.max"

summary(d.vars[,var1])
summary(d.vars[,var2])

x_min <- -0.7;
x_max <- 0.9;
N_x <- 50; #number of values in the x covariate
x <- seq(x_min,x_max,length.out=N_x)
x

y_min <- -0.4;
y_max <- 2.2;
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
  rename("years.sinceDHW" = X, 
         "Grav_Markets.max" = Y) 
head(dummy_data)

# for the other drivers, set them always at zero
# for categorical, set at base level
d.newdf <- dummy_data %>% 
  mutate(past.maxDHW = 0, 
         #years.sinceDHW = 0, 
         Grav_NearPop.max = 0, 
         #Grav_Markets.max = 0, 
         perc_crop2012 = 0,
         perc_crop2002.2012 = 0, 
         hdi2015 = 0, 
         Management = "Fished",
         Habitat = "Slope",
         Depth_m = 0, 
         npp_mean = 0, 
         wave_mean = 0, 
         maxTCdays.mean = 0, 
         reef_area_100km = 0, 
         Latitude = 0, 
         Method = "PIT", 
         n_points = 0)
d.newdf         
nrow(d.newdf)
#(1|Province) + (1|Country) + (1|socialsite),
#Remove random effects - idealized reef; no need for spatial arrangement

#predict competitive model over newdf
#summary(fit_competitive_brms)

#with more iterations
#16,000 iterations
#10,000 combinations
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

# test <- comp.st %>% 
#   group_by(order) %>% 
#   dplyr::summarize(median = median(comp.st), 
#             Q25 = quantile(comp.st, 0.25),
#             Q75 = quantile(comp.st, 0.75))
# test

#join back into d.newdf
yearsdhw.markets <- d.newdf %>% 
  select(1:2) %>% 
  #bind_cols(test[,c("median", "Q25", "Q75")]) %>% 
  bind_cols(comp.st[,c("median")]) %>% 
  mutate(threshold.median = as.factor(ifelse(median >= 0.1, "above", "below")))#, 
         #dist.threshold = comp.st.median - 0.1)
         #threshold.Q25 = as.factor(ifelse(Q25 >= 0.1, "above", "below"))) 

summary(yearsdhw.markets$median)
#summary(yearsdhw.markets$Q25)
#summary(yearsdhw.markets$Q75)

#backtransform variables to true values
var1 <- "years.sinceDHW"
var2 <- "Grav_Markets.max"

#d.models is raw values
summary(d.models[,var1])
summary(d.models[,var2])

mean.var1 <- mean(d.models[,var1])
mean.var2 <- mean(log(d.models[,var2]+ 1))

sd.var1 <- sd(d.models[,var1])
sd.var2 <- sd(log(d.models[,var2]+ 1))
  
yearsdhw.markets <- yearsdhw.markets %>% 
  mutate(years.sinceDHW.raw = (years.sinceDHW * 2*sd.var1) + mean.var1, 
         Grav_Markets.max.raw = (Grav_Markets.max * 2*sd.var2) + mean.var2)

summary(yearsdhw.markets$years.sinceDHW.raw)
summary(yearsdhw.markets$Grav_Markets.max.raw)
summary(yearsdhw.markets$median)

table(yearsdhw.markets$threshold.median)
#table(yearsdhw.markets$threshold.Q25)

#ggplot test - adjust print size to fit
yearsdhw.markets

#try distance variable for shading
yearsdhw.markets <- yearsdhw.markets %>% 
  mutate(dist = median - 0.1, 
         median.colour = ifelse(median < 0.1, 0, median))
summary(yearsdhw.markets$dist)
summary(yearsdhw.markets$median.colour)

#install.packages("colorspace")
library(colorspace)

gg.yearsdhw.markets <- ggplot(data = yearsdhw.markets, aes(x = years.sinceDHW.raw, y = Grav_Markets.max.raw)) + 
  geom_point(aes(fill = median.colour,
                 colour = median.colour), 
             alpha = 1, 
             #colour = "black",
             stroke = 0.25,
             shape = 22, size = 1) + 
  theme_sleek(base_size = 10) + 
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_colour_distiller(palette = "Spectral", direction = 1) +
  # scale_colour_continuous_sequential(palette = "Plasma", rev = FALSE) +
  # scale_fill_continuous_sequential(palette = "Plasma", rev = FALSE) +
  xlab("Years since max DHW") +
  ylab("Market gravity") + 
  theme(legend.position = "none") #+ 
  #coord_flip()
gg.yearsdhw.markets

# ggsave(here("analysis", "outputs", "figs", 
#             "fig3-yearsDHW-markets.pdf"), 
#        height = 2.5, width = 3)









