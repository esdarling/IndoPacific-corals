library(tidyverse)
library(reshape2)
library(car)
library(gdata)
library(gridExtra)
library(grid)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(data.table)
library(scales)
library(fields)
library(stringr)
library(here)
library(hclust)

install.packages("hclust")
library(dismo)
citation("dismo")
citation("hclust")

DROPBOX <- ("/Users/emilydarling/Dropbox/1-On the go/Coral Database/GLOBAL CORAL PAPERS/Paper1 - IP coral communities")
PROJHOME

## ================================================================
#load most recent dataset
#saved from Fig 3 earlier R version
#with all drivers, refuge, etc. 
bind <- read.csv(here("Fig3 bind data-3July2018.csv"), 
                      header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
names(bind)
head(bind)                 
                 
bind$strategy <- factor(bind$strategy, 
                        levels = c("refuge", "recover", "transform"))

##------------------------------------
#PORTFOLIO
unique(bind$refuge)
unique(bind$strategy)

#think about gravity for portfolios 
names(bind)
summary(bind$Grav_Markets.max)

bind$log.Grav_Markets.max <- log(bind$Grav_Markets.max + 1)
summary(bind$log.Grav_Markets.max)
quantile <- quantile(bind$Grav_Markets.max, probs = c(0.2, 0.8))
quantile[1]
quantile[2]

highgrav <- bind %>% 
  #top_n(100, Grav_Markets.max) %>% 
  #select(Province,Country, Source, Site, Grav_Markets.max) %>% 
  filter(Grav_Markets.max > quantile[2]) 
unique(highgrav$Country)
nrow(highgrav)

highgrav %>% 
  group_by(strategy) %>% 
  tally() %>% 
  mutate(n.total = nrow(highgrav), 
         perc = n / n.total*100)

lowgrav <- bind %>% 
  #top_n(-50, Grav_Markets.max) %>% 
  #select(Province,Country, Source, Site, Grav_Markets.max) %>% 
  filter(Grav_Markets.max < quantile[1])
unique(lowgrav$Country)
nrow(lowgrav)

lowgrav %>% 
  group_by(strategy) %>% 
  tally() %>% 
  mutate(n.total = nrow(lowgrav), 
         perc = n / n.total*100)
#independent of gravity 

#SUPP PORTFOLIO FIG
high.grav <- #ggplot(data = bind,
       ggplot(data = filter(bind,Grav_Markets.max > quantile[2]),
       #ggplot(data = filter(bind,Grav_Markets.max < quantile[1]),
       aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = strategy), 
             colour = "black", 
             alpha = 0.5, size = 2, shape = 21) + 
  scale_fill_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(0,31),
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) + 
  scale_y_continuous(limits = c(0,100), 
                     breaks = c(10,25,50,75)) + 
  theme(panel.grid = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.ticks = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        text=element_text(family="Times"),
        legend.position = "none") +
  xlab("Maximum Degree Heating Weeks \n2014-2017") + 
  ylab("% Competitive + stress-tolerant") + 
  geom_hline(yintercept = 10, lty = 3, size = 0.25) + 
  geom_vline(xintercept = 4, lty = 3, size = 0.25)

low.grav <- #ggplot(data = bind,
  #ggplot(data = filter(bind,Grav_Markets.max > quantile[2]),
  ggplot(data = filter(bind,Grav_Markets.max < quantile[1]),
         aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = strategy), 
             colour = "black", 
             alpha = 0.5, size = 2, shape = 21) + 
  scale_fill_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(0,31),
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) + 
  scale_y_continuous(limits = c(0,100), 
                     breaks = c(10,25,50,75)) + 
  theme(panel.grid = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.ticks = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        text=element_text(family="Times"),
        legend.position = "none") +
  xlab("Maximum Degree Heating Weeks \n2014-2017") + 
  ylab("% Competitive + stress-tolerant") + 
  geom_hline(yintercept = 10, lty = 3, size = 0.25) + 
  geom_vline(xintercept = 4, lty = 3, size = 0.25)

g <- arrangeGrob(cbind(ggplotGrob(high.grav), ggplotGrob(low.grav), size = "last"))

ggsave(file.path(DROPBOX, "Paper", "figures", "from R",
                 "SUPP FIG - gravity x portfolio.pdf"), g, 
       width = 10, height = 5)


#------------------------------------------------------------------------------
# PORTFOLIO figure - FIG 3

bind %>% 
  group_by(strategy) %>% 
  tally() %>% 
  mutate(perc = n / 2584*100)

ggplot(data = bind,
       aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = strategy), 
             colour = "black", 
             alpha = 0.5, size = 2, shape = 21) + 
  scale_fill_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(0,31),
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) + 
  scale_y_continuous(limits = c(0,100), 
                     breaks = c(10,25,50,75)) + 
  theme(panel.grid = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.ticks = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        text=element_text(family="Times"),
        legend.position = "none") +
  xlab("Maximum Degree Heating Weeks \n2014-2017") + 
  ylab("% Competitive + \nstress-tolerant") + 
  geom_hline(yintercept = 10, lty = 3, size = 0.25) + 
  geom_vline(xintercept = 4, lty = 3, size = 0.25)

ggsave(file.path(DROPBOX, "Paper", "figures", "from R",
                 "Fig3 - portfolio base.pdf"),
       width = 4, height = 3.5)

#PORTFOLIO FIG 3 - PROTECT sites with colour bar
summary(bind$dist)

ggplot(data = filter(bind, refuge == "refuge"),
#ggplot(data = filter(bind.melanesia, refuge == "refuge"),
       aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = dist),
             shape = 21, alpha = 0.75, colour = "grey30", size = 2) +
  scale_fill_gradient2(low = "white", mid = "dodgerblue", high = "dodgerblue", 
                       midpoint = 40) +
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(0,31), 
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) + 
  scale_y_continuous(limits = c(0,100), 
                     breaks = c(10,25,50,75)) + 
  theme(panel.grid = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.ticks = element_blank(), 
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        text=element_text(family="Times"),
        legend.position = "none") +
  #geom_hline(yintercept = 10, lty = 3, size = 0.25) + 
  #geom_vline(xintercept = 4, lty = 3, size = 0.25)
  xlab("Maximum Degree Heating Weeks \n2014-2017") + 
  ylab("% Competitive + \nstress-tolerant")
  
ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "Fig3 - portfolio refuges.pdf"), 
       width = 4, height = 3.5)

##------------------------------------------------------------------------------------------
#FIGURE 3 MAP
source(file.path(PROJHOME, "R", "global-base-map_v2.dark.R"))

#BASE - all points - non-refuge
gg.dark + geom_point(aes(x=X, y=Y, fill = strategy), 
                shape = 21, stroke = 0.25,
                colour = "grey20", 
                size = 2,
                alpha = 0.5,
                position = position_jitter(width=0.5, height=0.5), 
                data = filter(bind, strategy != "refuge")) + 
  scale_fill_manual(values = c("goldenrod", "grey50")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme(legend.title=element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.text.y = element_blank(), 
        #axis.ticks = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA), 
        #plot.margin=unit(c(0,0,0,0), "null"), 
        text=element_text(family="Times"), 
        #panel.spacing=unit(c(0,0,0,0), "null"), 
        legend.position = "none") 

ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "Fig3 -MAP- nonrefuge.pdf"), 
       width = 12.78, height = 7)

#PROTECT
ggplot() + geom_point(aes(x=X, y=Y, 
                         fill = dist), 
                     shape = 21, stroke = 0.25,
                     colour = "grey10", 
                     fill = "dodgerblue",
                     size = 5,
                     alpha = 0.5,
                     position = position_jitter(width=0.5, height=0.5), 
                     data = filter(bind, strategy == "refuge")) + 
  #scale_fill_gradient2(low = "white", mid = "dodgerblue", high = "dodgerblue", 
  #                     midpoint = 40) +
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme(legend.title=element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid = element_blank(),
        #axis.text.y = element_blank(), 
        #axis.ticks = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        #plot.margin=unit(c(0,0,0,0), "null"), 
        text=element_text(family="Times"), 
        #panel.spacing=unit(c(0,0,0,0), "null"), 
        legend.position = "none") 

ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "Fig3 -MAP- refuges colourfill.pdf"), 
       width = 12.9, height = 4.4)

##------------------------------------------------------------------------------------------
#FIGURE 3 FACET MAP - SUPPLEMENT
source(file.path(PROJHOME, "R", "global-base-map_v2.dark.R"))

#ALL three strategies
gg.dark + geom_point(aes(x=X, y=Y, fill = strategy), 
                     shape = 21, stroke = 0.25,
                     colour = "grey20", 
                     size = 4,
                     alpha = 0.75,
                     position = position_jitter(width=0.5, height=0.5), 
                     data = bind) + 
  scale_fill_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme(legend.title=element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA), 
        #plot.margin=unit(c(0,0,0,0), "null"), 
        text=element_text(family="Times"), 
        #panel.spacing=unit(c(0,0,0,0), "null"), 
        legend.position = "none", 
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  facet_wrap(~strategy, ncol = 1)

ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "SUPP FIG -MAP- all three strategies.pdf"), 
       width = 12.78, height = 16)


##------------------------------------------------------------------------------------------
# SOCIAL CONTEXT 
### ============================================================================
names(bind)
unique(bind$refuge)

#------------------------------------------------------------------------------
#anovas with unequal variances - 3 groups
names(bind)
?dplyr::rename

bind$strategy <- dplyr::recode(bind$strategy, 
                               refuge = "Protect", 
                               recover = "Recover",
                               transform = "Transform")
unique(bind$strategy)

#gravity - log
#crop change - standardize only 
hist(bind$perc_crop2002.2012)
hist(log(bind$perc_crop2012+1))
bind$l.perc_crop2012 <- log(bind$perc_crop2012 + 1)


social.vars <- bind %>% 
  dplyr::select(Source, Site, strategy, 
         l.Grav_Markets.max, l.Grav_NearPop.max, 
         perc_crop2002.2012, l.perc_crop2012, 
         hdi2015) %>% 
  melt(id.vars = 1:3)

#FIG 3D - CONTINUOUS VARS
unique(social.vars$variable)
var.labels <- c(l.Grav_Markets.max = "Market gravity", 
                l.Grav_NearPop.max = "Settlement gravity", 
                perc_crop2002.2012 = "Agricultural expansion", 
                l.perc_crop2012 = "Total agriculture", 
                hdi2015 = "HDI")

ggplot(data = social.vars, aes(x = strategy, y = value)) + 
  geom_boxplot(aes(colour = strategy), 
               size = 0.5,
               outlier.size = 0.75) + 
  facet_wrap(~variable, ncol = 3, scales = "free_y", 
             labeller=labeller(variable = var.labels)) +
  scale_colour_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  #scale_fill_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  theme_bw(base_size = 18) + 
  theme(#legend.position = "none", 
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(family="Times")) 

ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "Fig3 - SOCIAL continuous  vars.pdf"), 
       width = 8, height = 5)

#check how marginal effects are
head(social.vars)
summary <- social.vars %>% 
  group_by(strategy, variable) %>% 
  summarize(median = median(value)) %>% 
  arrange(variable)
summary

exp(1.16)
exp(1.89)

#Gravity, market
oneway.test(bind$l.Grav_Markets.max ~ bind$strategy)
summary(lm(bind$l.Grav_NearPop.max ~ bind$strategy))
#transform is significantly closer to markets, p < 0.001

#Gravity, near population
summary(lm(bind$l.Grav_NearPop.max ~ bind$strategy))
oneway.test(bind$l.Grav_NearPop.max ~ bind$strategy)
#transform has higher gravity near pop p < 0.001

#Croplands,change (CONVERSION to AGRICULTURE)
oneway.test(bind$perc_crop2002.2012 ~ bind$strategy)
summary(lm(bind$perc_crop2002.2012 ~ bind$strategy))
#recover, transform have more conversion to agriculture

#Crops, 2012
oneway.test(bind$l.perc_crop2012 ~ bind$strategy)
summary(lm(bind$l.perc_crop2012 ~ bind$strategy))
#recover, transform have more nearby agriculture in 2012

#HDI - national capacity to manage
summary(lm(bind$hdi2015 ~ bind$strategy))
oneway.test(bind$hdi2015 ~ bind$strategy)
#refuge in lowest hdi countries, p = <0.0001

#Management
bind$Management <- factor(bind$Management,
                          levels = c("Fished", "Restricted", "No-take"))
names(bind)

mgmt.test <- bind %>% 
  group_by(strategy, Management) %>% 
  tally() %>% 
  dcast(strategy ~ Management)
mgmt.test
chisq.test(as.matrix(mgmt.test[,-1]))
#sig difference 

#Fig 3D - management stacked bar plot

ggplot(data = bind, aes(x = strategy)) + 
  geom_bar(aes(fill = Management, colour = strategy), 
           position = "fill") + 
  scale_colour_manual(values = c("dodgerblue", "goldenrod", "grey50")) + 
  scale_fill_manual(values = c("white", "grey50", "black")) + 
  theme_bw(base_size = 18) + 
  theme(#legend.position = "none", 
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank(), 
        text=element_text(family="Times")) 

ggsave(file.path(DROPBOX, "Paper", "figures", "from R", 
                 "Fig3 - SOCIAL management var.pdf"), 
       width = 2.75, height = 2.6)



#more no-take; p < 0.001
#less open access refuges

# check <- filter(bind, Country == "Indonesia") %>% 
#   select(Province,Country, Source, Site, Grav_Markets.max)

#idea
#photos, portfolio
#2 maps panels - high grav, low grav (all the same places)
#box plots by social vars

