#This figure compares maps of empirical and modelled portfolios

#This file creates the maps for Fig 3 and supplement (3 strategies separate)
library(here)
source(here("analysis", "fig-3", "00-source-portfolios.R"))
source(here("analysis", "fig-1-map", "global-base-map_v2.dark.R"))

#d is original model dataset with posterior predictions
d
names(d)
unique(d$strategy)

#WIO countries
unique(d$Nation)

#MAPS
#arrange d
d <- d %>% 
  arrange(desc(strategy)) %>% 
  mutate(strategy = fct_recode(strategy, 
                               "Protect" = "refuge", 
                               "Recovery" = "recover", 
                               "Transform" = "transform"))

fig3b.map <- gg.dark + geom_point(aes(x=X, y=Y, 
                         fill = strategy, 
                         size = strategy), 
                     shape = 21, stroke = 0.25,
                     colour = "grey20", 
                     alpha = 1,
                     position = position_jitter(width=0.5, height=0.5), 
                     data = d) + 
  scale_size_manual(values = c(5,3,3)) + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#999999")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        #plot.margin=unit(c(0,0,0,0), "null"), 
        #text=element_text(family="Times"), 
        #panel.spacing=unit(c(0,0,0,0), "null"), 
        legend.position = "none") 

fig3b.map

##------------------------------------------------------------------------------------------
#FIGURE 3 FACET MAP - SUPPLEMENT
#ALL three strategies
gg.dark + geom_point(aes(x=X, y=Y, fill = strategy), 
                     shape = 21, stroke = 0.25,
                     colour = "grey20", 
                     size = 4,
                     alpha = 0.5,
                     position = position_jitter(width=0.5, height=0.5), 
                     data = d) + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#999999")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        #plot.margin=unit(c(0,0,0,0), "null"), 
        #text=element_text(family="Times"), 
        #panel.spacing=unit(c(0,0,0,0), "null"), 
        legend.position = "none", 
        strip.background = element_blank()) + 
  facet_wrap(~strategy, ncol = 1, 
             strip.position = "top")

ggsave(here("analysis", "outputs", "figs", 
            "SUPP-MAP-three strategies.pdf"), 
       width = 8, height = 8)
