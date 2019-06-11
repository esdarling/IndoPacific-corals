#This script creates figure 1 in NEE revision

library(here)
source(here("analysis", "fig-1-map", "00-source-map.R"))
source(here("analysis", "fig-1-map", "global-base-map_v2.R"))
source(here("analysis", "fig-1-map", "global-base-map_v2.dark.R"))

#gg
#gg.dark

head(d.map)

#set lh colours 
#change lh.max labels
d.map <- d.map %>% 
  mutate(lh.max = fct_recode(lh.max,
                             Competitive = "perc_competitive", 
                             `Stress-tolerant` = "perc_stresstolerant",
                             Generalist = "perc_generalist", 
                             Weedy = "perc_weedy"), 
         lh.max = fct_relevel(lh.max, 
                              c("Competitive", "Stress-tolerant", 
                                "Generalist", "Weedy")))
levels(as.factor(d.map$lh.max))

#lh.colours <- c("red","dodgerblue4", "darkorange", "chartreuse3")
#new color blind friendly
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
lh.colours <- c("#D55E00","#0072B2", "#F0E442", "#009E73")

#----------------------------------------------
#make map with most dominant lh
fig1 <- gg + geom_point(aes(x = X, y = Y, 
                    fill = lh.max, 
                    size = lh.max.value), 
                shape = 21, alpha = 0.75, 
                data = d.map) + 
  scale_size(range = c(1,7), 
             breaks = c(10,25,50,75), 
             guide = guide_legend(title="Absolute \n% cover")) +   
                                  #nrow=2,byrow=TRUE)) +
  scale_fill_manual(values = lh.colours, 
                    name = "Dominant \nlife history", 
                    guide = guide_legend(override.aes = list(alpha = 1, size = 3))) + 
                    #guide = guide_legend(nrow=2,byrow=TRUE)) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 18) + 
  theme(legend.position = "right", 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())

#library(cowplot)
#plot_grid(fig1, labels = "a")

fig1
ggsave(here("analysis", "outputs", "figs", 
            "fig1-map.pdf"), 
       width = 11.75, height = 4)

#SUPP map with 4 facets
gg + geom_point(aes(x = X, y = Y, 
                    fill = lh.max, 
                    size = lh.max.value), 
                shape = 21, alpha = 0.5, 
                data = d.map) + 
  scale_size(range = c(1,7), 
             breaks = c(10,25,50,75), 
             guide = guide_legend(title="Absolute \n% cover")) +   
  #nrow=2,byrow=TRUE)) +
  scale_fill_manual(values = lh.colours, 
                    name = "Life history", 
                    guide = guide_legend(override.aes = list(alpha = 1, size = 3))) + 
                    #guide = guide_legend(nrow=2,byrow=TRUE)) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.position = "right", 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) + 
  facet_wrap(~lh.max, ncol = 1)

ggsave(here("analysis", "outputs", "figs", 
            "supp map-4-life-histories.pdf"), 
       width = 8, height = 10)

#----------------------------------------------
#histograms with  10% 
head(d.map)

d.hist <- d.map %>% 
  select(Source, Site, Province, 
         perc_competitive:perc_weedy) %>% 
  melt(id.vars = 1:3) %>% 
  as.tibble() %>% 
  mutate(variable = fct_recode(variable, 
                               "Competitive" = "perc_competitive", 
                               "Stress-tolerant" = "perc_stresstolerant", 
                               "Generalist" = "perc_generalist", 
                               "Weedy" = "perc_weedy"))

d.hist

ggplot(data = d.hist, aes(x = value, fill = variable)) + 
  geom_histogram() + 
  facet_wrap(~variable,ncol = 4) +
  scale_fill_manual(values = lh.colours) + 
  theme_sleek(base_size = 14) + 
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent")) + 
  xlab("Absolute % cover") +
  ylab("# reefs") + 
  geom_vline(xintercept = 10, size = 0.25, colour = "grey50", lty = "dashed") + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1350), 
                     breaks = c(0,500,1000)) + 
  scale_x_continuous(breaks = c(0,10,25,50,75)) 
  
ggsave(here("analysis", "outputs", "figs", 
            "fig1-hist-lh.pdf"), 
       width = 10, height = 2.5) 
