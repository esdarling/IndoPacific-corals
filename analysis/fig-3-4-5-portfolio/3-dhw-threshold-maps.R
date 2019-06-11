#this code makes the supp MAPS of DHW thresholds
library(here)
source(here("analysis", "fig-3", "00-source-portfolios.R"))
source(here("analysis", "fig-1-map", "global-base-map_v2.dark.R"))

#d is original model dataset with posterior predictions
d
names(d)
unique(d$strategy)

#-------------------------------------------------------
#filter on 2014-17 DHW
#make maps for 2, 2.5, 3, 3.5

summary(d$CoralTemp.maxDHW)
unique(d$strategy)

dhw2 <- gg.dark + geom_point(aes(x=X, y=Y),
                             fill = "dodgerblue",  colour = "grey20", 
                             size = 4, shape = 21, stroke = 0.25, alpha = 0.25,
                             data = filter(d, CoralTemp.maxDHW < 2 & 
                                             strategy == "refuge")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

dhw2.5 <- gg.dark + geom_point(aes(x=X, y=Y),
                             fill = "dodgerblue",  colour = "grey20", 
                             size = 4, shape = 21, stroke = 0.25, alpha = 0.25,
                             data = filter(d, CoralTemp.maxDHW < 2.5 & 
                                             strategy == "refuge")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

dhw3 <- gg.dark + geom_point(aes(x=X, y=Y),
                             fill = "dodgerblue",  colour = "grey20", 
                             size = 4, shape = 21, stroke = 0.25, alpha = 0.25,
                             data = filter(d, CoralTemp.maxDHW < 3 & 
                                             strategy == "refuge")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

dhw3.5 <- gg.dark + geom_point(aes(x=X, y=Y),
                             fill = "dodgerblue",  colour = "grey20", 
                             size = 4, shape = 21, stroke = 0.25, alpha = 0.25,
                             data = filter(d, CoralTemp.maxDHW < 3.5 & 
                                             strategy == "refuge")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

dhw4 <- gg.dark + geom_point(aes(x=X, y=Y),
                               fill = "dodgerblue",  colour = "grey20", 
                               size = 4, shape = 21, stroke = 0.25, alpha = 0.25,
                               data = filter(d, CoralTemp.maxDHW < 4 & 
                                               strategy == "refuge")) + 
  scale_y_continuous(limits = c(-38,34), breaks = c(-20,0,20)) +
  scale_x_continuous(limits = c(30,260)) + 
  theme_sleek(base_size = 14) + 
  theme(legend.title=element_text(size=14),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())


#multiplot together
library(cowplot)
?plot_grid
plot_grid(dhw2, dhw2.5, dhw3, dhw3.5,dhw4,
          align = "h", ncol = 1, 
          labels = "AUTO")

ggsave(here("analysis", "outputs", "figs", 
            "supp-dhw-thresholds.pdf"),
       height = 8, width = 5)
