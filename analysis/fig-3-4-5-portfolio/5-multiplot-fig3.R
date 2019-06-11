#this file combines fig 3a and fig3b
library(here)
source(here("analysis", "fig-3", "1-fig-3a-empirical-portfolio.R"))
source(here("analysis", "fig-3", "2-fig-3-maps-empirical.R"))

#join together, export as one figure
fig3a
fig3b.map

#source(here("analysis", "bayesian-models", "8-multiplot-function.R"))
# pdf(here("analysis", "outputs", "figs", "fig3a-b.pdf"),
#     height = 3, width = 11)
# multiplot(p1, p2, ncol = 2)
# dev.off()

library(cowplot)
plot_grid(fig3a, fig3b.map, labels = c('a', 'b'), align = "h", 
          rel_widths = c(0.425, 0.85))

ggsave(here("analysis", "outputs", "figs", 
            "fig3a-b.pdf"),
       height = 4, width = 11)

#load in fig 3c 
#load in 4 plots for fig 3 - bivariate cofficients
source(here("analysis", "bayesian-models", "7b-predict-pop-yearsDHW.R"))
source(here("analysis", "bayesian-models", "7a-predict-markets-yearsdhw.R"))
source(here("analysis", "bayesian-models", "7c-predict-markets-productivity.R"))
source(here("analysis", "bayesian-models", "7e-predict-pop-productivity.R"))
source(here("analysis", "bayesian-models", "7d-predict-yearsDHW-cyclones.R"))

#join together, export as one figure
p1 <- gg.yearsdhw.pop
p2 <- gg.yearsdhw.markets
p3 <- gg.years.cyclones
p4 <- gg.pop.prod
p5 <- gg.markets.prod

#p6

plot_grid(p1, p2, p3, p4, p5,
          labels = c("","","","", ""), 
          align = "h", ncol = 5, 
          rel_widths = c(1, 1, 0.95, 1, 1.45))

ggsave(here("analysis", "outputs", "figs", 
            "fig3c.pdf"),
       height = 2.2, width = 11.2)

