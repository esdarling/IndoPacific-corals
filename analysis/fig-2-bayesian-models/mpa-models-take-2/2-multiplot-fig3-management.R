
#load original models - with "Fished" as Management level
source(here("analysis", "bayesian-models", "7b-predict-pop-yearsDHW.R"))
source(here("analysis", "bayesian-models", "7a-predict-markets-yearsdhw.R"))
source(here("analysis", "bayesian-models", "7c-predict-markets-productivity.R"))
source(here("analysis", "bayesian-models", "7e-predict-pop-productivity.R"))
source(here("analysis", "bayesian-models", "7d-predict-yearsDHW-cyclones.R"))

#Restricted as management level
source(here("analysis", "bayesian-models", "mpa-models-take-2", "1b-predict-pop-yearsDHW-restricted.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1a-predict-markets-yearsdhw-restricted.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1c-predict-markets-productivity-restricted.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1e-predict-pop-productivity-restricted.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1d-predict-yearsDHW-cyclones-restricted.R"))

#No-take as management level
source(here("analysis", "bayesian-models", "mpa-models-take-2","1b-predict-pop-yearsDHW-notake.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1a-predict-markets-yearsdhw-notake.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1c-predict-markets-productivity-notake.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1e-predict-pop-productivity-notake.R"))
source(here("analysis", "bayesian-models", "mpa-models-take-2","1d-predict-yearsDHW-cyclones-notake.R"))


#join together, export as one figure
p1 <- gg.yearsdhw.pop
p2 <- gg.yearsdhw.markets
p3 <- gg.years.cyclones
p4 <- gg.pop.prod
p5 <- gg.markets.prod

p6 <- gg.yearsdhw.pop.restricted
p7 <- gg.yearsdhw.markets.restricted
p8 <- gg.years.cyclones.restricted
p9 <- gg.pop.prod.restricted
p10 <- gg.markets.prod.restricted

p11 <- gg.yearsdhw.pop.notake
p12 <- gg.yearsdhw.markets.notake
p13 <- gg.years.cyclones.notake
p14 <- gg.pop.prod.notake
p15 <- gg.markets.prod.notake

library(cowplot)
plot_grid(p1, p2, p3, p4, p5,
          p6, p7, p8, p9, p10, 
          p11, p12, p13, p14, p15, 
          nrow = 3, 
          labels = c("a","","","","",
                     "b","","","","",
                     "c","","","",""))

ggsave(here("analysis", "outputs", "figs", 
            "fig3-by-management.pdf"),
       height = 6.5, width = 12)




