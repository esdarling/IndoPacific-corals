#this code predicts comp and ST models 
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "fig-3", "00-source-portfolios.R"))

names(d)

#how many reefs above 10% 
summary(d$comp_plus_st)
d %>% 
  filter(comp_plus_st >=10) %>% 
  nrow()

1856/2584

#new color blind friendly
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

#transform "#999999"
#recover "#E69F00"
#protect"#56B4E9"

#portfolio
ggplot(data = d, aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = strategy),
             colour = "black",
             alpha = 0.5, size = 4, shape = 21) +
  scale_fill_manual("Strategy", 
                    values = c("#56B4E9", "#E69F00", "#999999"),
                    labels = c("Protect", "Recover", "Transform")) +
  theme_sleek(base_size = 18) +
  scale_x_continuous(limits = c(0,31),
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) +
  scale_y_continuous(limits = c(0,100),
                     breaks = c(10,25,50,75)) +
  theme(panel.grid = element_blank()) +
  xlab("Maximum Degree Heating Weeks \n2014-2017") +
  ylab("% Coral cover \n(competitive + stress-tolerant)") +
  geom_hline(yintercept = 10, lty = 3, size = 0.5) +
  geom_vline(xintercept = 4, lty = 3, size = 0.5) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

ggsave(here("analysis", "outputs", "figs", 
            "portfolio-fig.pdf"),
       width = 8, height = 6)

## ---------------------------------------------------------------------
#facet_wrap by three strategies
ggplot(data = d, aes(x = CoralTemp.maxDHW, y = comp_plus_st)) +
  geom_point(aes(fill = strategy), 
             colour = "black", 
             alpha = 0.5, size = 2, shape = 21) + 
  scale_colour_manual(values = c("#56B4E9", "#E69F00", "#999999")) + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#999999")) + 
  theme_sleek(base_size = 12) + 
  scale_x_continuous(limits = c(0,31),
                     breaks = c(4,10,20,30), expand = c(0.02,0.02)) + 
  scale_y_continuous(limits = c(0,100), 
                     breaks = c(10,25,50,75)) + 
  theme(panel.grid = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        #axis.ticks = element_blank(), 
        #plot.background = element_rect(fill = "transparent", colour = NA), 
        #text=element_text(family="Times"),
        legend.position = "none") +
  xlab("Maximum Degree Heating Weeks \n2014-2017") + 
  ylab("% Competitive + \nstress-tolerant (empirical)") + 
  geom_hline(yintercept = 10, lty = 3, size = 0.25) + 
  geom_vline(xintercept = 4, lty = 3, size = 0.25) + 
  facet_wrap(~strategy, ncol = 3)

ggsave(here("analysis", "outputs", "figs", 
            "fig3-portfolio-facet-by-strategy.pdf"),
       width = 8, height = 4)





