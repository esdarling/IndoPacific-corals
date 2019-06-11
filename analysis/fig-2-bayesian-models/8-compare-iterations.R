#This script compares comp and st plots for 4000 vs 2000 iterations
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "00-source-models.R"))

#select and order coral genera from coef table
head(coef)

unique(coef$lh)
coef.taxa <- coef %>% 
  filter(lh %in% c("competitive", "stresstolerant", 
                   "competitive-5000", "stresstolerant-5000")) %>% 
  filter(response != "pit") %>% 
  filter(var != "Intercept") %>% 
  mutate(lh = capitalize(lh))

levels(as.factor(coef.taxa$lh))
coef.taxa

#set significance variable by taxa
coef.taxa <- coef.taxa %>% 
  mutate(g = if_else(Q90 < 0 | Q10 > 0, "sig", "ns"), #80 credible, 
         signif.taxa = ifelse(g == "sig", paste(lh, g, sep = "-"), "ns"))

#fig 2 - all lh x all vars
levels(as.factor(coef.taxa$signif.taxa))

ggplot(data = coef.taxa,  
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
                colour = "black", size = 0.25, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 0.75) +
  coord_flip() + 
  theme_sleek(base_size = 13) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkred",
                                 "grey80", #light-ns
                                 "blue", "darkblue")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 9))

ggsave(here("analysis", "outputs", "figs", 
            "fig2-compare-iterations-comp-st.pdf"), 
       width = 10, height = 5.5)
