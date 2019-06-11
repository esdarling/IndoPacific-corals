#This script plots coefficient tables for coral genera
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "00-source-models.R"))

#select and order coral genera from coef table
head(coef)

unique(coef$lh)
coef.taxa <- coef %>% 
  filter(lh %in% c("acropora", "porites", "montipora", "pocillopora")) %>% 
  filter(var != "Intercept") %>% 
  rename(taxa = "lh") %>% 
  mutate(taxa = capitalize(taxa))

coef.taxa$taxa <- factor(coef.taxa$taxa,
                    levels = c("Acropora", "Porites", 
                               "Montipora", "Pocillopora"))

levels(coef.taxa$taxa)
coef.taxa

#set significance variable by taxa
coef.taxa <- coef.taxa %>% 
  mutate(g = if_else(Q90 < 0 | Q10 > 0, "sig", "ns"), #80 credible, 
         signif.taxa = ifelse(g == "sig", paste(taxa, g, sep = "-"), "ns"))
levels(as.factor(coef.taxa$signif.taxa))

#plots
ggplot(data = coef.taxa,  
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #95th credible
                colour = "black", size = 0.2, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80? credible
                      colour = signif.taxa),
                  size = 0.5) +
  coord_flip() + 
  theme_sleek() + 
  facet_grid(group ~ taxa, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("darkred", "purple", "lightgrey", 
                                 "lightblue", "darkorange")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank())

ggsave(here("analysis", "outputs", "figs", 
            "supp taxa-coef.pdf"), 
       width = 8, height = 5)




