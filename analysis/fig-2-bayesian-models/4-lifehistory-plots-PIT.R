#This script plots coefficient tables for coral genera - with ONLY PIT method
library(here)
source(here("analysis", "bayesian-models", "00-source-models.R"))

#select and order coral genera from coef table
head(coef)

unique(coef$lh)
coef.pit <- coef %>% 
  filter(response == "pit") %>% 
  filter(var != "Intercept") %>% 
  mutate(lh = capitalize(lh))
unique(coef.pit$lh)

coef.pit$lh <- factor(coef.pit$lh,
                       levels = c("Competitive", "Stresstolerant", 
                                  "Generalist", "Weedy"))

coef.pit

#set significance variable by lh
coef.pit <- coef.pit %>% 
  mutate(g = if_else(Q90 < 0 | Q10 > 0, "sig", "ns"), #80 credible, 
         signif.lh = ifelse(g == "sig", paste(lh, g, sep = "-"), "ns"))
levels(as.factor(coef.pit$signif.lh))

#plots
ggplot(data = coef.pit,  
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #95th credible
                colour = "black", size = 0.2, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80? credible
                      colour = signif.lh),
                  size = 0.5) +
  coord_flip() + 
  theme_sleek(base_size = 13) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkorange", 
                                 "grey80", 
                                 "dodgerblue4", "chartreuse3")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 9))


ggsave(here("analysis", "outputs", "figs", 
            "supp lh-coef-pit-only.pdf"), 
       width = 8, height = 5.1)




