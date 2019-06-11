#This script plots coefficient tables for coral life histories
#splits out by each category, and higher font size for presentations

library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "00-source-models.R"))

#select and order coral genera from coef table
head(coef)

unique(coef$lh)
coef.taxa <- coef %>% 
  filter(lh %in% c("competitive-5000", "stresstolerant-5000", 
                   "generalist", "weedy")) %>% 
  filter(response != "pit") %>% 
  filter(var != "Intercept") %>% 
  mutate(lh = capitalize(lh), 
         lh = fct_recode(lh, 
                         "Competitive" = "Competitive-5000", 
                         "Stress-tolerant" = "Stresstolerant-5000"))

coef.taxa$lh <- factor(coef.taxa$lh,
                       levels = c("Competitive", "Stress-tolerant", 
                                  "Generalist", "Weedy"))

levels(coef.taxa$lh)
coef.taxa

#set significance variable by taxa
coef.taxa <- coef.taxa %>% 
  mutate(g = if_else(Q90 < 0 | Q10 > 0, "sig", "ns"), #80 credible, 
         signif.taxa = ifelse(g == "sig", paste(lh, g, sep = "-"), "ns"))

#fig 2 - all lh x all vars
levels(as.factor(coef.taxa$signif.taxa))
levels(as.factor(coef.taxa$group))
coef.taxa

#method
ggplot(data = filter(coef.taxa, group == "Methods"), 
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
                colour = "black", size = 0.5, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 1.5) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkorange",
                                 "grey80", #light-ns
                                 "dodgerblue4", 
                                 #"grey18", #dark-coralcover
                                 "chartreuse3")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 18))

ggsave(here("analysis", "outputs", "figs", 
            "keynote-fig2-methods.pdf"), 
       width = 14, height = 3)

#environment
ggplot(data = filter(coef.taxa, group == "Environment"), 
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
                colour = "black", size = 0.5, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 1.5) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkorange",
                                 "grey80", #light-ns
                                 "dodgerblue4", 
                                 #"grey18", #dark-coralcover
                                 "chartreuse3")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 18))

ggsave(here("analysis", "outputs", "figs", 
            "keynote-fig2-env.pdf"), 
       width = 14, height = 6)

#social
ggplot(data = filter(coef.taxa, group == "Social"), 
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
                colour = "black", size = 0.5, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 1.5) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkorange",
                                 "grey80", #light-ns
                                 "dodgerblue4", 
                                 #"grey18", #dark-coralcover
                                 "chartreuse3")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 18))

ggsave(here("analysis", "outputs", "figs", 
            "keynote-fig2-social.pdf"), 
       width = 14, height = 5)

#climate
ggplot(data = filter(coef.taxa, group == "Climate"), 
       aes(x = label, y = Estimate)) +
  geom_hline(aes(yintercept = 0), colour = "black") + 
  geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
                colour = "black", size = 0.5, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 1.5) +
  coord_flip() + 
  theme_sleek(base_size = 24) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("red", "darkorange",
                                 "grey80", #light-ns
                                 "dodgerblue4", 
                                 #"grey18", #dark-coralcover
                                 "chartreuse3")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 18))

ggsave(here("analysis", "outputs", "figs", 
            "keynote-fig2-climate.pdf"), 
       width = 14, height = 3)
