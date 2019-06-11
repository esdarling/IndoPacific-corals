#This script plots coefficient tables for coral life histories
library(here)
source(here("analysis", "00-source.R"))
source(here("analysis", "bayesian-models", "00-source-models.R"))

#select and order coral genera from coef table
head(coef)

unique(coef$lh)
coef.taxa <- coef %>% 
  filter(lh %in% c("competitive-5000", "stresstolerant-5000", 
                   "generalist", "weedy", 
                   "sumcover")) %>% 
  filter(response != "pit") %>% 
  filter(var != "Intercept") %>% 
  mutate(lh = capitalize(lh), 
         lh = fct_recode(lh, 
                         "Competitive" = "Competitive-5000", 
                         "Stress-tolerant" = "Stresstolerant-5000", 
                         "Total coral cover" = "Sumcover"))

coef.taxa$lh <- factor(coef.taxa$lh,
                         levels = c("Total coral cover", 
                                    "Competitive", "Stress-tolerant", 
                                    "Generalist", "Weedy"))

levels(coef.taxa$lh)
coef.taxa

#new color blind friendly
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
lh.colours <- c("#D55E00","#0072B2", "#F0E442", "#009E73")


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
                colour = "black", size = 0.5, width = 0) +
  geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
                      colour = signif.taxa),
                  size = 1) +
  coord_flip() + 
  theme_sleek(base_size = 18) + 
  facet_grid(group ~ lh, scales = "free_y", space = "free_y") + 
  scale_colour_manual(values = c("#D55E00", "#F0E442",
                                 "grey80", #light-ns
                                 "#0072B2", 
                                 "grey18", #dark-coralcover
                                 "#009E73")) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
  ylab("Standardized effect size") +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12))

ggsave(here("analysis", "outputs", "figs", 
            "fig2-lh-coef-with-sumcover.pdf"), 
       width = 12, height = 7.5)

# #-----
# #fig 2 - climate and social
# unique(coef.taxa$group)
# levels(coef.taxa$lh)
# 
# #reverse levels for panel plots - order of lh
# coef.taxa$lh <- factor(coef.taxa$lh, 
#                        levels = rev(levels(coef.taxa$lh)))
# levels(coef.taxa$lh)
# 
# #climate - fig2
# d.fig2.climate <- coef.taxa %>% 
#   filter(group == "Climate")
# unique(d.fig2.climate$signif.taxa)
# levels(d.fig2.climate$lh)
# 
# ggplot(data = d.fig2.climate, 
#        aes(x = lh, y = Estimate)) +
#   geom_hline(aes(yintercept = 0), colour = "black") + 
#   geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
#                 colour = "black", size = 0.2, width = 0) +
#   geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
#                       colour = lh),
#                   size = 0.5) +
#   coord_flip() + 
#   theme_sleek() + 
#   facet_wrap(~label, scales = "free_y", ncol = 1) + 
#   scale_colour_manual(values = c("darkgreen", "darkorange2", "blue", "red")) + 
#   #scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
#   ylab("Standardized effect size") +
#   theme(legend.position = "none", 
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(), 
#         text=element_text(family="Times"))
# 
# ggsave(here("analysis", "outputs", "figs",
#                  "EFFECT SIZES - climate.pdf"),
#        height = 5, width = 3)
# 
# #social - fig2
# d.fig2.social <- coef.taxa %>% 
#   filter(group == "Social")
# unique(d.fig2.social$signif.taxa)
# levels(d.fig2.social$lh)
# 
# #reverse levels of social var
# d.fig2.social$label <- factor(d.fig2.social$label, 
#                        levels = rev(levels(d.fig2.social$label)))
# levels(d.fig2.social$label)
# 
# ggplot(data = d.fig2.social, 
#        aes(x = lh, y = Estimate)) +
#   geom_hline(aes(yintercept = 0), colour = "black") + 
#   geom_errorbar(aes(ymin = Q5, ymax = Q95), #90% credible interval
#                 colour = "black", size = 0.2, width = 0) +
#   geom_pointrange(aes(ymin = Q10, ymax = Q90, #80% credible interval
#                       colour = lh),
#                   size = 0.5) +
#   coord_flip() + 
#   theme_sleek() + 
#   facet_wrap(~label, scales = "free_y", ncol = 2) + 
#   scale_colour_manual(values = c("darkgreen", "darkorange2", "blue", "red")) + 
#   #scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1)) + 
#   ylab("Standardized effect size") +
#   theme(legend.position = "none", 
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(), 
#         text=element_text(family="Times"))
# 
# ggsave(here("analysis", "outputs", "figs",
#             "EFFECT SIZES - social.pdf"),
#        height = 5, width = 6)



