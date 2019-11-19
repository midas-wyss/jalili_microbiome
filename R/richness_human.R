# Figure S9: Diversity analysis 
strip_labels <- c("1" = "Day 1",
                  "2" = "Day 2",
                  "3" = "Day 3",
                  "NA" = "Human stool")

combo_richness %>% 
  dplyr::filter(., metric == "Observed", !is.na(condition)) %>% 
  ggplot() + 
  geom_boxplot(aes(x = condition, y = alpha_diversity), outlier.size = 0) +
  geom_point(aes(x = condition, y = alpha_diversity, fill = condition), pch = 21, size = 4, color = "black") +
  scale_fill_manual(values = c("#006699", "#00CCFF", "#cc6633"), breaks = c("aerobic", "anaerobic", "human_stool")) + 
  labs(x=NULL,y="Observed Diversity") + 
  facet_grid(~ day, labeller = as_labeller(strip_labels), scales = "free") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        axis.text.x = element_text(size = 18, color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 24,face="bold"),
        legend.position = "none")