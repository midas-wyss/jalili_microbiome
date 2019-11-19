# Figure S7: Relative abundance on anaerobic chip across all days
# somewhat captured in figure 4a

df2 %>% 
  dplyr::filter(., sample_group == "anaerobic", genus != "Unknown") %>%
  # filter(., !is.na(condition)) %>%
  # dplyr::filter(., relative_abundance < 0.2) %>%
  ggplot() + 
  geom_boxplot(aes(x = genus, y = relative_abundance, fill = as.factor(day)), outlier.size = 0) +
  geom_point(aes(x = genus, y = relative_abundance, fill = as.factor(day)), pch = 21, size = 2, position = position_dodge(width = 0.75)) +
  # geom_dotplot(aes(x = genus, y = relative_abundance, fill = as.factor(day)), binaxis = "y", stackdir = "center", position = "dodge") +
  scale_fill_manual(breaks = c(1, 2, 3),
                    values = c("#FF9900", "#CC9933", "#996633"),
                    labels = c("Day 1", "Day 2", "Day 3"),
                    name = NULL) +
  # geom_point(aes(x = bacteria, y = relative_abundance, color = as.factor(day)), position = position_jitter(width = 0.2)) +
  # scale_color_manual(breaks = c(1, 2, 3),
  #                   values = c("#FF9900", "#CC9933", "#996633"),
  #                   labels = c("Day 1", "Day 2", "Day 3"),
  #                   name = "Day") +
  # facet_grid(. ~ condition, scales = "free") + 
  labs(y = "Relative abundance", x = "Bacterium") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5,size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        axis.title = element_text(size=20,face="bold",color="black"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12,color="black"))
ggsave(file=paste0("./results/Camacho_FigS7_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)