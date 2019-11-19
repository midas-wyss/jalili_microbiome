#Figure XXX-2: Akkermansia changes in abundance between chips, across days
df2 %>% 
  dplyr::filter(., genus == "Akkermansia") %>%
  dplyr::filter(., sample_group != "Hmb11") %>%
  ggplot() +
  geom_boxplot(aes(x = day, y = counts, fill = sample_group), outlier.size = 0) + 
  geom_point(aes(x = day, y = counts, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#006699", "#00CCFF"), 
                    breaks = c("aerobic", "anaerobic"),
                    labels = c("Aerobic", "Anaerobic"),
                    name = "Condition") + 
  labs(x = "Day", y = "Total counts") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid = element_blank())
ggsave(file=paste0("./results/Camacho_FigXXX-2_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)
