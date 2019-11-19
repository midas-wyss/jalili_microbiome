# Figure S6: Barplot for relative abundances across all samples
df2 %>%
  # tibble::add_column(., s_id = factor(df2$sample_group, levels = c("Hmb11", "anaerobic", "aerobic"))) %>% 
  tibble::add_column(., p_id = gsub("Hmb11 \\w* \\(replicate NA\\)", "Hmb11", gsub(" )", ")",paste(df2$sample_group,df2$day,"(replicate",df2$replicate,")")))) %>% 
  ggplot() + 
  geom_bar(aes(x = p_id, y = relative_abundance, fill = genus), stat = "identity", color = "black") + 
  scale_fill_manual(breaks = c("Unknown", "Sutterella", "Bilophila", "Akkermansia", "Blautia",
                               "Oscillospira", "Ruminococcus", "Enterococcus", "[Eubacterium]",
                               "Parabacteroides", "Bacteroides", "Citrobacter"),
                    values = c("#CCCCCC", "#FFCC00", "#99CCCC", "#33CC33", "#CC3399",
                               "#CC99CC", "#336699", "#66CCFF", "#669933",
                               "#FF6600", "#993300", "#CCCCFF"),
                    name = "Genus") + 
  labs(x = "Sample", y = "Relative\nabundance") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, angle = 90, color = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid = element_blank())
ggsave(file=paste0("./results/Camacho_FigS6_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)