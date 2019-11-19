# Figure XXX-1: Heatmap for human microbiome compared to the 2 chip conditions

df3 %>% 
  dplyr::mutate(., counts = replace(counts, counts == 0, 1)) %>%
  dplyr::filter(., sample != "Hmb11", genus != "Unknown") %>%
  ggplot() + 
  geom_tile(aes(x = genus, y = sample, fill = log10(counts)), color = "black") +
  scale_fill_gradient(low = "#FFFFFF", high = "#006699", guide = "colorbar", na.value = "white", name = "log10(counts)") +
  labs(x = "Genus", y = "Sample type") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        panel.grid = element_blank())
ggsave(file=paste0("./results/Camacho_FigXXX-1_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)



