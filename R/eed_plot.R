eed_df %>%
  # tibble::add_column(., s_id = paste(eed_df$tissue,
  #                                    eed_df$day,
  #                                    eed_df$dilution)) %>%
  dplyr::filter(., week != 0, dilution != 0) %>%
  ggplot() + 
  # geom_bar(aes(x = sample_name, y = relative_abundance, fill = genus), stat = "identity", color = "black") +
  geom_tile(aes(x = sample_name, y = genus, fill = relative_abundance), color = "black") +
  scale_fill_gradient2(low = "orange", high = "red", mid = "white") +
  # coord_equal() + 
  # geom_boxplot(aes(x = sample_name, y = log10(counts), fill = genus)) + 
  # scale_fill_manual(breaks = c("Unknown", "Sutterella", "Bilophila", "Akkermansia", "Blautia",
  #                              "Oscillospira", "Ruminococcus", "Enterococcus", "[Eubacterium]",
  #                              "Parabacteroides", "Bacteroides", "Citrobacter"),
  #                   values = c("#CCCCCC", "#FFCC00", "#99CCCC", "#33CC33", "#CC3399",
  #                              "#CC99CC", "#336699", "#66CCFF", "#669933",
  #                              "#FF6600", "#993300", "#CCCCFF"),
  #                   name = "Genus") + 
  labs(x = NULL, y = "Relative\nabundance") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, angle = 90, color = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid = element_blank(),
        legend.position = "right")
