# FIGURE 3 ----
fig_dev <- "tiff"

# Figure 3a: Diversity analysis 
strip_labels <- c("1" = "Day 1",
                  "2" = "Day 2",
                  "3" = "Day 3")

richness_data %>% 
  dplyr::mutate(., condition = replace(condition, condition == "aerobic", "Aerobic")) %>% 
  dplyr::mutate(., condition = replace(condition, condition == "anaerobic", "Anaerobic")) %>%
  dplyr::filter(., metric == "Observed", !is.na(condition)) %>% 
  ggplot() + 
  geom_boxplot(aes(x = condition, y = alpha_diversity), outlier.size = 0) +
  geom_point(aes(x = condition, y = alpha_diversity, fill = condition), pch = 21, size = 7, color = "black") +
  scale_fill_manual(values = c("#006699", "#00CCFF"), breaks = c("aerobic", "anaerobic")) + 
  labs(x = NULL, y = "Observed richness") + 
  facet_wrap(~ day, labeller = as_labeller(strip_labels), scales = "free") +
  scale_y_continuous(limits = c(100, 300)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 24, face = "bold", color = "black"),
        axis.text.y = element_text(size = 24, color = "black", face = "bold"),
        axis.text.x = element_text(size = 24, color = "black", face = "bold", angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 32, face = "bold", color = "black"),
        axis.ticks = element_line(color = "black", size = 1),
        axis.ticks.length = unit(0.1, "in"),
        legend.position = "none")
ggsave(file=paste0("results/", format(Sys.Date(),"%Y-%m-%d"), "_Camacho_Fig3a.", fig_dev),
       device = fig_dev,
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)



# Figure 3c: Comparison to human stool
df3 %>% 
  # tibble::add_column(., min_val = bstat[, 1], q1_val = bstat[, 2], mid_val = bstat[, 3], q2_val = bstat[, 4], max_val = bstat[, 5]) %>%
  dplyr::mutate(., counts = replace(counts, counts == 0, 1)) %>%
  dplyr::filter(., sample != "Hmb11", genus != "Unknown") %>%
  ggplot() + 
  geom_boxplot(aes(x = genus, y = log10(counts), fill = sample), outlier.size = 0) +
  geom_point(aes(x = genus, y = log10(counts), fill = sample), position = position_dodge(width = 0.75), pch = 21, color = "black", size = 4) +
  scale_color_manual(values = c("#006699", "#00CCFF", "#FF3300"),
                     breaks = c("Aerobic", "Anaerobic", "Human stool"),
                     labels = c("Aerobic", "Anaerobic", "Human stool"),
                     name = "Sample type") +
  scale_fill_manual(values = c("#006699", "#00CCFF", "#FF3300"),
                    breaks = c("Aerobic", "Anaerobic", "Human stool"),
                    labels = c("Aerobic", "Anaerobic", "Human stool"),
                    name = NULL) +
  scale_shape_manual(values = c(0, 5, 15),
                     breaks = c("Aerobic", "Anaerobic", "Human stool")) + 
  labs(x = NULL, y = "log10(total counts)") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 24, color = "black", face = "bold"),
        axis.text.y = element_text(size = 24, color = "black", face = "bold"),
        axis.title = element_text(size = 32, face = "bold", color = "black"),
        panel.grid = element_blank(),
        legend.text = element_text(size = 18, face = "bold"))
ggsave(file=paste0("./results/Camacho_Fig3c_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)


