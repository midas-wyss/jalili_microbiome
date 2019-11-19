# FIGURE 4 ----

# Figure 4a: Changes in genera in the anaerobic chip across days
df2 %>% 
  dplyr::filter(., genus != "Unknown", sample_group == "anaerobic") %>% 
  ggplot() + 
  geom_boxplot(aes(x = genus, y = log10(counts), fill = day), position = "dodge", outlier.size = 0) +
  geom_point(aes(x = genus, y = log10(counts), fill = day), pch = 21, size = 2, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#FFCC00", "#FF3300", "#996666"),
                     breaks = c(1, 2, 3),
                     labels = c("Day 1", "Day 2", "Day 3"),
                     name = NULL) +
  # scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"), 
  #                   breaks = c("1", "2", "3"),
  #                   labels = c("Day 1", "Day 2", "Day 3"),
  #                   name = NULL) + 
  # labs(y="log10(counts)",x="Genus") +
  labs(y = "log10(counts)",x = NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 24, color = "black", face = "bold"),
        axis.text.y = element_text(size = 24, color = "black", face = "bold"),
        axis.title = element_text(size = 32, face = "bold", color = "black"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 18, face = "bold"))
ggsave(file=paste0("./results/Camacho_Fig4a_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)


# Figure 4b: Comparison of chip (anaerobic or aerobic) to liquid culture
a1 <- which(df2$sample_group == "anaerobic" & df2$day == 3)
a2 <- which(df2$sample_group == "aerobic" & df2$day == 3)
ctr <- which(df2$sample_group == "Hmb11")

growth_df <- data_frame(genus = c(as.character(df2$genus[a1]),
                                  as.character(df2$genus[a2])),
                        diff_abundance = c(df2$counts[a1]/df2$counts[ctr],
                                           df2$counts[a2]/df2$counts[ctr]),
                        condition = c(rep("anaerobic",length(a1)),rep("aerobic",length(a2))))

growth_df$diff_abundance[growth_df$diff_abundance == 0] <- 1

growth_df %>% 
  dplyr::filter(., genus != "Unknown") %>%
  ggplot() + 
  geom_boxplot(aes(x = genus, y = log10(diff_abundance), fill = as.factor(condition)), outlier.size = 0) +
  geom_point(aes(x = genus, y = log10(diff_abundance), fill = as.factor(condition)), pch = 21, size = 2, position = position_dodge(width = 0.75)) +
  # geom_dotplot(aes(x = genus, y = log10(diff_abundance), fill = as.factor(condition)), stackdir = "center") +
  scale_fill_manual(values = c("#006699", "#00CCFF"), 
                    breaks = c("aerobic", "anaerobic"),
                    labels = c("Aerobic", "Anaerobic"),
                    name = NULL) + 
  labs(y="log10(Differential abundance)",x="Genus") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5,size=18,color="black"),
        axis.text.y = element_text(size=18,color="black"),
        axis.title = element_text(size=20,face="bold",color="black"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12,color="black"))
ggsave(file=paste0("./results/Camacho_Fig4b_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)

# 4b with 3 groups
a1 <- which(df2$sample_group == "anaerobic" & df2$day == 3)
a2 <- which(df2$sample_group == "aerobic" & df2$day == 3)
ctr <- which(df2$sample_group == "Hmb11")

growth_df <- df2[c(a1, a2, ctr), ]
growth_df$counts[is.infinite(log10(growth_df$counts))] <- 1

growth_df %>% 
  dplyr::filter(., genus != "Unknown") %>% 
  dplyr::filter(., day == 3 | day == "Hmb11") %>%
  ggplot() + 
  geom_boxplot(aes(x = genus, y = log10(counts), fill = as.factor(sample_group)), outlier.size = 0) +
  geom_point(aes(x = genus, y = log10(counts), fill = as.factor(sample_group)), pch = 21, size = 2, position = position_dodge(width = 0.75)) +
  # geom_dotplot(aes(x = genus, y = log10(diff_abundance), fill = as.factor(condition)), stackdir = "center") +
  scale_fill_manual(values = c("#006699", "#00CCFF", "#cc9966"), 
                    breaks = c("aerobic", "anaerobic", "Hmb11"),
                    labels = c("Aerobic", "Anaerobic", "Liquid culture"),
                    name = NULL) + 
  labs(y="log10(counts)",x=NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 24, color = "black", face = "bold"),
        axis.text.y = element_text(size = 24, color = "black", face = "bold"),
        axis.title = element_text(size = 32, face = "bold", color="black"),
        panel.grid = element_blank(),
        # strip.background = element_blank(),
        # strip.text = element_text(size=12,color="black"),
        legend.text = element_text(size = 18, face = "bold"))
ggsave(file=paste0("./results/Camacho_Fig4b_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)


# Figure 4c: Differential abundance of genera between the two oxygen conditions
fold_data %>% 
  dplyr::filter(., genus != "Unknown") %>%
  ggplot() + 
  geom_point(aes(x = fct_reorder(genus, log2, .desc = TRUE), y = log2, color = as.factor(day)), size = 7) + 
  scale_color_manual(values = c("#FFCC00", "#FF3300", "#996666"),
                     breaks = c(1, 2, 3),
                     labels = c("Day 1", "Day 2", "Day 3"),
                     name = NULL) + 
  labs(x = NULL, y = "log2(fold difference)") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 24, color = "black", face = "bold", hjust = 1, vjust = 0.5, angle = 90),
        axis.text.y = element_text(size = 24, color = "black", face = "bold"),
        axis.title = element_text(size = 32, face = "bold", color = "black"),
        panel.grid = element_blank(),
        legend.text = element_text(size = 18, face = "bold"))
ggsave(file=paste0("./results/Camacho_Fig4c_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)