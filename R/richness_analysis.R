# richness analysis ----
message("Richness analysis...")

richness_data <- phyloseq::estimate_richness(full_OTU)
richness_data <- data_frame(metric = as.vector(sapply(colnames(richness_data), rep, nrow(richness_data))),
                            alpha_diversity = as.vector(as.matrix(richness_data)),
                            sample_name = as.vector(t(sapply(rownames(richness_data), rep, ncol(richness_data)))),
                            condition = as.vector(t(sapply(sample_data(full_OTU)[, 6], rep, ncol(richness_data)))),
                            replicate = as.vector(t(sapply(sample_data(full_OTU)[, 7], rep, ncol(richness_data)))),
                            day = as.vector(t(sapply(sample_data(full_OTU)[, 5], rep, ncol(richness_data)))),
                            cond_day = as.vector(t(sapply(sample_data(full_OTU)[, 8], rep, ncol(richness_data)))))


# show what happens compared to the starter set
x1 <- data_frame(group = c("starter",
                           "aerobic_1", "aerobic_2", "aerobic_3",
                           "anaerobic_1", "anaerobic_2", "anaerobic_3"),
                 metric = rep("Observed", 7),
                 richness_mean = c(richness_data %>% dplyr::filter(., sample_name == "Wyss.Hmb11", metric == "Observed") %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% as.vector,
                                   richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 1) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean,
                                   richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 2) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean,
                                   richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 3) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean,
                                   richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 1) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean,
                                   richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 2) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean,
                                   richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 3) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% mean),
                 richness_sd = c(0,
                                 richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 1) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd,
                                 richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 2) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd,
                                 richness_data %>% dplyr::filter(., condition == "aerobic", metric == "Observed", day == 3) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd,
                                 richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 1) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd,
                                 richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 2) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd,
                                 richness_data %>% dplyr::filter(., condition == "anaerobic", metric == "Observed", day == 3) %>% dplyr::select(., alpha_diversity) %>% as.matrix %>% sd))

x1 %>% 
  ggplot() + 
  geom_bar(aes(x = group, y = richness_mean, fill = group), stat = "identity", color = "black") + 
  scale_fill_manual(values = c(rep("#006699",3), rep("#00CCFF", 3), "#666666"),
                    breaks = c(),
                    labels = NULL,
                    name = NULL) + 
  labs(x = NULL, y = "Average richness") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 20, face = "bold", color = "black"))



# add human data
rich_human <- phyloseq::estimate_richness(HMPv35)
stid <- sample_data(HMPv35)[which(sample_data(HMPv35)[, 6] == "Stool"), 1]
rich_human <- rich_human[which(gsub("X", "", rownames(rich_human)) %in% stid$X.SampleID), ]

richness_data <- data_frame(metric = as.vector(sapply(colnames(richness_data), rep, nrow(richness_data))),
                            alpha_diversity = as.vector(as.matrix(richness_data)),
                            sample_name = as.vector(t(sapply(rownames(richness_data), rep, ncol(richness_data)))),
                            condition = as.vector(t(sapply(sample_data(full_OTU)[, 6], rep, ncol(richness_data)))),
                            replicate = as.vector(t(sapply(sample_data(full_OTU)[, 7], rep, ncol(richness_data)))),
                            day = as.vector(t(sapply(sample_data(full_OTU)[, 5], rep, ncol(richness_data)))),
                            cond_day = as.vector(t(sapply(sample_data(full_OTU)[, 8], rep, ncol(richness_data)))))

combo_richness <- data_frame(metric = c(richness_data$metric, 
                                        as.vector(sapply(colnames(rich_human), rep, nrow(rich_human)))),
                             alpha_diversity = c(richness_data$alpha_diversity,
                                                 as.vector(as.matrix(rich_human))),
                             sample_name = c(richness_data$sample_name, 
                                             rep("human_stool", nrow(rich_human) * ncol(rich_human))),
                             condition = c(richness_data$condition, 
                                           rep("human_stool", nrow(rich_human) * ncol(rich_human))),
                             replicate = c(richness_data$replicate,
                                           rep(1, nrow(rich_human) * ncol(rich_human))),
                             day = c(richness_data$day,
                                     rep(NA, nrow(rich_human) * ncol(rich_human))),
                             cond_day = c(richness_data$cond_day,
                                          rep(1, nrow(rich_human) * ncol(rich_human))))




