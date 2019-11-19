# barplot on phylum
M <- tidy_correct_counts(tidy_phylo = tidy_full,control_sample_id = 26)
summ_full <- tidy_taxa(tidy_phylo = M,summary_level = "phylum")
relative_full <- relative_abundances(summary_matrix = summ_full$counts)

phylum_data <- data_frame(sample_name = as.vector(sapply(M$samples$filename, rep, nrow(summ_full$counts))),
                          sample_group = as.vector(sapply(M$samples$condition, rep, nrow(summ_full$counts))),
                          day=as.vector(sapply(M$samples$day,rep,nrow(summ_full$counts))),
                          replicate=as.vector(sapply(M$samples$replicate,rep,nrow(summ_full$counts))),
                          genus=rep(summ_full$bacteria,ncol(summ_full$counts)),
                          counts=as.vector(summ_full$counts),
                          relative_abundance=as.vector(relative_full))#,



phylum_data %>%
  # tibble::add_column(., s_id = factor(df2$sample_group, levels = c("Hmb11", "anaerobic", "aerobic"))) %>% 
  tibble::add_column(., p_id = gsub("NA \\w* \\(replicate NA\\)", "Hmb11", gsub(" )", ")", paste(phylum_data$sample_group, phylum_data$day, "(replicate", phylum_data$replicate, ")")))) %>% 
  ggplot() + 
  geom_bar(aes(x = p_id, y = relative_abundance, fill = genus), stat = "identity", color = "black") + 
  scale_fill_manual(breaks = c("Bacteroidetes", "Firmicutes", "Proteobacteria", "Verrucomicrobia"),
                    values = c("#CCCCCC", "#FFCC00", "#99CCCC", "#33CC33"),
                    name = "Phylum") +
  labs(x = "Sample", y = "Relative\nabundance") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, angle = 90, color = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.grid = element_blank())