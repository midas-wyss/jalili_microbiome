X <- import_biom("/Volumes/THoR/omics/data/microbial_community_sequencing/organ_chip/processed/media_optimization/otu_table-json.biom")


# get alessio's samples
f24_samples <- grep("F24",colnames(otu_table(X)))
hs_stock <- grep("Hmb1$",colnames(otu_table(X)))
aOTU <- prune_samples(samples = colnames(otu_table(X))[c(f24_samples,hs_stock)],X)

cols <- gsub(".F24","",gsub("Wyss.","",colnames(otu_table(aOTU))))
sample_id <- c(1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,"Hmb1")
media <- c(rep("20% FBS 4g/L glucose",3),rep("Mal-suco 0.12 g/L",3),rep("Pectin 0.5 g/L",3),rep("Starch 0.5 g/L",3),rep("Mucus 0.5 g/l",3),"Mal-suco 0.12 g/L","Human microbiome stock")

sample_metadata <- data.frame(sample_names=colnames(otu_table(aOTU)),
                              sample_id=cols,
                              media=media[match(cols,sample_id)])
rownames(sample_metadata) <- sample_metadata$sample_names
sample_data(aOTU) <- sample_metadata

new_taxa <- rename_taxa(phyloseq_otu_object = aOTU)
tax_table(aOTU) <- new_taxa
rm(new_taxa)


# tidy otu table
xx <- tidy_phyloseq(phyloseq_obj = aOTU)

# M2 <- tidy_correct_counts(tidy_phylo = xx, control_sample_id = 25)
summ_full_2 <- tidy_taxa(tidy_phylo = xx,summary_level = "genus")
relative_full_2 <- relative_abundances(summary_matrix = summ_full_2$counts)

# hmb1 sample
id <- 8

count_mat <- cbind(summ_full$counts, summ_full_2$counts[match(summ_full$bacteria, summ_full_2$bacteria), id])
rel_mat <- apply(count_mat, 2, function(y) y/sum(y))
s <- rbind(as.matrix(M$samples), c("Hmb1", "Hmb1", "Undiluted stock", NA, NA, NA, NA, NA))
s[25, 4] <- "Hmb11"
s[26, 4] <- "Hmb1"
s[25, 5] <- "Hmb11"
s[26, 5] <- "Hmb1"
s[25, 6] <- "Hmb11"
s[26, 6] <- "Hmb1"

s <- as.data.frame(s)


df4 <- data_frame(sample_name = as.vector(sapply(s$filename, rep, nrow(count_mat))),
                  sample_group = as.vector(sapply(s$condition, rep, nrow(count_mat))),
                  day = as.vector(sapply(s$day, rep, nrow(count_mat))),
                  replicate = as.vector(sapply(s$replicate, rep, nrow(count_mat))),
                  genus = rep(summ_full$bacteria, ncol(count_mat)),
                  counts = as.vector(count_mat),
                  relative_abundance = as.vector(rel_mat))


df4 %>%
  tibble::add_column(., p_id = gsub("Hmb11 \\w* \\(replicate NA\\)", "Hmb11", gsub("Hmb1 \\w* \\(replicate NA\\)", "Hmb1", gsub(" )", ")", paste(df4$sample_group, df4$day, "(replicate", df4$replicate, ")"))))) %>%
  # tibble::add_column(., p_id = gsub("Hmb11 \\w* \\(replicate NA\\)", "Hmb11", gsub(" )", ")", paste(df4$sample_group, df4$day, "(replicate", df4$replicate, ")")))) %>%
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