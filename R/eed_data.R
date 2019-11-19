library(readxl)

eed_OTU <- import_biom(BIOMfilename = "/Volumes/HOME/collaborations/ingber_lab/microbiome_eed/data/processed/otu_table-json.biom")
eed_samples <- read_csv(file = "~/work/projects/eed_microbiome_samples.csv")
x <- as.data.frame(eed_samples)
rownames(x) <- colnames(otu_table(eed_OTU))

new_taxa <- rename_taxa(phyloseq_otu_object = eed_OTU)
tax_table(eed_OTU) <- new_taxa
rm(new_taxa)

a1 <- eed_samples %>% 
  dplyr::filter(., 
                sample_type == "chip", 
                day == 3, 
                microbiome == 1, 
                dilution == 1000)

b1 <- c("Wyss.Exp3C.2", "Wyss.Exp3C.3")

M <- as.matrix(otu_table(eed_OTU)[, colnames(otu_table(eed_OTU)) %in% b1])


a2 <- eed_samples %>% 
  dplyr::filter(., 
                sample_type == "chip", 
                tissue == "ileum",
                microbiome == 1, dilution != 0)

b1 <- a2 %>% dplyr::select(., sample_id) %>% as.matrix 
M <- as.matrix(otu_table(eed_OTU)[, colnames(otu_table(eed_OTU)) %in% b1])
apply(M, 2, function(y) length(which(y != 0)))

sort(unique(c(unique(tax_table(eed_OTU)[which(M[, 1] != 0),6]),
              unique(tax_table(eed_OTU)[which(M[, 2] != 0),6]),
              unique(tax_table(eed_OTU)[which(M[, 3] != 0),6]),
              unique(tax_table(eed_OTU)[which(M[, 4] != 0),6]))))

# richness estimates
eed_richness <- estimate_richness(otu_table(eed_OTU))
xx <- eed_samples %>% dplyr::filter(., tissue == "ileum") %>% dplyr::select(., sample_id) %>% as.matrix

xx2 <- eed_samples %>% 
  dplyr::filter(., day == 0, sample_type == "chip", tissue != "stock") %>% 
  dplyr::select(., sample_id) %>% 
  as.matrix %>% 
  as.vector

xx3 <- eed_samples %>% 
  dplyr::filter(., day == 5, sample_type == "chip", tissue != "stock", tissue == "ileum") %>% 
  dplyr::select(., sample_id) %>% 
  as.matrix %>% 
  as.vector

xx4 <- eed_samples %>% 
  dplyr::filter(., day == 0, sample_type == "chip", tissue != "stock", tissue == "duodenum") %>% 
  dplyr::select(., sample_id) %>% 
  as.matrix %>% 
  as.vector


# tidy eed microbiome
eed_tidy <- tidy_phyloseq(phyloseq_obj = eed_OTU)
sings_eed <- tidy_singletons(tidy_phylo = eed_tidy)
eed_tidy$otu_table <- eed_tidy$otu_table[-sings_eed, ]
eed_tidy$taxonomy <- eed_tidy$taxonomy[-sings_eed, ]
freq_eed <- tidy_filter(tidy_phylo = eed_tidy, min_freq = 0.001, samp_freq = 0.05)
eed_tidy$otu_table <- eed_tidy$otu_table[freq_eed, ]
eed_tidy$taxonomy <- eed_tidy$taxonomy[freq_eed, ]
M <- tidy_correct_counts(tidy_phylo = eed_tidy, control_sample_id = 80)

summ_eed <- tidy_taxa(tidy_phylo = M,summary_level = "phylum")
relative_eed <- relative_abundances(summary_matrix = summ_eed$counts)

eed_df <- data_frame(sample_name = as.vector(sapply(M$samples$sample_id, rep, nrow(summ_eed$counts))),
                     tissue = as.vector(sapply(M$samples$tissue, rep, nrow(summ_eed$counts))),
                     sample_type = as.vector(sapply(M$samples$sample_type, rep, nrow(summ_eed$counts))),
                     day = as.vector(sapply(M$samples$day, rep, nrow(summ_eed$counts))),
                     week = as.vector(sapply(M$samples$week, rep, nrow(summ_eed$counts))),
                     dilution = as.vector(sapply(M$samples$dilution, rep, nrow(summ_eed$counts))),
                     replicate = as.vector(sapply(M$samples$replicate, rep, nrow(summ_eed$counts))),
                     media = as.vector(sapply(M$samples$media, rep, nrow(summ_eed$counts))),
                     group = as.vector(sapply(M$samples$group, rep, nrow(summ_eed$counts))),
                     infant_metadata = as.vector(sapply(M$samples$infant_metadata, rep, nrow(summ_eed$counts))),
                     genus = rep(summ_eed$bacteria,ncol(summ_eed$counts)),
                     counts = as.vector(summ_eed$counts),
                     relative_abundance = as.vector(relative_eed))

# Figure S6: Barplot for relative abundances across all samples
eed_df %>%
  tibble::add_column(., s_id = paste(eed_df$tissue, ":: day",
                                     eed_df$day, ":: dilution",
                                     eed_df$dilution, ":: replicate",
                                     eed_df$replicate, ":: week", 
                                     eed_df$week)) %>%
  dplyr::filter(., tissue == "ileum", day > 1, dilution != 0) %>%
  # dplyr::filter(., day >= 3, dilution != 0) %>%
  # tibble::add_column(., p_id = gsub("Hmb11 \\w* \\(replicate NA\\)", "Hmb11", gsub(" )", ")",paste(df2$sample_group,df2$day,"(replicate",df2$replicate,")")))) %>% 
  ggplot() + 
  geom_bar(aes(x = s_id, y = relative_abundance, fill = genus), stat = "identity", color = "black") +
  # geom_boxplot(aes(x = sample_name, y = log10(counts), fill = genus)) + 
  scale_fill_manual(breaks = c("Actinobacteria", "Bacteroidetes", "Cyanobacteria", "Firmicutes", "Proteobacteria",
                               "Tenericutes"),
                    values = c("#CCCCCC", "#FFCC00", "#99CCCC", "#33CC33", "#CC3399",
                               "#CC99CC"),
                    name = "Phylum") +
  labs(x = NULL, y = "Relative\nabundance") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 32, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, angle = 90, color = "black", hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 24, color = "black"),
        panel.grid = element_blank(),
        legend.position = "right")
ggsave(file=paste0("./results/Camacho_Supp-FigXXX_",format(Sys.Date(),"%Y-%m-%d"),".pdf"), 
       width = 11, 
       height = 8, 
       units = "in", 
       dpi = 600)


grep("Wyss.Exp1B", rownames(eed_richness))
