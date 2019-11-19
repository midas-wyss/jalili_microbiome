# PROCESS DATA #

# rename taxa ----
# sasan only data
new_taxa <- rename_taxa(phyloseq_otu_object = OTU)
tax_table(OTU) <- new_taxa
rm(new_taxa)

# full data (starter + media only)
new_taxa <- rename_taxa(phyloseq_otu_object = full_OTU)
tax_table(full_OTU) <- new_taxa
rm(new_taxa)

# need to rename human microbiome taxa as well, to correct NA to Unknown, for consistency
taxa <- tax_table(HMPv35)
new_taxa <- taxa
new_taxa <- apply(new_taxa, 2, function(x ){gsub("k__|p__|c__|o__|f__|g__", "", x)})
colnames(new_taxa) <- c("kingdom", "phylum", "class", "order", "family", "genus")

new_taxa[is.na(new_taxa[, 5]), 5] <- "Unknown"
new_taxa[which(new_taxa[, 5] == ""), 5] <- "Unknown"

new_taxa[is.na(new_taxa[, 6]), 6] <- "Unknown"
new_taxa[which(new_taxa[, 6] == ""), 6] <- "Unknown"

tax_table(HMPv35) <- new_taxa
rm(new_taxa)



# tidy data ----
message("Tidy data...")
# with alpha diversity out of the way, we can summarize data and tidy it up
tidy_otu <- tidy_phyloseq(OTU)
tidy_full <- tidy_phyloseq(full_OTU)
tidy_humans <- tidy_phyloseq(HMPv35)

# clean up otus ----
sings_otu <- tidy_singletons(tidy_phylo = tidy_otu)
sings_full <- tidy_singletons(tidy_phylo = tidy_full)
sings_humans <- tidy_singletons(tidy_phylo = tidy_humans)

tidy_otu$otu_table <- tidy_otu$otu_table[-sings_otu, ]
tidy_otu$taxonomy <- tidy_otu$taxonomy[-sings_otu, ]
tidy_full$otu_table <- tidy_full$otu_table[-sings_full, ]
tidy_full$taxonomy <- tidy_full$taxonomy[-sings_full, ]
tidy_humans$otu_table <- tidy_humans$otu_table[-sings_humans, ]
tidy_humans$taxonomy <- tidy_humans$taxonomy[-sings_humans, ]


# filter otus based on frequency next
freq_otu <- tidy_filter(tidy_phylo = tidy_otu, min_freq = 0.001, samp_freq = 0.05)
freq_full <- tidy_filter(tidy_phylo = tidy_full, min_freq = 0.001, samp_freq = 0.05)
freq_humans <- tidy_filter(tidy_phylo = tidy_humans, min_freq = 0.001, samp_freq = 0.05)

tidy_otu$otu_table <- tidy_otu$otu_table[freq_otu, ]
tidy_otu$taxonomy <- tidy_otu$taxonomy[freq_otu, ]
tidy_full$otu_table <- tidy_full$otu_table[freq_full, ]
tidy_full$taxonomy <- tidy_full$taxonomy[freq_full, ]
tidy_humans$otu_table <- tidy_human$otu_table[freq_humans, ]
tidy_humans$taxonomy <- tidy_human$taxonomy[freq_humans, ]



# SAVE DATA ----
# tidy_data <- list(no_hmb_ctr = tidy_otu, w_hmb_ctr = tidy_full, hmpv35 = tidy_humans)
# save(file=paste0(save_dir, "tidy_filtered_", format(Sys.Date(),"%Y-%m-%d"),".RData"), tidy_data)
# 
# untidy_unfil <- list(no_hmb_ctr = OTU, w_hmb_ctr = full_OTU, hmpv35 = HMPv35)
# save(file=paste0(save_dir, "unfiltered_data_", format(Sys.Date(),"%Y-%m-%d"),".RData"), untidy_unfil)
#
#
# save these tables
# save_dir <- "/Volumes/HOME/collaborations/ingber_lab/sasan_microbiome_oxygen/results/revisions/"
# save_dir <- "~/Desktop/jalili_revisions/data/"
# tab_otu <- as.matrix(tidy_otu$otu_table)
# colnames(tab_otu) <- paste(tidy_otu$samples$condition,"-- day",tidy_otu$samples$day,"-- replicate",tidy_otu$samples$replicate)
# tab_otu <- cbind(tidy_otu$taxonomy,tab_otu)
# write.csv(x = tab_otu,
#           file = paste0(save_dir,
#                         "otu-table-simple-filtered_",
#                         format(Sys.Date(),"%Y-%m-%d"),
#                         ".csv"),
#           quote = FALSE,
#           row.names = FALSE)
# 
# tab_full <- as.matrix(tidy_full$otu_table)
# colnames(tab_full) <- paste(tidy_full$samples$condition,"-- day",tidy_full$samples$day,"-- replicate",tidy_full$samples$replicate)
# tab_full <- cbind(tidy_full$taxonomy,tab_full)
# write.csv(x = tab_full,
#           file = paste0(save_dir,
#                         "otu-table-full-filtered_",
#                         format(Sys.Date(),"%Y-%m-%d"),
#                         ".csv"),
#           quote = FALSE,
#           row.names = FALSE)



# i'm showing the data below in a different manner in the differential abundance section  
# NORMALIZATION TO DAY 1 ----  
# normalize to day 1.  this will tell us which bugs go up or down over the course of the 3 days.
# ubacs <- unique(df2$genus)
# 
# xx <- df2$counts %>% matrix(., ncol = ncol(M$otu_table), nrow = length(ubacs))
# 
# norm_d1 <- vector(mode = "list", length = length(ubacs))
# for(i in seq(1,length(ubacs)))
# {
#   # get aerobic and anaerobic counts per bacterium for day 1
#   d1c_ae <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 1) %>% 
#     filter(., sample_group == "aerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum
#   
#   d1c_an <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 1) %>% 
#     filter(., sample_group == "anaerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum
#   
#   # normalize each day
#   a1_ae <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 1) %>% 
#     filter(., sample_group == "aerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_ae
#   
#   a1_an <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 1) %>% 
#     filter(., sample_group == "anaerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_an
# 
#   a2_ae <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 2) %>% 
#     filter(., sample_group == "aerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_ae
#   
#   a2_an <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 2) %>% 
#     filter(., sample_group == "anaerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_an
# 
#   a3_ae <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 3) %>% 
#     filter(., sample_group == "aerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_ae
#   
#   a3_an <- df2 %>% 
#     filter(., genus == ubacs[i]) %>% 
#     filter(., day == 3) %>% 
#     filter(., sample_group == "anaerobic") %>% 
#     dplyr::select(., counts) %>% 
#     sum / d1c_an
#   
#   ae_data <- c(a1_ae, a2_ae, a3_ae)
#   an_data <- c(a1_an, a2_an, a3_an)
# 
#   norm_d1[[i]] <- data_frame(bacterium = ubacs[i],
#                        day = rep(c(1,2,3), 2),
#                        condition = c(rep("aerobic", 3), rep("anaerobic", 3)),
#                        data = c(ae_data, an_data))
# }
# norm_d1 <- bind_rows(norm_d1)
# 
# norm_d1 %>% 
#   # filter(.,bacterium == "Sutterella") %>%
#   # filter(.,condition == "anaerobic") %>%
#   ggplot() + 
#   geom_point(aes(x = bacterium, y = data, color = factor(day)),size=3) +
#   # geom_boxplot() + 
#   # facet_grid(condition ~ bacterium,scales="free") +
#   facet_grid(~ condition, scales="free") +
#   # geom_jitter(width=1,size=2) +
#   # geom_point(position=position_jitter(),size=2) + 
#   # facet_grid(~ day) +
#   labs(y="Relative abundance",x="Bacterium") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 12, color = "black"),
#         axis.text.y = element_text(size=12, color = "black"),
#         axis.title = element_text(size=20, face = "bold", color = "black"),
#         panel.grid = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 12, color = "black"))
# 
