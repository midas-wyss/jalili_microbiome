sample_info <- as.data.frame(tidy_full$samples)
sample_info <- sample_info[-c(25,26), ]
counts <- as.matrix(summ_full$counts)
counts <- counts[, -25]

# with deseq2
dds <- DESeqDataSetFromMatrix(countData = counts, design = ~ cond_day, colData = sample_info)
xx2 <- DESeq2::DESeq(dds)
res <- results(xx2, cooksCutoff = FALSE)

fold_data <- data_frame(genus = rep(summ_full$bacteria, 3),
                        log2=c(results(xx2, contrast = c("cond_day", "anaerobic1", "aerobic1"))$log2FoldChange,
                               results(xx2, contrast = c("cond_day", "anaerobic2", "aerobic2"))$log2FoldChange,
                               results(xx2, contrast = c("cond_day", "anaerobic3", "aerobic3"))$log2FoldChange),
                        padj=c(results(xx2, contrast = c("cond_day", "anaerobic1", "aerobic1"))$padj,
                               results(xx2, contrast = c("cond_day", "anaerobic2", "aerobic2"))$padj,
                               results(xx2, contrast = c("cond_day", "anaerobic3", "aerobic3"))$padj),
                        day=c(rep(1, dim(xx2)[1]),
                              rep(2,dim(xx2)[1]),
                              rep(3,dim(xx2)[1])))



# trend ----
# look at the bugs that go up in anaerobic conditions versus those that go down
# this is comparing anaerobic vs aerobic conditions 
# ubac <- unique(fold_data$genus)
# ubac <- setdiff(ubac, "Unknown")
# 
# trans_state <- character(length = nrow(fold_data))
# mm_norm <- integer(length = nrow(fold_data))
# for(i in seq(1, length(ubac))) { 
#   a1 <- fold_data %>% 
#     dplyr::filter(., genus == ubac[i]) %>% 
#     dplyr::mutate(., minmax = (log2 - min(log2)) / (max(log2) - min(log2))  ) %>% 
#     dplyr::select(., minmax) %>% 
#     as.matrix 
#   
#   b1 <- sort(a1, decreasing = TRUE, index.return = TRUE)
#   
#   if (b1$ix[1] == 1) {
#     a2 <- which(fold_data$genus == ubac[i])
#     trans_state[a2] <- "decreasing"
#     mm_norm[a2] <- a1
#   } else if (b1$ix[1] == 2) { 
#     a2 <- which(fold_data$genus == ubac[i])
#     trans_state[a2] <- "transient"
#     mm_norm[a2] <- a1
#   } else if (b1$ix[1] == 3) {
#     a2 <- which(fold_data$genus == ubac[i])
#     trans_state[a2] <- "increasing"
#     mm_norm[a2] <- a1
#   }
#   
# }
# 
# fold_data <- fold_data %>% 
#   tibble::add_column(., trend_group = trans_state) %>% 
#   tibble::add_column(., minmax = mm_norm)
# 
# tg_labeler <- c("increasing" = "Increasing",
#                 "decreasing" = "Decreasing",
#                 "transient" = "Transient")
# 
# fold_data %>% 
#   tibble::add_column(., tg_f = factor(fold_data$trend_group, levels = c("decreasing", "transient", "increasing"))) %>% 
#   dplyr::filter(., genus != "Unknown") %>%
#   ggplot() + 
#   # geom_point(aes(x = factor(day), y = minmax, color = genus), size = 3) + 
#   geom_line(aes(x = day, y = minmax, color = genus), lwd = 1) +
#   facet_grid(. ~ tg_f, labeller = as_labeller(tg_labeler)) + 
#   labs(x = "Day", y = "min-max(log2(fold abundance))") + 
#   theme_bw() + 
#   theme(axis.title = element_text(size = 20, color = "black", face = "bold"),
#         axis.text = element_text(size = 12, color = "black"),
#         panel.grid = element_blank(),
#         strip.text = element_text(size = 12),
#         strip.background = element_blank())
# 
