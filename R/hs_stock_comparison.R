load("./data/2018-07-05_human_stock.RData")

hs_stock <- data[[2]]

# summarize data
summ_full <- tidy_taxa(tidy_phylo = hs_stock,summary_level = "genus")
relative_full <- relative_abundances(summary_matrix = summ_full$counts)

df4 <- data_frame(sample_name = as.vector(sapply(hs_stock$samples$filename, rep, nrow(summ_full$counts))),
                  dilution = c(rep(10, nrow(summ_full$counts)), rep(1000, nrow(summ_full$counts))),
                  genus = rep(summ_full$bacteria,ncol(summ_full$counts)),
                  counts = as.vector(summ_full$counts),
                  relative_abundance = as.vector(relative_full))


# merge df4 into df2 based on genus
df2_genus <- df2 %>% dplyr::select(., genus) %>% as.matrix %>% unique(.)
df4_genus <- df4 %>% dplyr::select(., genus) %>% as.matrix %>% unique(.)
common_genus <- setdiff(intersect(df2_genus, df4_genus), "Unknown")

tmp2 <- df2 %>% dplyr::filter(., genus %in% common_genus)
tmp4 <- df4 %>% dplyr::filter(., genus %in% common_genus)

df5 <- data_frame(genus = c(tmp2$genus, tmp4$genus),
                  sample_group = c(tmp2$sample_group, 
                                   rep("stock_10", length(common_genus)),
                                   rep("stock_1000", length(common_genus))),
                  day = c(tmp2$day, 
                          rep(NA, length(common_genus)),
                          rep(NA, length(common_genus))),
                  replicate = c(tmp2$replicate, 
                                rep(NA, length(common_genus)),
                                rep(NA, length(common_genus))),
                  counts = c(tmp2$counts, 
                             tmp4$counts),
                  relative_abundance = c(tmp2$relative_abundance, 
                                         tmp4$relative_abundance))

ubac <- unique(df5$genus)

# normalize 1:1000
bac_plots_norm1000 <- vector(mode = "list", length = length(ubac))
for (i in seq(1, length(ubac))) {
  
  a1 <- df5$counts[df5$genus == ubac[i] & df5$sample_group == "stock_1000"]
  a2 <- df5$counts[df5$genus == ubac[i] & df5$sample_group != "stock_1000"]
  if (a1 != 0) {
    xx1 <-  a2 / a1 
    
    bac_plots_norm1000[[i]] <- df5 %>% 
      dplyr::filter(., genus == ubac[i], sample_group != "stock_1000") %>%
      tibble::add_column(., norm_stock1000 = xx1) %>%
      dplyr::filter(., genus == ubac[i], sample_group != "stock_10", sample_group != "Hmb11") %>%
      # dplyr::filter(., sample_group != "Hmb11") %>%
      ggplot() +
      geom_boxplot(aes(x = day, y = norm_stock1000, fill = sample_group)) + 
      geom_point(aes(x = day, y = norm_stock1000, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                        breaks = c("aerobic", "anaerobic", "Hmb11"),
                        labels = c("Aerobic", "Anaerobic", "Human stock"),
                        name = "Condition") +
      geom_hline(yintercept = 1, lty = 2, color = "red", lwd = 1) +
      labs(x = "Day", y = "GoC / stock 1:1000", title = ubac[i]) + 
      theme_bw() + 
      theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
            axis.text = element_text(size = 12, color = "black"),
            panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
    
  } else {
    xx1 <-  a2
    
    bac_plots_norm1000[[i]] <- df5 %>% 
      dplyr::filter(., genus == ubac[i], sample_group != "stock_1000") %>%
      tibble::add_column(., norm_stock1000 = xx1) %>%
      dplyr::filter(., genus == ubac[i], sample_group != "stock_10", sample_group != "Hmb11") %>%
      # dplyr::filter(., sample_group != "Hmb11") %>%
      ggplot() +
      geom_boxplot(aes(x = day, y = norm_stock1000, fill = sample_group)) + 
      geom_point(aes(x = day, y = norm_stock1000, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                        breaks = c("aerobic", "anaerobic", "Hmb11"),
                        labels = c("Aerobic", "Anaerobic", "Human stock"),
                        name = "Condition") +
      geom_hline(yintercept = 0, lty = 2, color = "red", lwd = 1) +
      labs(x = "Day", y = "GoC / stock 1:1000", title = ubac[i]) + 
      theme_bw() + 
      theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
            axis.text = element_text(size = 12, color = "black"),
            panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
    
  }
}

# normalize to stock 1:10
bac_plots_norm10 <- vector(mode = "list", length = length(ubac))
for (i in seq(1, length(ubac))) {
  
  a1 <- df5$counts[df5$genus == ubac[i] & df5$sample_group == "stock_10"]
  a2 <- df5$counts[df5$genus == ubac[i] & df5$sample_group != "stock_10"]
  if (a1 != 0) {
    xx1 <-  a2 / a1 
    
    bac_plots_norm10[[i]] <- df5 %>% 
      dplyr::filter(., genus == ubac[i], sample_group != "stock_10") %>%
      tibble::add_column(., norm_stock1000 = xx1) %>%
      dplyr::filter(., genus == ubac[i], sample_group != "stock_1000", sample_group != "Hmb11") %>%
      # dplyr::filter(., sample_group != "Hmb11") %>%
      ggplot() +
      geom_boxplot(aes(x = day, y = norm_stock1000, fill = sample_group)) + 
      geom_point(aes(x = day, y = norm_stock1000, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                        breaks = c("aerobic", "anaerobic", "Hmb11"),
                        labels = c("Aerobic", "Anaerobic", "Human stock"),
                        name = "Condition") +
      geom_hline(yintercept = 1, lty = 2, color = "red", lwd = 1) +
      labs(x = "Day", y = "GoC / stock 1:10", title = ubac[i]) + 
      theme_bw() + 
      theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
            axis.text = element_text(size = 12, color = "black"),
            panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
    
  } else {
    xx1 <-  a2
    
    bac_plots_norm10[[i]] <- df5 %>% 
      dplyr::filter(., genus == ubac[i], sample_group != "stock_10") %>%
      tibble::add_column(., norm_stock1000 = xx1) %>%
      dplyr::filter(., genus == ubac[i], sample_group != "stock_1000", sample_group != "Hmb11") %>%
      # dplyr::filter(., sample_group != "Hmb11") %>%
      ggplot() +
      geom_boxplot(aes(x = day, y = norm_stock1000, fill = sample_group)) + 
      geom_point(aes(x = day, y = norm_stock1000, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                        breaks = c("aerobic", "anaerobic", "Hmb11"),
                        labels = c("Aerobic", "Anaerobic", "Human stock"),
                        name = "Condition") +
      geom_hline(yintercept = 0, lty = 2, color = "red", lwd = 1) +
      labs(x = "Day", y = "GoC / stock 1:10", title = ubac[i]) + 
      theme_bw() + 
      theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
            axis.text = element_text(size = 12, color = "black"),
            panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
    
  }
}