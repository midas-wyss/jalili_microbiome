# Figure XXX-3: All genera, similar to XXX-2.
bac_plots <- vector(mode = "list", length = length(ubac))
for (i in seq(1, length(ubac))) {
  bac_plots[[i]] <- df2 %>% 
    dplyr::filter(., genus == ubac[i], sample_group != "Hmb11") %>%
    # dplyr::filter(., sample_group != "Hmb11") %>%
    ggplot() +
    geom_boxplot(aes(x = day, y = counts, fill = sample_group)) + 
    geom_point(aes(x = day, y = counts, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                      breaks = c("aerobic", "anaerobic", "Hmb11"),
                      labels = c("Aerobic", "Anaerobic", "Starting sample"),
                      name = "Condition") +
    geom_hline(yintercept = df2$counts[df2$sample_group == "Hmb11" & df2$genus == ubac[i]], lty = 2, color = "red", lwd = 1) +
    labs(x = "Day", y = "Total counts", title = ubac[i]) + 
    theme_bw() + 
    theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
}


bac_plots_norm <- vector(mode = "list", length = length(ubac))
for (i in seq(1, length(ubac))) {
  
  xx1 <- df2$counts[df2$genus == ubac[i] & df2$sample_group != "Hmb11"] / df2$counts[df2$genus == ubac[i] & df2$sample_group == "Hmb11"]
  
  bac_plots_norm[[i]] <- df2 %>% 
    dplyr::filter(., genus == ubac[i], sample_group != "Hmb11") %>%
    tibble::add_column(., norm_hmb11 = xx1) %>%
    # dplyr::filter(., sample_group != "Hmb11") %>%
    ggplot() +
    geom_boxplot(aes(x = day, y = norm_hmb11, fill = sample_group)) + 
    geom_point(aes(x = day, y = norm_hmb11, fill = sample_group), pch = 21, size = 2, color = "black", position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("#006699", "#00CCFF", "#CCCCCC"),
                      breaks = c("aerobic", "anaerobic", "Hmb11"),
                      labels = c("Aerobic", "Anaerobic", "Starting sample"),
                      name = "Condition") +
    geom_hline(yintercept = 1, lty = 2, color = "red", lwd = 1) +
    labs(x = "Day", y = "Total counts", title = ubac[i]) + 
    theme_bw() + 
    theme(axis.title = element_text(size = 20, face = "bold", color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank(), title = element_text(size = 20, face = "bold"))
}


