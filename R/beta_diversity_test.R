# PERMANOVA for beta diversity
# test for beta-diversity (between groups diversity)
# global
# get data from phyloseq obj
# metadata <- as(sample_data(sasan_otu_final), "data.frame")
# metadata <- as(sample_data(full_OTU), "data.frame")
metadata <- data.frame(tidy_otu$samples)

counts <- as.matrix(tidy_otu$otu_table)
d <- vegan::vegdist(x = t(counts), method = "bray") # <-- distance between samples!
# d <- vegan::vegdist(x = counts, method = "bray")

permanova_stats <- adonis(formula = t(counts) ~ condition,
                          data = metadata,
                          method = "bray",
                          permutations = 1000)

# per day
# day 1
x1 <- metadata[which(metadata$cond_day == "aerobic1" | metadata$cond_day == "anaerobic1"), ]
y1 <- counts[, which(colnames(counts) %in% x1$filename)]
permanova_day1 <- adonis(formula = t(y1) ~ condition,
                         data = x1,
                         method = "bray",
                         permutations = 1000)

x1 <- metadata[which(metadata$cond_day == "aerobic2" | metadata$cond_day == "anaerobic2"), ]
y1 <- counts[, which(colnames(counts) %in% x1$filename)]
permanova_day2 <- adonis(formula = t(y1) ~ condition,
                         data = x1,
                         method = "bray",
                         permutations = 1000)

x1 <- metadata[which(metadata$cond_day == "aerobic3" | metadata$cond_day == "anaerobic3"), ]
y1 <- counts[, which(colnames(counts) %in% x1$filename)]
permanova_day3 <- adonis(formula = t(y1) ~ condition,
                         data = x1,
                         method = "bray",
                         permutations = 1000)


# PERMANOVA on human samples vs anaerobic chip data
all_phyla <- unique(c(tidy_otu$taxonomy$Genus,
               tidy_humans$taxonomy$genus))

d1 <- matrix(0, nrow = length(all_phyla), ncol = ncol(tidy_otu$otu_table))
d2 <- matrix(0, nrow = length(all_phyla), ncol = ncol(tidy_humans$otu_table))
for (i in seq(1, length(all_phyla))) {
  y1 <- which(tidy_otu$taxonomy$Genus == all_phyla[i])
  y2 <- which(tidy_humans$taxonomy$genus == all_phyla[i])
  
  if (length(y1) > 1) {
    c1 <- Matrix::colSums(tidy_otu$otu_table[y1, ]) 
    d1[i, ] <- c1
  } else if (length(y1) == 1) {
    d1[i, ] <- tidy_otu$otu_table[y1, ]
  }
  
  if (length(y2) > 1) {
    c2 <- Matrix::colSums(tidy_humans$otu_table[y2, ])  
    d2[i, ] <- c2
  } else if (length(y2) == 1) {
    d2[i, ] <- tidy_humans$otu_table[y2, ]
  }
}

d3 <- cbind(d1, d2)
colnames(d3) <- c(tidy_otu$samples$filename,
                  tidy_humans$samples$X.SampleID)

# permanova against human samples ----
d <- vegan::vegdist(x = t(counts), method = "bray") # <-- distance between samples!

# random sampling of human data (don't need all the possible samples)
permanova_human <- vector(mode = "list", length = 1000)

for (i in 1:1000) {
  sid <- sample(x = ncol(d2), size = ncol(d1), replace = FALSE)
  
  metadata <- data.frame(filename = c(tidy_otu$samples$filename,
                                      paste0("X", tidy_humans$samples$X.SampleID[sid])),
                         condition = c(tidy_otu$samples$condition,
                                       rep("human_stool", length(sid))))
  
  d3 <- cbind(d1, d2[, sid])
  colnames(d3) <- c(as.character(tidy_otu$samples$filename),
                    paste0("X", tidy_humans$samples$X.SampleID[sid]))
  counts <- d3
  
  
  x1 <- metadata[which(metadata$condition == "aerobic" | metadata$condition == "human_stool"), ]
  y1 <- counts[, which(colnames(counts) %in% x1$filename)]
  human_aerobic_permanova <- adonis(formula = t(counts) ~ condition,
                                    data = metadata,
                                    method = "bray",
                                    permutations = 1000)
  
  x1 <- metadata[which(metadata$condition == "anaerobic" | metadata$condition == "human_stool"), ]
  y1 <- counts[, which(colnames(counts) %in% x1$filename)]
  human_anaerobic_permanova <- adonis(formula = t(counts) ~ condition,
                                      data = metadata,
                                      method = "bray",
                                      permutations = 1000)
  
  permanova_human[[i]] <- data_frame(anaerobic = human_anaerobic_permanova$aov.tab$`Pr(>F)`[1],
                                     aerobic = human_aerobic_permanova$aov.tab$`Pr(>F)`[1])
  
}
permanova_human <- dplyr::bind_rows(permanova_human)

