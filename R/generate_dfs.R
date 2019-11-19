# generate data frames for analysis ----


# SUMMARIZATION OF OTUS ----
message("Generate data frames...")
# summarizing otus to genera level

# simple first, in which i don't correct otus based on what was on media
summ_otu <- tidy_taxa(tidy_phylo = tidy_otu, summary_level = "genus")
relative_otu <- relative_abundances(summary_matrix = summ_otu$counts)

df1 <- data_frame(bacteria=rep(summ_otu$bacteria,ncol(summ_otu$counts)),
                  counts=as.vector(summ_otu$counts),
                  relative_abundance=as.vector(relative_otu),
                  condition=as.vector(sapply(tidy_otu$samples$condition,rep,nrow(summ_otu$counts))),
                  day=as.vector(sapply(tidy_otu$samples$day,rep,nrow(summ_otu$counts))),
                  replicate=as.vector(sapply(tidy_otu$samples$replicate,rep,nrow(summ_otu$counts))),
                  plot_var=paste(condition,"-- day",day,"-- replicate",replicate))

# now let's correct with the reads found in the media alone
M <- tidy_correct_counts(tidy_phylo = tidy_full,control_sample_id = 26)
summ_full <- tidy_taxa(tidy_phylo = M,summary_level = "genus")
relative_full <- relative_abundances(summary_matrix = summ_full$counts)

df2 <- data_frame(sample_name = as.vector(sapply(M$samples$filename, rep, nrow(summ_full$counts))),
                  sample_group = as.vector(sapply(M$samples$condition, rep, nrow(summ_full$counts))),
                  day=as.vector(sapply(M$samples$day,rep,nrow(summ_full$counts))),
                  replicate=as.vector(sapply(M$samples$replicate,rep,nrow(summ_full$counts))),
                  genus=rep(summ_full$bacteria,ncol(summ_full$counts)),
                  counts=as.vector(summ_full$counts),
                  relative_abundance=as.vector(relative_full))#,
# condition=as.vector(sapply(M$samples$condition,rep,nrow(summ_full$counts))),
# plot_var=paste(condition,"-- day",day,"-- replicate",replicate))

df2$sample_name[is.na(df2$sample_name)] <- "Hmb11"
df2$sample_group[is.na(df2$sample_group)] <- "Hmb11"
df2$day[is.na(df2$day)] <- "Hmb11"


# HUMAN DATA ----
human_summarized <- tidy_taxa(tidy_phylo = tidy_humans, summary_level = "genus")
human_abundance <- relative_abundances(summary_matrix = human_summarized$counts)


# put dataframe together for comparison
ubac <- unique(df2$genus)
ubac <- intersect(ubac, human_summarized$bacteria) # <-- these are present in both data sets

x1 <- which(human_summarized$bacteria %in% ubac)
x2 <- which(tidy_humans$samples$HMPbodysubsite == "Stool")

y1 <- which(df2$sample_group == "aerobic")
y2 <- which(df2$sample_group == "anaerobic")
y3 <- which(df2$sample_group == "Hmb11")

t1 <- which(df2$day == "3")

z1 <- which(human_summarized$bacteria %in% ubac)
z2 <- which(df2$genus %in% ubac)

df3 <- data_frame(sample = c(rep("Aerobic", length(Reduce(intersect, list(y1, z2, t1)))),
                             rep("Anaerobic", length(Reduce(intersect, list(y2, z2, t1)))),
                             rep("Hmb11", length(intersect(y3, z2))),
                             rep("Human stool", length(x2) * length(z1))),
                  genus = c(df2$genus[Reduce(intersect, list(y1, z2, t1))], 
                            df2$genus[Reduce(intersect, list(y2, z2, t1))],
                            df2$genus[intersect(y3, z2)],
                            rep(human_summarized$bacteria[z1], length(x2))),
                  relative_abundance = c(df2$relative_abundance[Reduce(intersect, list(y1, z2, t1))],
                                         df2$relative_abundance[Reduce(intersect, list(y2, z2, t1))],
                                         df2$relative_abundance[intersect(y3, z2)],
                                         as.vector(human_abundance[z1, x2])),
                  counts = c(df2$counts[Reduce(intersect, list(y1, z2, t1))], 
                             df2$counts[Reduce(intersect, list(y2, z2, t1))], 
                             df2$counts[intersect(y3, z2)], 
                             as.vector(human_summarized$counts[z1, x2])))


# normalize to counts in human stool
ubac <- unique(df3$genus)

rel_group <- character(nrow(df3))
bstat <- matrix(0, nrow(df3), 5)
for (i in seq(1, length(ubac))) {
  y1 <- df3$genus == ubac[i]
  x1 <- df3$genus == ubac[i] & df3$sample == "Aerobic"
  x2 <- df3$genus == ubac[i] & df3$sample == "Anaerobic"
  x3 <- df3$genus == ubac[i] & df3$sample == "Human stool"
  x4 <- df3$genus == ubac[i] & df3$sample == "Hmb11"
  
  ar <- max(c(mean(df3$relative_abundance[x1]),
              mean(df3$relative_abundance[x2]),
              mean(df3$relative_abundance[x3])))
  
  if (ar < 0.1) {
    rel_group[y1] <- "low"
  } else if (ar >= 0.1) {
    rel_group[y1] <- "high"
  }
  
  a1 <- boxplot.stats(df3$relative_abundance[x1])
  a2 <- boxplot.stats(df3$relative_abundance[x2])
  a3 <- boxplot.stats(df3$relative_abundance[x3])
  a4 <- boxplot.stats(df3$relative_abundance[x4])
  bstat[x1, ] <- a1$stats
  bstat[x2, ] <- a2$stats
  bstat[x3, ] <- a3$stats
  bstat[x4, ] <- a4$stats
  
}