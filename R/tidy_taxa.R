tidy_taxa <- function(tidy_phylo, 
                      summary_level = c("kingdom",
                                        "phylum",
                                        "class",
                                        "order",
                                        "family",
                                        "genus",
                                        "species",
                                        "complete"))
{
  library(Matrix)
  library(tidyverse)
  
  if(summary_level == "kingdom") {
    col <- 1
  } else if (summary_level == "phylum") {
    col <- 2
  } else if (summary_level == "class") {
    col <- 3
  } else if (summary_level == "order") {
    col <- 4
  } else if (summary_level == "family") {
    col <- 5
  } else if (summary_level == "genus") {
    col <- 6
  } else if (summary_level == "species") {
    col <- 8
  } else {
    col <- 9
  }
  
  unique_names <- unique(as.matrix(tidy_phylo$taxonomy[, col]))
  
  M <- matrix(0, nrow = length(unique_names), ncol = dim(tidy_phylo$otu_table)[2])
  
  for (i in seq(1, length(unique_names))) {
    a1 <- which(tidy_phylo$taxonomy[, col] == as.character(unique_names[i]))
    if(length(a1) > 1) {
      M[i, ] <- Matrix::colSums(tidy_phylo$otu_table[a1, ])
    } else if(length(a1) == 1) {
      M[i, ] <- tidy_phylo$otu_table[a1, ]
    }  
  }
  
  summarized_otus <- list(counts = M, bacteria = unique_names)
  # rownames(M) <- unique_names
  # colnames(M) <- colnames(otu_table(phyloseq_obj))
  # return(M)
  return(summarized_otus)
}

