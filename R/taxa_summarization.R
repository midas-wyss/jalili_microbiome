taxa_summarization <- function(phyloseq_obj,summary_level=c("kingdom","phylum","class","order","family","genus","species","complete"))
{
  tmp <- tax_table(phyloseq_obj)
  if(summary_level == "kingdom")
  {
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
  
  unique_names <- as.character(unique(tmp[,col]))
  M <- matrix(0,nrow=length(unique_names),ncol=ncol(otu_table(phyloseq_obj)))
  
  for(i in seq(1,length(unique_names)))
  {
    a1 <- which(tmp[,col] == as.character(unique_names[i]))
    if(length(a1) != 0)
    {
      M[i,] <- colSums(otu_table(phyloseq_obj)[a1,])
    }
  }
  
  rownames(M) <- unique_names
  colnames(M) <- colnames(otu_table(phyloseq_obj))
  
  return(M)
}