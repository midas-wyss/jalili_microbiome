tidy_correct_counts <- function(tidy_phylo,control_sample_id)
{
  
  tmp <- as.matrix(x = tidy_phylo$otu_table)
  M <- apply(tmp,2,function(x) x - tmp[,control_sample_id])
  M <- apply(M,2,function(x) {a1 <- which(x < 0); x[a1] <- 0; return(x) } )
  
  cOTU <- list(otu_table=M[,-control_sample_id],
               samples=tidy_phylo$samples[-control_sample_id,],
               taxonomy=tidy_phylo$taxonomy)
  
  return(cOTU)
}

