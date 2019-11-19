tidy_singletons <- function(tidy_phylo)
{
  library(tidyverse)
  library(Matrix)
  
  # first, get row sums for all otus
  rs <- Matrix::rowSums(tidy_phylo$otu_table)
  
  # now count presence of otus in samples (ie, identify singletons -- otus that are only present in 1 sample)
  op <- apply(as.matrix(tidy_phylo$otu_table),1,function(x) length(which(x != 0)))
  
  # singletons to remove are those where we only have 5 reads in only 1 sample or those that have no reads at all
  sings <- which(op == 1 & rs <= 5)
  sings <- union(sings,which(rs == 0))
  
  return(sings)
}