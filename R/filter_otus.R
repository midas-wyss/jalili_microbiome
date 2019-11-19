filter_otus <- function(phyloseq_obj,min_freq,samp_freq)
{
  # filter otus from data.
  # keep only ones with frequency higher than min_freq (default: 0.1%)
  # keep only ones present in at least samp_freq of the samples (default: 5%)
  
  if(missing(min_freq)) min_freq <- 0.1/100
  if(missing(samp_freq)) samp_freq <- 5/100
  
  a1 <- apply(as.matrix(otu_table(phyloseq_obj)),2,as.numeric)
  a1 <- apply(a1,2,function(x) x/sum(x))
  a1 <- apply(a1,1,function(x) length(which(x >= min_freq)))
  
  x1 <- ceiling(ncol(otu_table(phyloseq_obj)) * samp_freq)
  x1 <- which(a1 >= x1) # these are the otus to keep in set
  
  return(x1)
  
}



