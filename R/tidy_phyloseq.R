tidy_phyloseq <- function(phyloseq_obj)
{
  library(Matrix)
  library(tidyverse)
  
  # hack. not considering trees here.
  
  tmp_otu <- Matrix(otu_table(phyloseq_obj),sparse=TRUE)
  samples <- as_data_frame(sample_data(phyloseq_obj))
  taxonomy <- as_data_frame(tax_table(phyloseq_obj)) %>% 
    sapply(.,as.character) %>% 
    as_data_frame
  
  tidy_obj <- list(otu_table=tmp_otu,samples=samples,taxonomy=taxonomy)
  
  return(tidy_obj)
}