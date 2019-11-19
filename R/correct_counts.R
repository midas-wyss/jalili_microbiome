correct_counts <- function(phyloseq_obj,control_sample_id)
{
  M <- apply(otu_table(phyloseq_obj),2,function(x) x - otu_table(phyloseq_obj)[,control_sample_id])
  M <- apply(M,2,function(x) {a1 <- which(x < 0); x[a1] <- 0; return(x) } )
  
  rownames(M) <- rownames(tax_table(phyloseq_obj))
  colnames(M) <- colnames(otu_table(phyloseq_obj))
  cOTU <- phyloseq(otu_table(M,taxa_are_rows = TRUE),
                   tax_table(phyloseq_obj),
                   sample_data(phyloseq_obj),
                   phy_tree(phyloseq_obj))

  return(cOTU)
}

