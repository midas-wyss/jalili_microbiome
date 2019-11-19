##### 
# REMOVE OTU SINGLETONS
# For metagenomics analysis
# takes in phyloseq object
#####
remove_singletons <- function(phyloseq_otu_object)
{
  ##### REMOVE SINGLETONS #####
  # catching singletons with less than 5 reads
  a1 <- which(apply(otu_table(phyloseq_otu_object),1,function(x) length(which(x != 0))) == 1)
  a2 <- rowSums(otu_table(phyloseq_otu_object))
  a3 <- intersect(a1,which(a2 <= 5))
  kt <- setdiff(seq(1,nrow(otu_table(phyloseq_otu_object))),a3)
  new_phyloseq <- prune_taxa(taxa = rownames(tax_table(phyloseq_otu_object)[kt,]),phyloseq_otu_object)
  
  # remove OTUs with no reads
  b1 <- rowSums(otu_table(new_phyloseq))
  b2 <- which(b1 == 0)
  kt <- setdiff(seq(1,nrow(otu_table(new_phyloseq))),b2)
  new_phyloseq <- prune_taxa(taxa = rownames(tax_table(new_phyloseq)[kt,]),new_phyloseq)
  
  return(new_phyloseq)
  
}


# no_read_otus <- function(phyloseq_otu_object)
# {
#   ##### REMOVE SINGLETONS #####
#   # catching singletons with less than 5 reads
#   # a1 <- which(apply(otu_table(phyloseq_otu_object),1,function(x) length(which(x != 0))) == 1)
#   # a2 <- rowSums(otu_table(phyloseq_otu_object))
#   # a3 <- intersect(a1,which(a2 <= 5))
#   # kt <- setdiff(seq(1,nrow(otu_table(phyloseq_otu_object))),a3)
#   # new_phyloseq <- prune_taxa(taxa = rownames(tax_table(phyloseq_otu_object)[kt,]),phyloseq_otu_object)
#   
#   # remove OTUs with no reads
#   b1 <- rowSums(otu_table(new_phyloseq))
#   b2 <- which(b1 == 0)
#   kt <- setdiff(seq(1,nrow(otu_table(new_phyloseq))),b2)
#   new_phyloseq <- prune_taxa(taxa = rownames(tax_table(new_phyloseq)[kt,]),new_phyloseq)
#   
#   return(new_phyloseq)
#   
# }


