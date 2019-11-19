rename_taxa <- function(phyloseq_otu_object)
{
  ##### AGGREGATE DATA #####
  taxa <- tax_table(phyloseq_otu_object)
  new_taxa <- taxa
  new_taxa <- apply(new_taxa, 2, function(x ){gsub("k__|p__|c__|o__|f__|g__|s__", "", x)})
  colnames(new_taxa) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  
  new_taxa[is.na(new_taxa[, 5]), 5] <- "Unknown"
  new_taxa[which(new_taxa[, 5] == ""), 5] <- "Unknown"
  
  new_taxa[is.na(new_taxa[, 6]), 6] <- "Unknown"
  new_taxa[which(new_taxa[, 6] == ""), 6] <- "Unknown"
  
  new_taxa[is.na(new_taxa[, 7]), 7] <- "sp."
  new_taxa[which(new_taxa[, 7] == ""), 7] <- "sp."
  
  GenSpe <- paste(new_taxa[, 6], new_taxa[, 7])
  GenSpe <- gsub(pattern = " $", replacement = "", GenSpe)
  GenSpe <- gsub(pattern = "^ sp.$", replacement = "", GenSpe)
  GenSpe <- gsub(pattern = "Unknown sp.", replacement = "", GenSpe)
  #GenSpe[which(GenSpe == "")] = "Unknown"
  new_taxa <- cbind(new_taxa, GenSpe)
  colnames(new_taxa)[8] <- "Common"
  
  new_taxa <- cbind(new_taxa,paste(new_taxa[, 2],
                                   new_taxa[, 3],
                                   new_taxa[, 4],
                                   new_taxa[, 5],
                                   new_taxa[, 8],
                                  sep = "; "))
  colnames(new_taxa)[9] <- "Complete classification"
  return(new_taxa)
  #tax_table(OTU) = new.taxa
  #rm(GenSpe,new.taxa)
  #rm(GenSpe)
}