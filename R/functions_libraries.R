# functions and libraries ----
message("Load libraries...")
# libraries ----
library(tidyverse)
library(tidytext)
library(phyloseq)
library(vegan)
library(DESeq2)

message("Import functions....")
# functions ----
# source("/Volumes/HOME/scripts/r/microbiome/barplot_microbiome.R")
source("./R/filter_otus.R")
source("./R/tidy_taxa.R")
source("./R/relative_abundances.R")
source("./R/taxa_summarization.R")
source("./R/correct_counts.R")
source("./R/rename_taxa.R")
source("./R/remove_singletons.R")
source("./R/tidy_phyloseq.R")
source("./R/tidy_singletons.R")
source("./R/tidy_filter.R")
source("./R/tidy_correct_counts.R")