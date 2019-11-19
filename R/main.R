#####
# Microbiome analyses (Firoozinezad et al, Nat Biomed Eng)
# 
# Diogo M. Camacho, Ph.D.
# Systems Discovery
# Wyss Institute
# Feb/Jul 2018
####

message("---- Libraries and functions ----")
source("./R/functions_libraries.R")

message("---- Load data ----")
load("./data/2018-07-02_unfiltered_data.RData")
load("./data/2018-07-02_tidy_filtered.RData")

OTU <- untidy_unfil[[1]]
full_OTU <- untidy_unfil[[2]]
HMPv35 <- untidy_unfil[[3]]

tidy_otu <- tidy_data[[1]]
tidy_full <- tidy_data[[2]]
tidy_humans <- tidy_data[[3]]


message("---- Data processing ----")
# source("./R/process_data.R")
source("./R/richness_analysis.R")
source("./R/generate_dfs.R")
source("./R/differential_abundance.R")
source("./R/beta_diversity_test.R")

message("")
message("")
message("---- Plots ----")
source("./R/figure_3.R")
source("./R/fig3b-hack.R")
source("./R/figure_4.R")
source("./R/figure_s6.R")
source("./R/figure_s7.R")
source("./R/figure_xxx-1.R")
source("./R/figure_xxx-2.R")
source("./R/figure_xxx-3.R")