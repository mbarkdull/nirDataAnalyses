library(tidyverse)
library(ape)
library(phytools)

# Using the tree from Nelsen et al. 2018 (https://doi.org/10.1073/pnas.1719794115)
nelsenTree <- read.tree(file = "Nelsen2018_Dryad_Supplementary_File_7_ML_TREE_treepl_185.tre")
plot(nelsenTree)

# List valid dolichoderine genera from AntWeb:
dolichoderineGenera <- c("Anillidris",
                         "Anonychomyrma",
                         "Aptinoma",
                         "Arnoldius",
                         "Axinidris",
                         "Azteca",
                         "Bothriomyrmex",
                         "Chronoxenus",
                         "Doleromyrma",
                         "Dolichoderus",
                         "Dorymyrmex",
                         "Ecphorella",
                         "Forelius",
                         "Froggattella",
                         "Gracilidris",
                         "Iridomyrmex",
                         "Leptomyrmex",
                         "Linepithema",
                         "Liometopum",
                         "Loweriella",
                         "Nebothriomyrmex",
                         "Ochetellus",
                         "Papyrius",
                         "Philidris",
                         "Ravavy",
                         "Tapinoma",
                         "Technomyrmex",
                         "Turneria")

# Write a function to find species in those genera in the Nelsen tree:
findDolichoderines <- function(genus) {
  matchingTips <- grep(genus, 
                       tipLabels, 
                       value = TRUE)
  return(matchingTips)
}

# Make the function safe with purrr::possibly:
possiblyFindDolichoderines <- purrr::possibly(findDolichoderines,
                                              otherwise = "No match.")

# Apply it over all genera with map:
dolichoderineTipsInTree <- purrr::map(dolichoderineGenera,
                                      possiblyFindDolichoderines)

# Convert the output to a list:
dolichoderineTipsInTree <- unlist(dolichoderineTipsInTree)

# Trim the tree to include only Dolichoderine tips:
trimmedTree <- keep.tip(phy = nelsenTree,
                        tip = dolichoderineTipsInTree)

# Plot the tree:
ggtree::ggtree(trimmedTree) +
  ggtree::geom_tiplab(size = 3) +
  xlim(0, 120)

# Export the tree in Newick format:
ape::write.tree(phy = trimmedTree,
                file = "dolichoderineTreeFromNelsen2018.txt")
