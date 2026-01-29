library(tidyverse)
library(ape)
library(phytools)

#### Get a genus-level tree for all of Formicidae: ####
# Using the consensus tree from Borowiec et al. 2024
borowiecTree <- read.tree(file = "./06_borowiecConsensusTrees/Consensus_tree.newick")

# Fix the tip labels:
tipLabels <- borowiecTree$tip.label %>%
  stringr::str_split_i(pattern = "_",
                       i = 1)

borowiecTree$tip.label <- tipLabels

# List the genera that we have imaged:
imagedGenera <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/14TdkBwfKToeK63GIZOCdQDAOzsPtZXl5SGMu8OmLrUc/edit?usp=sharing",
                                          col_names = TRUE)
imagedGenera <- unique(imagedGenera$Genus)

# Write a function to find species in those genera in the Borowiec tree:
findGenera <- function(genus) {
  matchingTips <- grep(genus, 
                       borowiecTree$tip.label, 
                       value = TRUE)
  
  if (length(matchingTips) == 0) {
    print(paste("Could not find",
                genus))
  }
  
  return(matchingTips)
}

# Make the function safe with purrr::possibly:
possiblyFindGenera <- purrr::possibly(findGenera,
                                      otherwise = "No match.")

# Apply it over all genera with map:
matchingTipsInTree <- purrr::map(imagedGenera,
                                 possiblyFindGenera)

# Convert the output to a list: 
matchingTipsInTree <- unlist(matchingTipsInTree) 
matchingTipsInTree <- matchingTipsInTree[!is.na(matchingTipsInTree)]
  

# Trim the tree to include only tips for genera we have imaged:
trimmedTree <- keep.tip(phy = borowiecTree,
                        tip = matchingTipsInTree)

# Plot the tree:
ggtree::ggtree(trimmedTree) +
  ggtree::geom_tiplab(size = 2) +
  xlim(0, 120)

# Export the tree in Newick format:
ape::write.tree(phy = trimmedTree,
                file = "trimmedGenusTreeBasedOnBorowiecetal2024.txt")

#### Get a dolichoderine tree ####
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

#### Get a Polyrachis tree ####
# Read in the Polyrachis NIR data, to know which species we need:
polyrachisData <- readxl::read_xlsx(path = "./PolyrachisDataOct2025.xlsx")
polyrachisData$specificEpithet <- paste(polyrachisData$genus, 
                                        polyrachisData$species,
                                        sep = "_")
polyrachisSpecies <- unique(polyrachisData$specificEpithet)

# Read in the Polyrachis tree from Blanchard and Moreau 2022, 10.1111/syen.12578
polyrachisTreeOriginal <- read.tree("./BlanchardMoreau_PolyrhachisSpinesUCEs_DryadUpload/MCMCtreeFiveRunsCombinedIndepRates_176taxa_70p_Newick_tree.txt")
polyrachisTree <- polyrachisTreeOriginal

# Fix the tip labels:
species <- str_split_i(string = polyrachisTree$tip.label,
                       pattern = "_",
                       i = 2)
polyrachisTree$tip.label <- paste("Polyrhachis_",
                                  species,
                                  sep = "")
rm(species)

# Find the species for which we have data in the tree:
findPolyrachis <- function(species) {
  matchingTips <- grep(species, 
                       polyrachisTree$tip.label, 
                       value = TRUE)
  return(matchingTips)
}

# Make the function safe with purrr::possibly:
possiblyFindPolyrachis <- purrr::possibly(findPolyrachis,
                                          otherwise = "No match.")

# Apply it over all genera with map:
polyrachisTipsInTree <- purrr::map(polyrachisSpecies,
                                   possiblyFindPolyrachis)

# Convert the output to a list:
polyrachisTipsInTree <- unlist(polyrachisTipsInTree)

# Trim the tree to include only Dolichoderine tips:
trimmedTree <- keep.tip(phy = polyrachisTree,
                        tip = polyrachisTipsInTree)

# Plot the tree:
ggtree::ggtree(trimmedTree) +
  ggtree::geom_tiplab(size = 3) +
  xlim(0, 0.25)

# Export the tree in Newick format:
ape::write.tree(phy = trimmedTree,
                file = "polyrachisTreeFromBlanchardAndMoreau2022.txt")








