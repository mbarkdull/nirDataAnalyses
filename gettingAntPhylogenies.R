library(tidyverse)
library(ape)
library(phytools)
library(ggtree)

#### Get a genus-level tree for all of Formicidae using the consensus tree from Borowiec et al. 2024, and grafting in missing genera: ####
rawBorowiecTree <- read.tree(file = "./06_borowiecConsensusTrees/Consensus_tree.newick")
borowiecTree <- rawBorowiecTree
# Fix the tip labels:
tipLabels <- borowiecTree$tip.label %>%
  stringr::str_split_i(pattern = "_",
                       i = 1)
borowiecTree$tip.label <- tipLabels

# List the genera that we have imaged:
imagedGenera <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/14TdkBwfKToeK63GIZOCdQDAOzsPtZXl5SGMu8OmLrUc/edit?usp=sharing",
                                          col_names = TRUE) %>%
  filter(Genus != "Must be an option present in Specify.")
imagedGenera <- unique(imagedGenera$Genus)

# Write a function to find the imaged species in those genera in the Borowiec tree:
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

#### Add in missing genera ####
# Check which genera are missing from the Borowiec tree:
generaMissingFromBorowiec <- setdiff(imagedGenera, tipLabels)

# Twenty-six genera are missing. Most of them are in the Nelsen tree, so we can use that information to add them in:
nelsenTree <- read.tree(file = "Nelsen2018_Dryad_Supplementary_File_7_ML_TREE_treepl_185.tre")
ggtree::ggtree(nelsenTree,
               linewidth = 0.05) +
  ggtree::geom_tiplab(size = 0.5) 

ggsave(filename = "nelsenTree.pdf",
       width = 6, 
       height = 20, 
       units = "in")

# Doleomyrma is sister to a clade containing Iridomyrmex, Froggattella, Ochetellus, Terneria, and Philidris. Which node is that? 408.
ggtree::ggtree(trimmedTree,
               color = "gray") +
  ggtree::geom_tiplab(size = 1) + 
  geom_nodelab(aes(label = node), 
               size = 1) +
  xlim(0, 120)

ggsave(filename = "borowiecWithNodeLabels.pdf",
       width = 6, 
       height = 20, 
       units = "in")

# Get the node number at which to add the tip:
nodeNumber <- findMRCA(trimmedTree, 
                       tips = c("Iridomyrmex", "Froggattella", "Ochetellus", "Turneria", "Philidris"), 
                       type = "node")

# Add the tip:
tip <- list(edge = matrix(c(2, 1), 1, 2),
            tip.label = "Doleomyrma",
            edge.length = 1.0,
            Nnode = 1)
class(tip) <- "phylo"
updatedTree <- bind.tree(trimmedTree,
                         tip,
                         where = nodeNumber,
                         position = 1)

ggtree::ggtree(updatedTree,
               color = "gray") +
  ggtree::geom_tiplab(size = 0.9,
                      hjust = -0.25) + 
  geom_text(aes(label = node), 
            size = 0.9) +
  xlim(0, 120)

ggsave(filename = "updatedTree.pdf",
       width = 6, 
       height = 20, 
       units = "in")

# Write a function to do the inserting:
insertMissingSpecies <- function(speciesToInsert,
                                 taxaToGetMRCA) {
  # Create the tip object:
  tip <- list(edge = matrix(c(2, 1), 1, 2),
              tip.label = speciesToInsert,
              edge.length = 1.0,
              Nnode = 1)
  class(tip) <- "phylo"
  
  # Get the node number for the MRCA:
  if (length(taxaToGetMRCA) == 1) {
    nodeNumber <- which(updatedTree$tip.label == taxaToGetMRCA)
  } else {
    nodeNumber <- findMRCA(updatedTree, 
                           tips = taxaToGetMRCA, 
                           type = "node")
  }
  
  # Add the tip in the correct place:
  updatedTree <- bind.tree(updatedTree,
                           tip,
                           where = nodeNumber,
                           position = 1)
  
  ggtree::ggtree(updatedTree,
                 color = "gray") +
    ggtree::geom_tiplab(size = 0.9,
                        hjust = -0.25) + 
    geom_text(aes(label = node), 
              size = 0.9) +
    xlim(0, 120)
  
  ggsave(filename = "updatedTree.pdf",
         width = 6, 
         height = 20, 
         units = "in")
  
  return(updatedTree)
}

# Doleomyrma:
#updatedTree <- insertMissingSpecies(speciesToInsert = "Doleomyrma",
#                                    taxaToGetMRCA = c("Iridomyrmex", "Froggattella", "Ochetellus", "Turneria", "Philidris"))

# Chronoxenus is sister to Bothriomyrmex, so add at tip 182
updatedTree <- insertMissingSpecies(speciesToInsert = "Chronoxenus",
                                    taxaToGetMRCA = c("Bothriomyrmex"))

# Austroponera is sister to a clade composed of Ponera, Pseudoponera, and Ectomomyrmex.
# This clade has different relationships with Cryptopone between the two trees, so it's not evident where to merge Austroponera in. 

# Buniapone is sister to Paltothyreus, so can be added at node 121
updatedTree <- insertMissingSpecies(speciesToInsert = "Buniapone",
                                    taxaToGetMRCA = c("Paltothyreus"))

# Emeryopone is sister to a clade containing Ponera, Pseudoponera, Ectomomyrmex, Austroponera, and Cryptopone
# So add at node 372
updatedTree <- insertMissingSpecies(speciesToInsert = "Emeryopone",
                                    taxaToGetMRCA = c("Ponera", "Pseudoponera", "Ectomomyrmex", "Cryptopone"))

# Euponera is sister to Brachyponera, so it can go at node 127
updatedTree <- insertMissingSpecies(speciesToInsert = "Euponera",
                                    taxaToGetMRCA = c("Brachyponera"))

# Loboponera is sister to Plectroctena so it goes at 134
updatedTree <- insertMissingSpecies(speciesToInsert = "Loboponera",
                                    taxaToGetMRCA = c("Plectroctena"))

# Ophthalmopone is sister to Megaponera, so it goes at 125
updatedTree <- insertMissingSpecies(speciesToInsert = "Ophthalmopone",
                                    taxaToGetMRCA = c("Megaponera"))

# Streblognathus is sister to Mesoponera, so it goes at 124
updatedTree <- insertMissingSpecies(speciesToInsert = "Streblognathus",
                                    taxaToGetMRCA = c("Mesoponera"))

# Baracidris is sister to Adelomyrmex, so it goes at 39
updatedTree <- insertMissingSpecies(speciesToInsert = "Baracidris",
                                    taxaToGetMRCA = c("Adelomyrmex"))

# Erromyrma is sister to Monomorium, so it goes at 40
updatedTree <- insertMissingSpecies(speciesToInsert = "Erromyrma",
                                    taxaToGetMRCA = c("Monomorium"))

# Chelaner is sister to Erromyrma + Monomorium, so it goes at 281
updatedTree <- insertMissingSpecies(speciesToInsert = "Chelaner",
                                    taxaToGetMRCA = c("Monomorium", "Erromyrma"))

# Syllophopsis is tricky; it falls in multiple places in the Nelsen tree, 
# but three of the four species fall sister to a clade of three Myrmicaria and one Monomorium species, 
# so I'll put it there for now, at node 41
updatedTree <- insertMissingSpecies(speciesToInsert = "Syllophopsis",
                                    taxaToGetMRCA = c("Myrmicaria"))

# Austromorium is hard because it falls sister to one of the two groups of Monomorium in the Nelsen tree, not clear how that relates to Chelaner and Erromyrma 

# Mesostruma is sister to Epopostruma, so node 3
updatedTree <- insertMissingSpecies(speciesToInsert = "Mesostruma",
                                    taxaToGetMRCA = c("Epopostruma"))

# Talaridris is sister to Eurhopalothrix, so node 27
updatedTree <- insertMissingSpecies(speciesToInsert = "Talaridris",
                                    taxaToGetMRCA = c("Eurhopalothrix"))

# Xymmer + Mystrium are sister to some of Stigmatomma (really nested within a bunch of Stigmatomma species), so go at node 152
updatedTree <- insertMissingSpecies(speciesToInsert = "Xymmer",
                                    taxaToGetMRCA = c("Stigmatomma"))

# Mystrium is sister to Xymmer, so node 241
updatedTree <- insertMissingSpecies(speciesToInsert = "Mystrium",
                                    taxaToGetMRCA = c("Stigmatomma", "Xymmer"))

# Probolomyrmex is sister to Discothyrea, so node 241
updatedTree <- insertMissingSpecies(speciesToInsert = "Probolomyrmex",
                                    taxaToGetMRCA = c("Discothyrea"))

# Per Ward et al.'s 2010 Dolichoderine phylogeny, Arnoldius is sister to Chronoxenus+Bothriomyrmex
updatedTree <- insertMissingSpecies(speciesToInsert = "Arnoldius",
                                    taxaToGetMRCA = c("Chronoxenus", "Bothriomyrmex"))

# Per Dore et al.'s 2025 Ponerine phylogeny, Asphinctopone is sister to Corrieopone, which we do not have, and in turn to Mesoponera
updatedTree <- insertMissingSpecies(speciesToInsert = "Asphinctopone",
                                    taxaToGetMRCA = c("Mesoponera"))

# Paramycetophylax falls within Cyphomyrmex based on Hanisch et al.'s 2022 tree
updatedTree <- insertMissingSpecies(speciesToInsert = "Paramycetophylax",
                                    taxaToGetMRCA = c("Cyphomyrmex"))

# Per Dore et al.'s 2025 Ponerine phylogeny, Wadeura falls within Parvaponera, 
# which we don't have, and is in turn sister to Pseudoponera:
updatedTree <- insertMissingSpecies(speciesToInsert = "Wadeura",
                                    taxaToGetMRCA = c("Pseudoponera"))

# Poneracantha: per Campacho et al. 2022's tree of Ectatomminae and Heteroponerine
# its position varies across analyses. Sister to Holcoponera, which we don't have, in one, or to 
# Alfaria, which we also don't have; those three form a clade sister to Gnamptogenys+Typhlomyrmex
updatedTree <- insertMissingSpecies(speciesToInsert = "Poneracantha",
                                    taxaToGetMRCA = c("Gnamptogenys", "Typhlomyrmex"))

# Stictoponera is sister to ((Gnamptogenys, Typhlomyrmex), ((Holcoponera, Alfaria), Poneracantha))
updatedTree <- insertMissingSpecies(speciesToInsert = "Stictoponera",
                                    taxaToGetMRCA = c("Gnamptogenys", "Typhlomyrmex", "Poneracantha"))

# Check again which taxa are missing:
setdiff(imagedGenera, updatedTree$tip.label)

# Export the tree in Newick format:
ape::write.tree(phy = updatedTree,
                file = "updatedTreeBasedOnBorowiecetal2024PlusAdditions.txt")

#### As a point of comparison, get a genus-level tree for all of Formicidae using the tree from Nelsen et al. 2018 (https://doi.org/10.1073/pnas.1719794115) ####
nelsenTree <- read.tree(file = "Nelsen2018_Dryad_Supplementary_File_7_ML_TREE_treepl_185.tre")
plot(nelsenTree)

nelsenGenera <- str_split_i(nelsenTree$tip.label,
                            pattern = "_",
                            i = 1) %>%
  unique()

generaMissingFromNelsen <- setdiff(imagedGenera, nelsenGenera)

