################################################################################
# File-Name: text_tagging.R
# Date: 15 March 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: To read in a plain text file, tag it for parts of speech, and extract 
# the information from the tagged file
# Based on: https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.pdf
# Data Used: texts in plain text format
# Stoplist: none 
# Packages Used: korPus
# Input: plain text file
# Output: selection of tagged files in dataframe or plain text format
# Last Updated: 15 March 2016
################################################################################

#################
## Preparation ##
#################

# Prior to running the script TreeTagger needs to be downloaded to your computer
# it can be downloaded from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Instructions for downloading and set up of TreeTagger are on the site.
# If error 'error TreeTagger/lib/english.par not found' appears check the
# TreeTagger 'lib' folder and rename the 'english-utf8.par' file 'english.par'.

####################################
## Reading in and Processing Text ##
####################################

# Install and load required package
# install.packages(koRpus)
library(koRpus)

filename <- list.files("Data/Texts/", pattern="*.txt")

# Read in text and apply POS tagger
text_tagged <- treetag(paste0("Data/Texts/", filename[]),
                       treetagger="manual", lang="en", 
                       TT.options=list(path="TreeTagger", preset="en"))

# This creates a kRp.tagged file

# Check that the processing has worked
head(taggedText(text_tagged))

# View the structure of the file
str(describe(text_tagged))

# View the slot names of the file
slotNames(text_tagged)

##################################################################
## Extract the Tagged words and create separate files if needed ##
##################################################################

# Extract the words, tags and description
tagged_doc <- text_tagged@TT.res[, c(1,2,6)]

# Check that this has worked
head(tagged_doc)

# Extract nouns
single_nouns <- subset(tagged_doc, tag == "NN")
plural_nouns <- subset(tagged_doc, tag == "NNS")
nouns <- rbind(single_nouns, plural_nouns)
nouns <- nouns$token

# Save noun file as plain text
write(nouns, file = paste0("Data/Tagged/", filename[]))

# Extract proper nouns
proper_single_nouns <- subset(tagged_doc, tag == "NP")
proper_plural_nouns <- subset(tagged_doc, tag == "NPS")
proper_nouns <- rbind(proper_single_nouns, proper_plural_nouns)
proper_nouns <- proper_nouns$token

# Save proper noun file as plain text
write(proper_nouns, file = paste0("Data/Proper_Nouns/", filename[]))


