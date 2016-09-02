################################################################################
# File-Name: word2vec_analysis.R
# Date: 1 September 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: Word2Vec analysis and visualisation of Jane Austen's Novels
# Based on: https://github.com/bmschmidt/wordVectors/tree/master/R
#           http://www.codeproject.com/Tips/788739/Visualization-of-High-
#               Dimensional-Data-using-t-SNE
#           
# Data Used: Plain text files of Austen's published novels
# Packages Used: wordVectors, tsne, Rtsne, magrittr, ggplot2, ggrepel, plyr, 
#                dplyr, cluster, stringi
# Input: folder of plain text files
# Output: csv files, wordlists, t-SNE plots 
# Last Updated: 2 September 2016
################################################################################

# Folder structure (1) Main - (2) Texts, Results - (3) Plots

################################
# Load the wordVectors package #
################################

library(devtools)
install_github("bmschmidt/wordVectors") # Check that Xcode license agreed - yes
library(wordVectors)

##########################################
# Load the additional packages required  #
##########################################

# If not already installed use install.packages() first

library(tsne)
library(Rtsne)
library(magrittr) # Not currently used
library(ggplot2)
library(ggrepel)
library(plyr) # Not currently used
library(dplyr) # Not currently used
library(cluster) # Not currently used
library(stringi)

#########################
# Prepare the text file #
#########################

# If a prepared text file has not already been created follow this step - it 
# takes in a folder of .txt files and outputs a single .txt file which combines
# the texts in one document removes punctuation and converts all words to lower
# case.

# prep_word2vec("Texts", "Results/Austen_corpus.txt", lowercase =  T)

# Corpus of Austen novels saved as Austen_corpus.txt in Results folder

###################
# Train the model #
###################

# train_word2vec takes several parameters - an input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses skip-gram this can be changed by including cbow = 1

ja <- train_word2vec("Results/Austen_corpus.txt", output = "Results/ja.bin", 
                     threads = 3, vectors = 300, window = 15)

#ja_vec_cbow <- train_word2vec("Results/output.txt", output = 
#                "Results/ja_cbow.bin", cbow = 1, threads = 3, vectors = 300, 
#                 window = 15)

#############################################
# Read in a previously created vector model #
############################################

# ja <- read.vectors("Results/ja.bin")

############################################
# Create function to analyse and visualise #
############################################

# The function takes 4 arguments:
# vsm - a vector space model 
# words - a character vector of focus words
# seed - an integer
# ref_name - the reference name for the exported files - must be in " "

# The function will create a vector which is the average of the words input and 
# will output a wordlist of the 500 nearest words, a csv of the words and their
# positions in the t-SNE plot, and a plot of the 2D reduction of the vector space
# model using t-SNE. The points for each word are marked in red so the labels 
# can be moved for ease of reading

w2v_analysis <- function(vsm, words, seed, ref_name) {
        # Set the seed
        if (!missing(seed))
                set.seed(seed)
     
        # Identify the nearest 10 words to the average vector of search terms
        ten <- nearest_to(vsm, vsm[[words]])
        
        # Identify the nearest 500 words to the average vector of search terms and 
        # save as a .txt file
        main <- nearest_to(vsm, vsm[[words]], 500)
        wordlist <- names(main)
        filepath <- paste0("Results/", ref_name)
        write(wordlist, paste0("Results/", ref_name, ".txt"))
        
        # Create a subset vector space model
        new_model <- vsm[[wordlist, average = F]]
        
        # Run Rtsne to reduce new VSM to 2D (Barnes-Hut)
        reduction <- Rtsne(as.matrix(new_model), dims = 2, initial_dims = 50, 
                           perplexity = 30, theta = 0.5, check_duplicates = F,
                           pca = F, max_iter = 1000, verbose = F, 
                           is_distance = F, Y_init = NULL)
        
        # Extract Y (positions for plot) as a dataframe and add row names
        df <- as.data.frame(reduction$Y)
        rows <- rownames(new_model)
        rownames(df) <- rows
        
        # Save dataframe as .csv file
        write.csv(df, paste0("Results/", ref_name, ".csv"))
        
        # Create t-SNE plot and save as jpeg
        ggplot(df) +
                geom_point(aes(x = V1, y = V2), color = "red") +
                geom_text_repel(aes(x = V1, y = V2, label = rownames(df))) +
                xlab("Dimension 1") +
                ylab("Dimension 2 ") +
                # geom_text(fontface = 2, alpha = .8) +
                theme_bw(base_size = 12) + 
                theme(legend.position = "none") +
                ggtitle(paste0("2D reduction of VSM ", ref_name, " using t_SNE"))
        
        ggsave(paste0(ref_name, ".jpeg"), path = "Results/Plots", width = 24, 
               height = 18, dpi = 100)
        
        new_list <- list("Ten nearest" = ten, "Status" = "Analysis Complete") 
        return(new_list)
        
}

######################################
# Explore target words and visualise #
######################################

# Create search terms:
ind <- c("independence", "independent")
finance <- c("wealth", "property", "inheritance", "fortune")
marriage <- c("marriage", "wedding")
personal <- c("thought", "moral", "reason", "opinion", "belief")
status <- c("status", "rank", "position", "superior", "aristocracy", "gentry")
dependent <- c("dependent", "dependence")

# Run Analysis
w2v_analysis(ja, ind, 42, "Independence_Independent")

w2v_analysis(ja, finance, 42, "Finance")

w2v_analysis(ja, marriage, 42, "Marriage")

w2v_analysis(ja, personal, 42, "Personal")

w2v_analysis(ja, status, 42, "Status")

w2v_analysis(ja, dependent, 42, "Dependent")
