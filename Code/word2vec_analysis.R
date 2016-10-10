################################################################################
# File-Name: word2vec_analysis.R
# Date: 1 September 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: Word2Vec analysis and visualisation 
# Based on: https://github.com/bmschmidt/wordVectors/tree/master/R
#           http://www.codeproject.com/Tips/788739/Visualization-of-High-
#               Dimensional-Data-using-t-SNE
#           
# Data Used: Plain text files 
# Packages Used: wordVectors, tsne, Rtsne, magrittr, ggplot2, ggrepel
# Input: folder of plain text files
# Output: csv files, wordlists, t-SNE plots 
# Last Updated: 10 October 2016
################################################################################

# The function takes 5 arguments:
# vsm - a vector space model 
# words - a character vector of focus words
# seed - an integer
# path - the path to the folder you want to save your results in
# ref_name - the reference name for the exported files - must be in " "

# The function will create a vector which is the average of the words input and 
# will output a wordlist of the 500 nearest words, a csv of the words and their
# positions in the t-SNE plot, and a plot of the 2D reduction of the vector space
# model using t-SNE. The points for each word are marked in red so the labels 
# can be moved for ease of reading.
# set.seed is used to ensure replicability

w2v_analysis <- function(vsm, words, seed, path, ref_name) {
        # Set the seed
        if (!missing(seed))
                set.seed(seed)
        
        # Identify the nearest 10 words to the average vector of search terms
        ten <- nearest_to(vsm, vsm[[words]])
        
        # Identify the nearest 500 words to the average vector of search terms and 
        # save as a .txt file
        main <- nearest_to(vsm, vsm[[words]], 500)
        wordlist <- names(main)
        filepath <- paste0(path, ref_name)
        write(wordlist, paste0(filepath, ".txt"))
        
        
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
        write.csv(df, paste0(filepath, ".csv"))
        
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
        
        ggsave(paste0(ref_name, ".jpeg"), path = path, width = 24, 
               height = 18, dpi = 100)
        
        new_list <- list("Ten nearest" = ten, "Status" = "Analysis Complete") 
        return(new_list)
        
}


