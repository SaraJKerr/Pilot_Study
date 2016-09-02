################################################################################
# File-Name: workflow_script.R
# Date: 17 March 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: To act as a workflow script to action the different analyses in a
#          structured manner
# Based on: 
# Data Used: 
# Stoplist: none 
# Packages Used: 
# Input: plain text file
# Output: 
# Last Updated: 13 July 2016
################################################################################


# Load/Install Necessary Packages #
cluster
devtools
dplyr
factoextra
ggplot2
koRpus
magrittr
mallet
Rtsne
tm
tsne
wordVectors

# Set Pathways for Inputs and Outputs #
# Inputs #
main_dir <- "Pilot" # working directory
text_dir <- "PhD_Pilot/Data/Plain_Texts" # directory where texts are stored
authors <- dir(text_dir)
files <- dir("PhD_Pilot/Data/Plain_Texts/Austen")
stoplist_dir <- # directory where stoplists are stored
tagged_dir <- # directory where tagged texts are stored

# Outputs #
results_dir <- # directory for results in .csv format
plots_dir <- # directory for plots generated
text_out_dir <- # directory for .txt files generated
        

################################################################################
# Will develop this bit later - aim to create a function which shows the authors,
# allows selection of author and shows files for that author - allowing selection
# for analysis. Not necessary but nice!

# Function which provides a numbered list of authors in the text_dir
show_authors <- function(author_name) {
        for(i in 1:length(author_name)) {
                cat(i, author_name[i], "\n", sep = " ")
        }
}
show_authors(authors)

# Function which provides a numbered list of files in the author folder
show_files <- function(file_name) {
        for(i in 1:length(file_name)) {
                cat(i, file_name[i], "\n", sep= " ")
        }
}

show_files(files) # Produces a numbered list
################################################################################
library(tm)



ja_corp <- () # Corpus is loaded


ja_corp <- tm_map(ja_corp, removeWords, stopwords("english"))

ja_corp <- tm_map(ja_corp, removePunctuation) 

ja_corp <- tm_map(ja_corp, tolower)  

ja_corp <- tm_map(ja_corp, PlainTextDocument) 

dtm <- DocumentTermMatrix(docs)   
dtm 

