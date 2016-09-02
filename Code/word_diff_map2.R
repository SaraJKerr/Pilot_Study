################################################################################
# File-Name: word_diff_map2.R
# Date: 8 March 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: Word cloud showing statistically significant differences - an attempt 
# to improve on word_difference_map
# Based on: work of Drew Conway and Rolf Fredheim and word_difference_map
# Data Used: Austen_1814_MP_3ch.txt, Edgeworth_1814_P_3ch.txt
# Stoplist: none (option to use built in stoplist)
# Packages Used: tm, ggplot2
# Input: plain text files
# Output: plot
# Last Updated: 30 March 2016
################################################################################

#####################################
## Reading in and Processing Texts ##
#####################################

# Read in texts - need to be in same folder
text_1 <- readLines('Data/Plain_Texts/Austen_1814_MP.txt')
text_2 <- readLines('Data/Plain_Texts/Edgeworth_1814_P.txt')

# Load tm package
require(tm)

# Convert for use in tm package - creates a Volatile Corpus 
text_1c <- Corpus(VectorSource(text_1))
text2_c <- Corpus(VectorSource(text_2))

# Remove punctuation, whitespace, change to lower case, 
# remove numbers. No stoplist or stemming
text_1c <- tm_map(text_1c, stripWhitespace)
text1c <- tm_map(text_1c, content_transformer(tolower))
#text_1c <- tm_map(text_1c, removeWords, stopwords("english")) # Remove 1st # to activate
#text_1c <- tm_map(text_1c, stemDocument) # Remove 1st # to activate
text_1c <- tm_map(text_1c, removePunctuation) 
text_1c <- tm_map(text_1c, removeNumbers) 

text2_c <- tm_map(text2_c, stripWhitespace)
text2_c <- tm_map(text2_c, content_transformer(tolower))
#text2_c <- tm_map(text2_c, removeWords, stopwords("english")) # Remove 1st # to activate
#text2_c <- tm_map(text2_c, stemDocument) # Remove 1st # to activate
text2_c <- tm_map(text2_c, removePunctuation)
text2_c <- tm_map(text2_c, removeNumbers)

# Create Document Term Matrix
dtm_1 <- DocumentTermMatrix(text_1c) 
dtm_2 <- DocumentTermMatrix(text2_c)

# Convert DTM to data frame
df_1 <- data.frame(as.matrix(dtm_1))
df_2 <- data.frame(as.matrix(dtm_2))

author_1 <- as.matrix(sort(sapply(df_1, "sum"), decreasing = T)
                    [1:length(df_1)], colnames = count)
author_2 <- as.matrix(sort(sapply(df_2, "sum"), decreasing = T)
                       [1:length(df_2)], colnames = count)

# Removing missing values
author_1 <- author_1[complete.cases(author_1),] 
author_2 <- author_2[complete.cases(author_2),]

# Creates data frame 
words_author_1 <- data.frame(author_1)
words_author_2 <- data.frame(author_2)

# Merge the two tables by row names
words_compare <- merge(words_author_1, words_author_2, by="row.names", all = T)
# Replace NA with 0
words_compare[is.na(words_compare)] <- 0

# words_compare now has 3 columns - the word and the number of times it appears 
# in each text

##########################################
## Calculating Statistical Significance ##
##########################################

# Calculate the relative number of mentions per term by calculating proportions
# This adds two new columns
words_compare$prop <- words_compare$author_1/sum(words_compare$author_1) 
words_compare$prop2 <- words_compare$author_2/sum(words_compare$author_2)
# The data frame now has 5 columns

# Broke down the z score formula a little to understand how it worked
a <- words_compare$prop
b <- words_compare$prop2
c <- words_compare$author_1
d <- words_compare$author_2
e <- sum(c)
f <- sum(d)

# z score formula - adds column for z scores
words_compare$z <- (a - b) / ((sqrt(((sum(c) * a) + (sum(d) * b)) / (sum(c) + 
                        sum(d)) * (1 - ((sum(c) * a) + (sum(d) * b)) / (sum(c) +
                            sum(d))))) * (sqrt((sum(c) + sum(d)) / (sum(c) *
                                sum(d)))))

# Keep data of moderate significance - confidence level of 95%
# words_compare <- subset(words_compare, abs(z) > 1.96) # Remove 1st # to activate

#################
## Visualising ##
#################

# Order words according to significance 
words_compare <- words_compare[order(abs(words_compare$z), decreasing = T),]
# Positive z scores are Austen, negative are Edgeworth

# Calculate percentage reduction:
words_compare$dif1 <- -100 * (1 - words_compare$prop/words_compare$prop2)
# Calculate percentage increase:
words_compare$dif2 <- 100 * (1 - words_compare$prop2/words_compare$prop)

# Merge results - creates new column combining $dif1 and $dif2 then removes them
words_compare$dif <- 0
words_compare$dif[words_compare$dif1 < 0] <- words_compare$dif1[words_compare$dif1 < 0]
words_compare$dif[words_compare$dif2 > 0] <- words_compare$dif2[words_compare$dif2 > 0]
words_compare$dif1 <- NULL
words_compare$dif2 <- NULL

# Load graphics package
require(ggplot2)

words_compare$z2 <- 0 # insignificant terms
words_compare$z2[abs(words_compare$z) >= 1.96] <- 1 # significant at 95% confidence 
words_compare$z2[abs(words_compare$z) >= 2.58] <- 2 # significant at 99% confidence
words_compare$z2[abs(words_compare$z) >= 3.08] <- 3 # significant at 99.79%

# Remove terms which only appear in one text
words_compare <- words_compare[words_compare$dif >-99 & words_compare$dif <99,]

words_compare <- words_compare[order(abs(words_compare$prop2+words_compare$prop),
                                   decreasing = T),]

# Plot by z score and difference highlighting min and max outlying words
ggplot(words_compare, aes(z, dif, colour=z2, label=Row.names)) + geom_point()+
        scale_colour_gradientn(name="Z Score", labels=c("Not significant", "1.96", "2.58", "3.08"),colours=rainbow(4))+
        geom_text(aes(label=ifelse(words_compare$z == max(words_compare$z), as.character(Row.names),'')), hjust=0, vjust=0) +
        geom_text(aes(label=ifelse(words_compare$z == min(words_compare$z), as.character(Row.names),'')), hjust=0, vjust=0) +
        ylab("Difference")+
        xlab("Z Score")+
        #theme(panel.background = element_rect(colour =  = "pink"))+
        theme(panel.background = element_rect(fill = "black"))+
        ggtitle("Relationship between z score and percentage difference \n in Austen and Edgeworth")

# Plot 
ggplot(head(words_compare, 100), aes(dif, log(abs(author_1 + author_2)),size = (author_1 + author_2),label=Row.names, colour = z2, position_jitter(w=0.002, h=0.002)))+
        scale_colour_gradientn(colours=rainbow(4))+
        geom_text(fontface = 2, alpha = .8) +
        scale_size(range = c(2, 10)) +
        ylab("log number of mentions") +
        xlab("Percentage difference between samples \n <-------More in Edgeworth --------------|--------More in Austen----------->") +
        geom_vline(xintercept=0, colour  = "red", linetype=2)+
        theme_bw() + theme(legend.position = "none") +
        ggtitle("Differences in Terms Used by \n Austen and Edgeworth")

# Plot brings up error "Don't know how to automatically pick scale for object
# of type AsIs. Defaulting to continuous" but plot produced


