# Pilot Study

# From https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-
#            topic-modeling-using-r/

#### Load text mining package 'tm' ####

library(tm)

#### Load files into Corpus ####

# Set working directory where files are
setwd("~/PhD_Main/Pilot/Pilot_Study/Data/Corpus")

# Get list of pathways to .txt files
filenames <- list.files(getwd(),pattern="*.txt")

# Read files into a kist
files <- lapply(filenames,readLines)

# Create corpus from list - this is a V or volatile corpus 
docs <- Corpus(VectorSource(files))


#### Start Preprocessing ####

# Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

# Remove punctuation
docs <- tm_map(docs, removePunctuation) 

# Strip digits
docs <- tm_map(docs, removeNumbers)

# Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 
# my_stopwords <- c("a", "able", "about", "across","after", "all", "almost", "also",
#                   "am", "among", "an", "and", "any", "are", "as", "at", "be",
#                   "because", "been", "but", "by", "can", "cannot", "could", "dear",
#                   "did", "do", "does", "either", "else", "ever", "every", "for",
#                   "from", "get", "got","had","has", "have", "he", "her", "hers",
#                   "him", "his", "how", "however", "i", "if", "in", "into", "is",
#                   "it", "its", "just", "least", "let", "like", "likely", "may",
#                   "me", "might", "most", "must", "my", "neither", "no", "nor","not",
#                   "of", "off", "often", "on", "only", "or", "other", "our","own",
#                   "rather", "said", "say","says","she", "should", "since", "so",
#                   "some", "than", "that", "the", "their", "them", "then", "there",
#                   "these", "they", "this", "tis", "to", "too", "twas", "us", "wants",
#                   "was", "we", "were", "what", "when", "where", "which", "while",
#                   "who", "whom", "why", "will", "with", "would", "yet", "you",
#                   "your")
# docs <- tm_map(docs, removeWords, myStopwords)

# Remove whitespace
docs <- tm_map(docs, stripWhitespace)

# Check results so far
writeLines(as.character(docs[[1]]))

# Stem document
docs <- tm_map(docs,stemDocument)  

# Create document-term matrix
dtm <- DocumentTermMatrix(docs)

# Convert rownames to filenames
rownames(dtm) <- filenames

# Collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

# Length should be total number of terms
length(freq)

# Create sort order (descending)
ord <- order(freq,decreasing=TRUE)

# List all terms in decreasing order of freq and write to disk
freq[ord]

write.csv(freq[ord], "Results/Word_freq_.csv")

freq[which(freq == 10612)]
head(freq[ord])
freq_ord <- freq[ord]
plot(freq_ord[1:10])


#### Identify the optimum number of topics ####

# From: https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# Load packages
library(ldatuning)
library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)


# This takes hours - 
result <- FindTopicsNumber(
        dtm,
        topics = seq(from = 2, to = 50, by = 1),
        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 42),
        mc.cores = 2L, # Check this with About this Mac
        verbose = TRUE
)

FindTopicsNumber_plot(result)

#Alternative topic metric
control <- list(burnin = 500, iter = 2000, keep = 100)
(k <- optimal_k(dtm, 50, control = control))
#### Run topic modelling with LDA ####

# Set parameters for Gibbs sampling
burnin <- 500
iter <- 2000
thin <- 500
seed <-list(123, 420, 224, 10098, 49)
nstart <- 5
best <- TRUE

# Number of topics
k <- 11 # number suggested from ldatuning

################################################################################

# Alternative - cross validation
# http://ellisp.github.io/blog/2017/01/05/topic-model-cv

library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)

burnin = 1000
iter = 1000
keep = 50

# define our "full data" - during development  
full_data  <- dtm
n <- nrow(full_data)

#-----------validation--------
k <- 5 # number of topics

splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]

fitted <- LDA(train_set, k = k, method = "Gibbs",
              control = list(burnin = burnin, iter = iter, keep = keep) )
perplexity(fitted, newdata = train_set)
perplexity(fitted, newdata = valid_set)


#---------------5-fold cross-validation single value of k
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)

cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
        library(topicmodels)
})
clusterExport(cluster, c("full_data", "k", "burnin", "iter", 
                         "keep", "splitfolds"))

results <- foreach(i = 1:folds) %dopar% {
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(burnin = burnin, iter = iter, keep = keep))
        return(perplexity(fitted, newdata = valid_set))
}
stopCluster(cluster)

#----------------5-fold cross-validation, different numbers of topics------
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
        library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 3, 4, 5, 10, 15, 20, 25, 30, 50, 100) # how many topics
clusterExport(cluster, c("full_data", "burnin", "iter", "keep", 
                         "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
        results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
                k <- candidate_k[j]
                results_1k <- matrix(0, nrow = folds, ncol = 2)
                colnames(results_1k) <- c("k", "perplexity")
                for(i in 1:folds){
                        train_set <- full_data[splitfolds != i , ]
                        valid_set <- full_data[splitfolds == i, ]
                        
                        fitted <- LDA(train_set, k = k, method = "Gibbs",
                                      control = list(burnin = burnin, iter = iter, keep = keep) )
                        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
                }
                return(results_1k)
        }
})
stopCluster(cluster)

results_df <- as.data.frame(results)

ggplot(results_df, aes(x = k, y = perplexity)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        ggtitle("5-fold cross-validation of topic modelling sample set",
                "(ie five different models fit for each candidate number of topics)") +
        labs(x = "Candidate number of topics", 
             y = "Perplexity when fitting the trained model to the hold-out set")

# Run LDA using Gibbs sampling - takes time
ldaOut <-LDA(dtm, k, method = "Gibbs", control = list(nstart = nstart, 
                seed = seed, best = best, burnin = burnin, iter = iter, 
                thin = thin))

#### Write out results ####
#docs to topics
ldaOut_topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut_topics, file = paste0("Data/LDAGibbs",k,"_DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut_terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut_terms, file = paste0("Data/LDAGibbs",k,"_TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("Data/LDAGibbs",k,"TopicProbabilities.csv"))


# convert output to JSON - Trinker
topicmodels2LDAvis <- function(x, ...){
        post <- topicmodels::posterior(x)
        if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
        mat <- x@wordassignments
        LDAvis::createJSON(
                phi = post[["terms"]], 
                theta = post[["topics"]],
                vocab = colnames(post[["terms"]]),
                doc.length = slam::row_sums(mat, na.rm = TRUE),
                term.frequency = slam::col_sums(mat, na.rm = TRUE)
        )
}

ldaOut %>%
        topicmodels2LDAvis() %>%
        LDAvis::serVis(out.dir = "Plots/LDAvis1")