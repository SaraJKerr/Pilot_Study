################################################################################
# File-Name: topic_modelling.R
# Date: 17 March 2016
# Author: Sara J Kerr
# ORCID:orcid.org/0000-0002-2322-1178
# Purpose: 
# Based on: Mimno Mallet and Matthew Jockers
# Data Used: 
# Stoplist: none 
# Packages Used: 
# Input: plain text file
# Output: csv file, cluster dendrogram, 
# Last Updated: 30 March 2016
################################################################################

install.packages("mallet")

require(mallet)

# Topic model can be created using whole text or extracted nouns/verbs
documents <- mallet.read.dir("Data/Plain_Texts") # For full text

# For nouns only - extracted noun file has one word per line, needs to be collapsed
# This piece of code reads in the file, collapses it, saves the collapsed version
# to a .txt file in a different folder then reads it into Mallet
text.nouns <- scan("Data/Tagged_Texts/Nouns/Austen_1814_MP_nouns.txt", 
                   what="character", sep="\n")
text.nouns.col <- paste(text.nouns, collapse = " ")
write(text.nouns.col, file="Data/Tagged_Texts/Collapsed/Austen_1814_MP_nouns_c.txt")
documents <- mallet.read.dir("Data/Tagged_Texts/Collapsed")


mallet.instances <- mallet.import(documents$id, documents$text,
                                  "Data/Stoplists/en.txt", token.regexp = 
                                          "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# Method to identify appropriate number of topics


n.topics <- 50
topic.model <- MalletLDA(n.topics)

topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
length(vocabulary)
vocabulary[1:100]

head(word.freqs)

topic.model$setAlphaOptimization(20,50)   # optional line - otherwise default

topic.model$train(200)
topic.model$maximize(20)

doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)

topic.docs <-t(doc.topics)
topic.docs <- topic.docs/rowSums(topic.docs)
write.csv(topic.docs, "Analysis/Results/topic-doc.csv")

topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model,
                topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

plot(hclust(dist(topic.words)), labels = topics.labels)

x<- hclust(dist(topic.words))

plot.new()
plot(x)
groups <- cutree(x, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(x, k=5, border="red") # draw dendogram with red borders around the 5 clusters   


