# Code from Ben Schmidt's GitHub https://github.com/bmschmidt/wordVectors

library(devtools)
install_github("bmschmidt/wordVectors") # Needed to check that Xcode license agreed

# Test
library(wordVectors)

# Creating your own model #####################################################
# prep_word2vec function is extremely inefficient compared to text parsing 
# functions written in python or sed or pretty much any language you can think of.
# If you can create a file with punctuation already stripped or separated with 
# some other tool, I strongly recommend doing it that way. But if you're working 
# with a few hundred documents, this will get the job done, slowly. On the 
# cookbooks, it should take a couple minutes.
prep_word2vec("cookbooks","cookbooks.txt",lowercase=T)

model = train_word2vec("cookbooks.txt",output="cookbooks.vectors",threads = 3,
                       vectors = 100,window=12)
###############################################################################
# To read in a preexisting model
model = read.vectors("cookbooks.vectors")


nearest_to(model,model[["fish"]])

nearest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp",
                          "roe","eels")]],50)
install.packages("tsne") #      T-distributed Stochastic Neighbour Embedding
library(tsne)

# Plot these words in a reduced dimensional space
some_fish = nearest_to(model,model[[c("fish","salmon","trout","shad","flounder",
                                      "carp","roe","eels")]],50)
plot(filter_to_rownames(model,names(some_fish)))

plot(model)


nearest_to(model,model[["sauce"]])
nearest_to(model, model[["meat"]])
nearest_to(model, model[[c("meat", "beef", "pork", "chicken", "veal")]], 50)

meats <- nearest_to(model, model[[c("meat", "beef", "pork", "chicken", "veal")]], 50)
plot(filter_to_rownames(model, names(meats)))

# Need to install magrittr package to use the feed forward notation
install.packages("magrittr")
library(magrittr)
model %>% nearest_to(model[["beef"]]) %>% round(3)
model %>% nearest_to(model[[c("oysters","ham","bread","chicken")]],20) %>% names

# Exploring thematic clusters - Food
food_words = model %>% nearest_to(model[[c("oysters","ham","bread","chicken",
                "sausage","cheese","biscuits","peanut","sardines","squash",
                "peaches","cherries","berries","kale","sorghum","tomatoes",
                "asparagus","lima","egg","steak","salmon","mackerel","shad",
                "meat", "beef", "pork", "chicken", "veal")]]
                ,300) %>% names
sample(food_words,50)

# create a hierarchy of food words based on their distances from each other.
foods = model[rownames(model) %in% food_words[1:50],]

food_distances = cosineDist(foods,foods) %>% as.dist
plot(as.dendrogram(hclust(food_distances)),horiz=F,cex=1,
     main="Cluster dendrogram of the fifty words closest to a food vector")

# Focusing on two terms and plotting words in this discursive space
foods = model[rownames(model) %in% food_words,]
meat_score = foods %>% cosineSimilarity(model[[c("meat","meats")]])
vegetable_score = foods %>% cosineSimilarity(model[[c("vegetable","vegetables")]])

plot(meat_score,vegetable_score,type='n',main="Top 300 food words plotted by their similarity to meats\n(x axis) and vegetables (y axis).")
text(meat_score,vegetable_score,labels=rownames(foods),cex=.7)
abline(a=0,b=1)

# We can capture a relationship in one vector. Can be tracked in space.
# This is a relationship, not a vocabulary item: but it is defined in the same
# vector space, so we can score any words based on their relationships to it.
# This means we can create text layouts specific to any desired word relationship.

all_foods = data.frame(word = rownames(foods))

meat_vegetable_vector = model[["meat"]] - model[["vegetable"]]

all_foods$meatiness_vs_veginess = cosineSimilarity(foods,meat_vegetable_vector)

sweet_salty_vector = model[[c("sugar","sweet")]] - model[[c("salty","salt")]]

all_foods$sweet_vs_salty = cosineSimilarity(foods,sweet_salty_vector)

library(ggplot2)
ggplot(all_foods,aes(x=meatiness_vs_veginess,y=sweet_vs_salty,label=word)) + 
        geom_text(size=2.5) +
        scale_y_continuous("<----- saltier ..............  sweeter ------>",limits=c(-.45,.25)) +
        scale_x_continuous("<----- vegetalier ..............  meatier ------>",limits=c(-.25,.33))


# Polysemy - filtering to remove unwanted meanings

model %>% nearest_to(model[["meat"]])
model %>% 
        filter_to_rownames(c("beef","pork","chicken","fish")) %>% 
        cosineSimilarity(model[["meat"]]) # these first steps view the words

fishless = model[["meat"]] %>% 
        reject(model[["fish"]]) 

model %>% nearest_to(fishless) 

###############################################################################
###############################################################################
# Trial using 135 texts from 1800-1809
prep_word2vec("Text19","text19.txt",lowercase=T)

# Extended number of vectors to 300 and 100
texts19 = train_word2vec("text19.txt",output="text19short.vectors",threads = 3,
                       vectors = 100,window=12)
nearest_to(texts19, texts19[["independent"]])
nearest_to(texts19, texts19[["dependent"]])

library(dplyr)

# Gender as a binary
gender_vector = texts19[["she"]] - texts19[["he"]]

# Using cosine similarity to show the words which are similar or dissimilar to 
# to the gender vector

library(ggplot2)

word_scores = data.frame(word=rownames(texts19))
word_scores$gender_score = texts19 %>% cosineSimilarity(gender_vector) %>% as.vector

ggplot(word_scores %>% filter(abs(gender_score)>.25)) + 
        geom_bar(aes(y=gender_score,x=reorder(word,gender_score),fill=gender_score<0),stat="identity") + 
        coord_flip()+scale_fill_discrete("Indicative of gender",labels=c("she","he")) + 
        labs(title="The words showing the strongest skew along the gender binary")
# Lots of words - must find a way of reducing the number

# 
independence_vector = texts19[[c("independent","independence")]]-
        texts19[[c("dependent","dependence")]]
gender_vector = texts19[[c("woman","she","her","hers","miss","herself", "mrs",
                           "lady")]] - 
        texts19[[c("man","he","his","him","mr","himself","herself","sir")]]

word_scores$gender_score = texts19 %>% cosineSimilarity(gender_vector) %>% as.vector

word_scores$independence_score = cosineSimilarity(texts19,independence_vector) %>% as.vector

groups = c("gender_score","independence_score")
word_scores %>% mutate( genderedness=ifelse(gender_score>0,"female","male"),independence=ifelse(independence_score>0,"positive",
        "negative")) %>% group_by(independence,genderedness) %>% filter(rank(-(abs(gender_score*independence_score)))<=36) %>% mutate(eval=-1+rank(abs(independence_score)/abs(gender_score))) %>% ggplot() + 
        geom_text(aes(x=eval %/% 12,y=eval%%12,label=word,fontface=ifelse(genderedness=="female",2,3),color=independence),hjust=0) + 
        facet_grid(independence~genderedness) + 
        theme_minimal() + 
        scale_x_continuous("",lim=c(0,3)) + 
        scale_y_continuous("") + 
        labs(title="The top negative (red) and positive(blue)\nused to describe men (italics) and women(bold)") + 
        theme(legend.position="none")



