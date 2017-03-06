# Text clustering
# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/


#load tm library
library(tm)
library(SnowballC) # Not used at moment - issue


dname <- file.path("Data/Corpus") # Tells R the path to the files
dname
dir(dname) # Lists the files in the directory/folder

ja_me_ot <- dir(dname)

docs <- Corpus(DirSource(dname)) # Create V corpus

# Preprocess texts
docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, removeWords, stopwords("english")) # To remove stopwords
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace   

writeLines(as.character(docs[[10]]))
docs <- tm_map(docs, PlainTextDocument)

# Create dtm
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- ja_me_ot
#print a summary
dtm

# Convert DTM to matrix
m <- as.matrix(dtm)
rowSums(m)
tokens <- sum(rowSums(m))
rownames(m) <- ja_me_ot
#compute Euclidean distance between document vectors
d <- dist(m)

#run hierarchical clustering using Ward’s method
groups <- hclust(d, method = "ward.D")
groups2 <- hclust(d, method = "complete")


#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang = -1)
plot(groups2, hang = -1)



#cut into 2 subtrees – try 3 and 5
rect.hclust(groups,2)

rect.hclust(groups,3)

rect.hclust(groups,5)

#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
        as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
csm <- as.matrix(cs)

c <- as.vector(csm)
c <- c[which(c != 0)]

library(igraph)
g <- graph_from_incidence_matrix(csm)

plot(g)

edges <- get.edgelist(g)


# Name columns
colnames(edges) <- c("from", "to")


# Convert to data fram to extract > 0.5
edf <- as.data.frame(edges)
# Add weight and subset
edf$weight <- c
edf2 <- subset(edf, weight > 0.5)
edf2 <- as.matrix(edf2)


# Create igraph object 2
g2 <- graph(edges = edf2[, -3])

plot(g2)

g2 <- simplify(g2)

# Calculate degree for all nodes
deg <- degree(g2, mode="all")
plot(g2, vertex.size=deg*3)
hist(deg, breaks=1:vcount(g2)-1, main="Histogram of node degree")

# Degree distribution
deg.dist <- degree_distribution(g2, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")

# Reduce arrow size
E(g2)$arrow.size <- 0.05
V(g2)$id <- 1:30
V(g2)$name

plot(g2)

g2.sym <- as.undirected(g2, mode="collapse", 
                        edge.attr.comb=list(weight="sum", "ignore"))

# Find cliques (complete subgraphs of an undirected graph)
cliques(g2.sym) # list of cliques       
sapply(cliques(g2.sym), length) # clique sizes
largest_cliques(g2.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(g2.sym))
vcol[unlist(largest_cliques(g2.sym))] <- "gold"
plot(g2.sym, vertex.label=V(g2.sym)$name, vertex.color=vcol)

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(g2) 
dendPlot(ceb, mode="hclust")
l <- layout_with_kk(g2)
plot(ceb, g2, vertex.label = V(g2)$id, main = "Cluster Edge Betweenness") 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, g2)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.


# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(g2))
plot(cfg, as.undirected(g2), vertex.label = V(g2)$id, 
     main = "Cluster Fast Greedy" )

# We can also plot the communities without relying on their built-in plot:
V(g2)$community <- ceb$membership

pal2 <- rainbow(5, alpha = 0.7) 

plot(g2, vertex.color=pal2[V(g2)$community], vertex.label = V(g2)$id, 
     main = "Cluster Edge Betweenness 2")

V(g2)$color <- pal2[V(g2)$community]

library(RColorBrewer)
pal3 <- brewer.pal(11, "Paired") 
plot(g2,vertex.shape="none", vertex.size=0.75, edge.color = "gray90",
     vertex.label.color=pal3[V(g2)$community], layout=layout.by.attr(g2, wc=10))


library(visNetwork)
# Plot in visNetwork
visIgraph(g2)


# Create and save

visIgraph(g2) %>% visSave("JA_ME_OT_30.html")

# Vector space model
library(wordVectors)
library(tsne)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(stringi)
library(magrittr)
library(igraph)
library(RColorBrewer)
library(visNetwork)
# Prepare the files
prep_word2vec("Data/corpus_18W/", "Results/corp18.txt", lowercase =  T)

# train_word2vec takes several parameters - an input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses skip-gram this can be changed by including cbow = 1
corp <- train_word2vec("Results/corp18.txt", output = "Data/Models/corp18.bin", 
                                        threads = 3, vectors = 100, window = 10)

corp <- read.vectors("Data/Models/corp.bin") # Read in pre-existing model
# Using built in clustering

set.seed(42)
centers <- 150
clustering <- kmeans(corp, centers = centers, iter.max = 40)


clusters <- sapply(sample(1:centers, 15), function(n){
        names(clustering$cluster[clustering$cluster ==n][1:10])
}) 

write.csv(clusters, "Results/clusters_corp_30")
# Load word2vec_analysis.R

# Create search terms:
ind <- c("independence", "independent")
finance <- c("wealth", "property", "inheritance", "fortune")
marriage <- c("marriage", "wedding")
personal <- c("thought", "moral", "reason", "opinion", "belief")
status <- c("status", "rank", "position", "superior", "aristocracy", "gentry") 
dependent <- c("dependent", "dependence")

term_set = lapply(ind, function(ind){
        nearest_words = corp %>% nearest_to(corp[[ind]], 20) %>%
                names
}) %>% unlist

subset1 <- corp[[term_set, average = F]]

subset1 %>%
        cosineDist(subset1) %>%
        as.dist %>%
        hclust %>%
        plot(main = "Independence Corpus 18")

# JA Corpus

ja <- read.vectors("Data/ja.bin")
term_set = lapply(ind, function(ind){
        nearest_words = ja %>% nearest_to(ja[[ind]], 20) %>%
                names
}) %>% unlist

subset1 <- ja[[term_set, average = F]]

subset1 %>%
        cosineDist(subset1) %>%
        as.dist %>%
        hclust %>%
        plot(main = "Independence JA")

# ME Corpus

me <- read.vectors("Data/me.bin")
term_set = lapply(ind, function(ind){
        nearest_words = me %>% nearest_to(me[[ind]], 20) %>%
                names
}) %>% unlist

subset1 <- me[[term_set, average = F]]

subset1 %>%
        cosineDist(subset1) %>%
        as.dist %>%
        hclust %>%
        plot(main = "Independence ME")

# Create a small focused visualisation
x <- nearest_to(corp, corp[["independent"]], 100) # 100 nearest words to rising
y <- corp[[names(x), average = F]] # creates a VSM of nearest 100 words



# Calculate cosine similarity of each word
sim <- cosineSimilarity(y, y) %>% round(2)

weight <- as.vector(sim)
# Create igraph object
g <- graph_from_incidence_matrix(sim)

# Extract edgelist
e <- get.edgelist(g)

# Name columns
colnames(e) <- c("from", "to")

# Convert to data fram to extract > 0.5
edf <- as.data.frame(e)
# Add weight and subset
edf$weight <- weight[which(weight != 0)]
edf2 <- subset(edf, weight > 0.5)

# Convert back to matrix, removing weight column
edges <- as.matrix(edf2[, -3])

# Create igraph object 2
g2 <- graph(edges = edges)

plot(g2)

# Add cosine similarity weights
E(g2)$weight <- edf2$weight

# Remove loops
g2 <- simplify(g2)

# Calculate degree for all nodes
deg <- degree(g2, mode="all")
plot(g2, vertex.size=deg*3)
hist(deg, breaks=1:vcount(g2)-1, main="Histogram of node degree")

# Degree distribution
deg.dist <- degree_distribution(g2, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")

# Degree (number of ties)
degree(g2, mode="in")
centr_degree(g2, mode="in", normalized=T)

# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
closeness(g2, mode="all", weights=NA) 
centr_clo(g2, mode="all", normalized=T) 

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
eigen_centrality(g2, directed=T, weights=NA)
centr_eigen(g2, directed=T, normalized=T) 

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betweenness(g2, directed=T, weights=NA)
edge_betweenness(g2, directed=T, weights=NA)
centr_betw(g2, directed=T, normalized=T)

# Highlight the focus word
V(g2)$color<-ifelse(V(g2)$name=='independent', 'red', 'lightskyblue') 

# Reduce arrow size
E(g2)$arrow.size <- 0.05

plot(g2)
# Hubs and authorities

# The hubs and authorities algorithm developed by Jon Kleinberg was initially used 
# to examine web pages. Hubs were expected to contain catalogues with a large number 
# of outgoing links; while authorities would get many incoming links from hubs, 
# presumably because of their high-quality relevant information. 

hs <- hub_score(g2, weights=NA)$vector
as <- authority_score(g2, weights=NA)$vector

par(mfrow=c(1,2))
plot(g2, vertex.size=hs*50, main="Hubs")
plot(g2, vertex.size=as*30, main="Authorities")
dev.off()


# Create interactive version
visIgraph(g2)

# Create and save

visIgraph(g2) %>% visSave("Independent_Corpus18.html", selfcontained = T)

# Converting to an undirected network for clustering
g2.sym <- as.undirected(g2, mode="collapse", 
                        edge.attr.comb=list(weight="sum", "ignore"))

# Find cliques (complete subgraphs of an undirected graph)
cliques(g2.sym) # list of cliques       
sapply(cliques(g2.sym), length) # clique sizes
largest_cliques(g2.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(g2.sym))
vcol[unlist(largest_cliques(g2.sym))] <- "gold"
plot(g2.sym, vertex.label=V(g2.sym)$name, vertex.color=vcol)

# A number of algorithms aim to detect groups that consist of densely connected nodes
# with fewer connections across groups. 

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(g2) 
dendPlot(ceb, mode="hclust")
plot(ceb, g2) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, g2)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.


# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(g2))
plot(cfg, as.undirected(g2))

# We can also plot the communities without relying on their built-in plot:
V(g2)$community <- cfg$membership


# Create colours
pal2 <- rainbow(10, alpha = 0.5) 

plot(g2, vertex.color=pal2[V(g2)$community])

l <- layout_with_kk(g2)

plot(g2, vertex.color=pal2[V(g2)$community], layout = l)

# http://stackoverflow.com/questions/28715736/how-to-spread-out-community-graph-made-by-using-igraph-package-in-r/28722680#28722680
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
        g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
        E(g)$weight <- 1
        
        attr <- cbind(id=1:vcount(g), val=wc)
        g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
        
        l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
        return(l)
}

pal3 <- brewer.pal(8, "Dark2") 
plot(g2,vertex.shape="none", vertex.size=0.75, edge.color = "gray90",
     vertex.label.color=pal3[V(g2)$community], layout=layout.by.attr(g2, wc=22),
     main = "Independence 100 in Corpus 18")

V(g2)$color <- pal2[V(g2)$community]


# Plot in visNetwork
visIgraph(g2)


# Create and save

visIgraph(g2) %>% 
        visSave("Independence_100_Corpus_18_community.html")


# http://stackoverflow.com/questions/28715736/how-to-spread-out-community-graph-made-by-using-igraph-package-in-r/28722680#28722680
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
        g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
        E(g)$weight <- 1
        
        attr <- cbind(id=1:vcount(g), val=wc)
        g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
        
        l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
        return(l)
}

pal3 <- brewer.pal(11, "Paired") 
plot(g2,vertex.shape="none", vertex.size=0.75, edge.color = "gray90",
     vertex.label.color=pal3[V(g2)$community], layout=layout.by.attr(g2, wc=10))
