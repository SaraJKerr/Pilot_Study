#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(igraph)
require(visNetwork)
# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Network Visualisations"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
              h3("'Rational Creatures'"),
              h4("Applying Vector Space Models to 19th Century Literature"),
              p("These visualisations are sematic network diagrams created as
                 part of my PhD."),
              p("The diagrams are created from vector space
                 models, using cosine similarity and a threshold of 0.5 to
                 identify relationships between the words.")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         visNetworkOutput("network")
      )
   )
)


server <- function(input, output) {
   
   output$network <- renderVisNetwork({
           sim <- read.csv("corpus18_sim.csv")
           rnames <- sim$X
           sim <- sim[, -1]
           rownames(sim) <- rnames
           weight <- as.vector(sim)
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
           
           # Add cosine similarity weights
           E(g2)$weight <- edf2$weight
           
           # Remove loops
           g2 <- simplify(g2)
           
           visIgraph(g2)
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

