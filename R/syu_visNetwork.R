#' Title
#'
#' @param df result of get_nrc_sentiment
#' @import visNetwork
#' @return a visNetwork
#' @export

syu_visNetwork <- function(df){
  myCMatrix <- get_emotion_correlation(df)

  myCMatrix[!lower.tri(myCMatrix)] <-0
  myGraph <-
    igraph::graph_from_adjacency_matrix(myCMatrix*100,  mode="undirected", weighted=T)
  myGraphData <- toVisNetworkData(myGraph)
  # myGraphData$nodes$color=c("darkred", "grey", "orange", "darkblue", "purple",'green','olive','grey')
  # myGraphData$nodes$group=c('red','yellow')
  myGraphData$edges$value <-round(myGraphData$edges$weight)
  myGraphData$nodes$value <- igraph::strength(myGraph)
  visNetwork(nodes = myGraphData$nodes, edges = myGraphData$edges) %>%
    visIgraphLayout(layout = "layout_in_circle")
}

