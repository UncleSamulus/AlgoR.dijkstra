# Shortest path in a Graph - AlgoR Project
# Samuel ORTION
# GPLv3+


#' Bellman-Ford algorithm (R version)
#'
#' @description Find shortest path between two nodes in a graph
#' @param summits a vector of all summits
#' @param edges a nested list, which items are composed of `from`, `to` and `weight`
#' @param start the node which distance from is computed
#' @param end the target node
#' @return a list with `d` the constructed distance for each node and `pred` a vector containing at item i the predecessor of i-th summit
bellmanford <- function(summits, edges, start, end) {
  nodedistance <- rep(Inf, length(summits))
  nodedistance[start] <- 0
  predecessor <- rep(NULL, length(summits))
  predecessor[start] <- start
  for (k in 1:length(summits)) {
    for (edge in edges) {
      from <- edge$from
      to <- edge$to
      weight <- edge$weight
      if (nodedistance[from] + weight < nodedistance[to]) {
        nodedistance[to] <- nodedistance[from] + weight
        predecessor[to] <- from
      }
    }
  }

  # Check if there are no negative weighted cycle
  for (edge in edges) {
    from <- edge$from
    to <- edge$to
    weight <- edge$weight
    if (nodedistance[to] > nodedistance[from] + weight) {
      throw(Exception(
        "Error: There is an absorbant cycle in the graph given to `bellmanford`"
      ))
    }
  }

  return(list(d = nodedistance, pred = predecessor))
}

#' Dijkstra Algorithm
#' @param summits the vector of vertices
#' @param edges a list of 3-uplet list (from, to, weight) representing the weighted graph
#' @param start the start node
#' @param end the target node
#' @return a list with `d` the constructed distance for each node and `pred` a vector containing at item i the predecessor of i-th summit
dijkstra <- function(summits, edges, start, end) {
  visited_nodes <- NULL
  nodedistance <- rep(Inf, length(summits))
  nodedistance[start] <- 0
  predecessor <- rep(NULL, length(summits))
  predecessor[start] <- start

  while (length(visited_nodes) < length(summits)) {
    current <- nextclosestnode(summits, visited_nodes, nodedistance)
    visited_nodes <- c(visited_nodes, current)
    if (current == end) {
      break
    }
    for (edge in edges) {
      if (edge$from == current) {
        dist <- nodedistance[current] + edge$weight
        if (dist < nodedistance[edge$to]) {
          nodedistance[edge$to] <- dist
          predecessor[edge$to] <- edge$from
        }
      }
    }
  }
  return(list(
    d = nodedistance[end], pred = predecessor
  ))
}

nextclosestnode <- function(summits, visited, distance) {
  nodeleft <- which(!(summits %in% visited))
  distleft <- distance[nodeleft]
  mindist <- min(distleft)
  return(
    which(distance == mindist)[1]
  )
}


#' Find the shortest path from a list of predecessors
#' @param predecessors list of predecessors
#' @param start start node
#' @param end end node
#' @return list with `distance` the overall distance and `path` the vector of vertices passed through
shortestpath <- function(predecessors, start, end) {
  if (is.null(predecessors[end])) {
    throw(Exception(
      "There is no path from `end` to `start`"
    ))
  } else {
    path <- end
    current <- end
    i <- 1
    while (current != start) {
      if (i > length(predecessors)) {
        throw(Exception(
          "Could not find a proper path with given predecessors vector"
        ))
      }
      current <- predecessors[current]
      path <- c(current, path)
      i <- i + 1
    }
  }
  return(path)
}
