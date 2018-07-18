################################################################
## Copyright (C) DREAL PACA > Mathieu Rajerison               ##
## Licence Creative Commons BY NC SA                          ##
## Réalisation 2015                                           ##
## Contact : mathieu.rajerison@developpement-durable.gouv.fr  ##
################################################################

require(igraph)
require(sp)
require(FNN)
require(rgeos)
require(rgdal)
require(maptools)
require(R.utils)

mergeTwoLayers = function(layer1, layer2) {
  names(layer2) = names(layer1)
  proj4string(layer2)=proj4string(layer1)
  row.names(layer2) = paste("second", 1:length(layer2))
  l.all = spRbind(layer1, layer2)
  return(l.all)
}

findStartEndNodes <- function(spLines) {
  coords <- spLines@lines[[1]]@Lines[[1]]@coords
  startEndNodes <- rbind(head(coords, 1), tail(coords, 1))
  rownames(startEndNodes) <- apply(startEndNodes, 1, paste, collapse=" ")
  return(startEndNodes)
}

construireReseau <- function(spLines) {  
  # EDGES
  edges = do.call(rbind, lapply(spLines@lines, function(ls) {
    cbind(head(ls@Lines[[1]]@coords, 1), 
          tail(ls@Lines[[1]]@coords, 1))}))
  
  # EDGE IDS
  IDS <- sapply(1:nrow(edges), function(x) paste(edges[x, ][order(edges[x, ])], collapse=" "))
  
  # LONGUEUR DES SEGMENTS
  lengths = gLength(spLines, byid=T)
  
  # GRAPHE
  froms = paste(edges[, 1], edges[, 2])
  tos = paste(edges[, 3], edges[, 4])
  
  graph = graph.edgelist(cbind(froms, tos), directed = FALSE)
  
  # EDGES
  E(graph)$name = 1:ecount(graph)
  E(graph)$weight = lengths
  
  # COORDONNEES DES VERTICES
  xy = do.call(rbind, strsplit(V(graph)$name, " "))
  
  V(graph)$x = as.numeric(xy[, 1])
  V(graph)$y = as.numeric(xy[, 2])
  
  return(graph)
}

mergeThenSplitLine <- function(spLine) {
  m <- gLineMerge(spLine)
  nLines <- length(m@lines[[1]]@Lines)
  l <- lapply(1:nLines, function(x) Lines(list(m@lines[[1]]@Lines[[x]]), ID=x))
  merged <- SpatialLinesDataFrame(SpatialLines(l), data=data.frame(1:length(l)))  
  return(merged)
}
