######################################################################
##  Copyright 2014-2016 Andrew R. Plummer 
##  
##  This file is part of the Lain model.
##  
##  The Lain model is free software: you can redistribute it and/or
##  modify it under the terms of the GNU General Public License as
##  published by the Free Software Foundation, either version 3 of the
##  License, or (at your option) any later version.
##  
##  The Lain model is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##  General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with the Lain model.  If not, see
##  <http://www.gnu.org/licenses/>.
##  
##  Author:       Andrew R. Plummer
##  Affiliations: The Ohio State University
##                www.learningtotalk.org
##  Email:        plummer.321@osu.edu
######################################################################

######################################################################
##  This is a collection of functions that implement several path
##  operations described in Plummer and Beckman (2015).
######################################################################


## Loading required packages

library("igraph")

## Converts a weighted adjacency matrix into an "igraph" undirected
## graph for further computations using the igraph package.

weightMatrix2igraph <- function(weightMatrix) {
  graph <- graph.adjacency(weightMatrix,mode="undirected",weighted=TRUE)
  return(graph)
}

## gets the shortest path of within a given structure given an initial
## and final vertex.

getShortestPath <- function(weightMatrix, init, fin) {

  graphRep <- weightMatrix2igraph(weightMatrix)
  shortestPath <- get.shortest.paths(graphRep, from=init,to=fin)
  pathVertices <- shortestPath$vpath[[1]]
  
  return(pathVertices)

}

## reverses a given path

revPath <- function(inputPath) {
    outputPath <- rev(as.vector(inputPath))
    return(outputPath)
}


## concatenates two given paths

consPaths <- function(inputPath1,inputPath2) {
    outputPath <- c(as.vector(inputPath1),as.vector(inputPath2))
    return(outputPath)
}


