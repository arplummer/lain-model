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
##  This is a collection of functions that implement the Laplacian
##  eigenmaps dimensionality reduction algorithm described in Belkin
##  and Nyogi (2003), as well as the joint Laplacian manifold
##  alignment algorithm, as described in Ham et al. (2005), and
##  several graph operations described in Plummer and Beckman (2015).
######################################################################

## Loading required packages

library("Matrix")
library("RANN")


#################################################################################
## Adjacency matrix functions.
##
## The following functions create adjacency matrices from data, and
## other adjacency matrices.
#################################################################################

## The function adjacencyRelation creates a nearest-neighbors graph
## from a matrix or numeric data frame "dat" using the Euclidean
## distance between the rows of data.  The nearest-neighbor relation
## that results from the computation is not reflexive, and is
## symmetrically closed.


adjacencyRelation <- function(dat,adjType="nn",adjParam=10,includeLoops=FALSE) {

    if (adjType=="nn") {
    
        numberOfDataPoints <- dim(dat)[1]
        neighborhoodFrame <- nn2(dat,dat,k=(adjParam+1),searchtype="standard")
        
        neighborsMatrix <- neighborhoodFrame$nn.idx

        if (includeLoops) {
            neighborsMatrix <- neighborsMatrix[,1:adjParam]
        }

        else { neighborsMatrix <- neighborsMatrix[,-1] }
        
        sparseNeighborsComponent1 <- rep(rep(1:numberOfDataPoints),each=adjParam)
        sparseNeighborsComponent2 <- as.vector(as.matrix(t(neighborsMatrix)))
        sparseNeighborsMatrixTop <- cbind(sparseNeighborsComponent1,sparseNeighborsComponent2)
        sparseNeighborsMatrixBot <- sparseNeighborsMatrixTop[,c(2,1)]
        sparseNeighborsMatrix <- unique(rbind(sparseNeighborsMatrixTop,sparseNeighborsMatrixBot))
        
        adjMatrix <- as(sparseMatrix(sparseNeighborsMatrix[,1],sparseNeighborsMatrix[,2],x=1),"symmetricMatrix")
        return(adjMatrix)
    }


    else if (adjType=="eps") {

        distanceMatrix <- as.matrix(dist(dat,method="euclidean",diag=TRUE,upper=TRUE))

        isDistanceWithinRadius <- (distanceMatrix < adjParam)

        if (includeLoops) { adjMatrix <- isDistanceWithinRadius*1 }

        else {
            isDistanceNontrivial <- (distanceMatrix > 0)
            adjMatrix <- (isDistanceWithinRadius*isDistanceNontrivial)*1
        }

        adjMatrix <- Matrix(adjMatrix,sparse=TRUE)

        return(adjMatrix)
    }
    
    else { return("Error, adjType must be 'nn' or 'eps'")}
}



## The combinedAdjacency function takes in two adjacency matrices and
## an alignment matrix which specifies which vertices of the two
## adjacency matrices are in correspondence, and yields a combined
## ajacency matrix.


combinedAdjacency <- function(adjMatrix1,adjMatrix2,alignment,mu=1) {

  numberOfPoints1 <- dim(adjMatrix1)[1]
  numberOfPoints2 <- dim(adjMatrix2)[1]

  alignmentMatrix <- sparseMatrix(alignment[,1],alignment[,2],dims=c(numberOfPoints1,numberOfPoints2),x=mu)
  
  combinedAdjacencyTop <- cBind(adjMatrix1,alignmentMatrix)
  combinedAdjacencyBot <- cBind(t(alignmentMatrix),adjMatrix2)
  combinedAdjacency <- rBind(combinedAdjacencyTop,combinedAdjacencyBot)

  return(combinedAdjacency)
}



#################################################################################
## Weighted Adjacency Functions.
##
## The following functions create weighted adjacency matrices from
## adjacency matrices and other weighted adjacency matrices.
#################################################################################

## The default constant weighting of 1 simply returns the argument
## adjacency matrix.  That is, every adjacency matrix is a weighted
## adjacency matrix.

weightedAdjacency <- function(dat,adjMatrix,weightType="constant",weightParam=1) {

    if (weightType=="constant") {
        
        weightMatrix <- adjMatrix*weightParam

        return(weightMatrix)
    }

    else if (weightType=="heatKernel") {

        numberOfDataPoints <- dim(dat)[1]
        weightMatrix <- sparseMatrix(1:numberOfDataPoints,1:numberOfDataPoints,x=0)
        
        distMatrix <- as.matrix(dist(dat,method="euclidean",diag=TRUE,upper=TRUE))
        adjInds <- which(adjMatrix != 0, arr.ind=TRUE)

        for (rowNum in 1:nrow(adjInds)) {

            weightMatrix[adjInds[rowNum,1],adjInds[rowNum,2]] <- exp(-(distMatrix[adjInds[rowNum,1],adjInds[rowNum,2]]^2)/weightParam)
        
        }
            
        return(weightMatrix)
    }

    else { return("Error, weightType must be 'constant' or 'heatKernel")}
}


## The combinedWeightedAdjacency function takes in two weighted
## adjacency matrices and an alignment matrix with weights mu, which
## specifies which vertices of the two adjacency matrices are in
## correspondence, and yields a combined weighted adjacency matrix.

combinedWeightedAdjacency <- function(weightMatrix1,weightMatrix2,alignment,mu) {

  numberOfPoints1 <- dim(weightMatrix1)[1]
  numberOfPoints2 <- dim(weightMatrix2)[1]

  alignmentMatrix <- sparseMatrix(alignment[,1],alignment[,2],dims=c(numberOfPoints1,numberOfPoints2),x=mu)
  
  combinedWeightedTop <- cBind(weightMatrix1,alignmentMatrix)
  combinedWeightedBot <- cBind(t(alignmentMatrix),weightMatrix2)
  combinedWeighted <- rBind(combinedWeightedTop,combinedWeightedBot)

  return(combinedWeighted)
}



#################################################################################
## Graph Laplacian Computations
##
## The following functions create graph Laplacians from weighted
## adjacency matrices and other graph Laplacians.
#################################################################################

graphLaplacian <- function(weightMatrix,lapType="unnormalized") {
    
  loopWeights <- diag(diag(weightMatrix))
  columnSums <- colSums(weightMatrix)
  degreeMatrix <- diag(columnSums)
  laplacianMatrix <- degreeMatrix - weightMatrix - loopWeights

  if (lapType=="unnormalized") {
      return(laplacianMatrix)
  }

  else if (lapType=="normalized") {

      normalizationMatrix <- diag(1/sqrt(diag(degreeMatrix)))
      normalizedLaplacianMatrix <- normalizationMatrix %*% laplacianMatrix %*% normalizationMatrix
      return(normalizedLaplacianMatrix)
  }

  else { "Error, lapType must be 'unnormalized' or 'normalized'" }
}


combinedLaplacian <- function(lap1,lap2,alignment,mu=1) {

  numberOfPoints1 <- dim(lap1)[1]
  numberOfPoints2 <- dim(lap2)[1]

  alignmentMatrix <- sparseMatrix(alignment[,1],alignment[,2],dims=c(numberOfPoints1,numberOfPoints2),x=-mu)

  upperMuVector <- rep(0,numberOfPoints1)
  upperMuVector[alignment[,1]] <- mu
  upperMuDiag <- Diagonal(x=upperMuVector)

  lowerMuVector <- rep(0,numberOfPoints2)
  lowerMuVector[alignment[,2]] <- mu
  lowerMuDiag <- Diagonal(x=lowerMuVector)
  
  combinedLaplacianTop <- cBind(lap1+upperMuDiag,alignmentMatrix)
  combinedLaplacianBot <- cBind(t(alignmentMatrix),lap2+lowerMuDiag)
  combinedLaplacian <- rBind(combinedLaplacianTop,combinedLaplacianBot)

  return(combinedLaplacian)
}


#################################################################################
## Laplacian Eigenmapping
##
## The following functions carry out Laplacian Eigenmapping based on
## graphs at different stages of construction.
#################################################################################

laplacianEigenmap <- function(lap) {

  numberOfDataPoints <- dim(lap)[1]
 
  eigenInfo <- eigen(lap, symmetric=TRUE)
  eigenvalues <- rev(eigenInfo$values)
  eigenvectors <- eigenInfo$vectors[,numberOfDataPoints:1]

  return(list(eigenvalues=eigenvalues,eigenvectors=eigenvectors))
}




weightedAdjacencyEigenmap <- function(weightMatrix1,lapType="unnormalized") {

  numberOfPoints1 <- dim(weightMatrix1)[1]

  lap <- graphLaplacian(weightMatrix1,lapType="unnormalized")
  
  eigenMapInfo <- laplacianEigenmap(lap)
  eigenVals <- eigenMapInfo$eigenvalues
  eigenMap <- eigenMapInfo$eigenvectors

  return(list(eigenVals,eigenMap))
}




## A shortcut function which takes in weighted adjacency matrices and
## a weighted alignment, and yields an eigenmapping.

combinedWeightedAdjacencyEigenmap <- function(weightMatrix1,weightMatrix2,alignment,mu=1,lapType="unnormalized") {

  numberOfPoints1 <- dim(weightMatrix1)[1]
  numberOfPoints2 <- dim(weightMatrix2)[1]
  tot <- numberOfPoints1 + numberOfPoints2

  combined <- combinedWeightedAdjacency(weightMatrix1,weightMatrix2,alignment,mu)   

  lap <- graphLaplacian(combined,lapType="unnormalized")
  
  eigenMapInfo <- laplacianEigenmap(lap)
  eigenVals <- eigenMapInfo$eigenvalues
  eigenMap <- eigenMapInfo$eigenvectors
  eigenMap1 <- eigenMap[1:numberOfPoints1,]
  eigenMap2 <- eigenMap[(numberOfPoints1+1):tot,]

  return(list(eigenVals,eigenMap1,eigenMap2))
}


## A shortcut function which takes in graph Laplacians and a weighted
## alignment, and yields an eigenmapping.

## combinedLaplacianEigenmap <- function(lap1,lap2,alignment,mu=1) {

##   numberOfPoints1 <- dim(lap1)[1]
##   numberOfPoints2 <- dim(lap2)[1]
##   tot <- numberOfPoints1 + numberOfPoints2

##   combined <- combinedLaplacian(lap1,lap2,alignment,mu)   

##   eigenMap <- laplacianEigenmap(combined)
##   eigenMap1 <- eigenMap[1:numberOfPoints1,]
##   eigenMap2 <- eigenMap[(numberOfPoints1+1):tot,]

##   return(list(eigenMap1,eigenMap2))
## }
