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
##  This script is a demonstration of manifold alignment corresponding
##  to the introductory quadrilateral example in Chapter 2 of Plummer
##  (2014).
######################################################################

## Loading required source files

source("../base-model/manifoldFunctions.R")
source("../base-model/visualizationFunctions.R")
 

## Setting output file for figures.

figureDir <- "figures/demo-figures/quad-alignment-demo-figures/"



## Generating the quadrilateral data

quadX <- c(runif(3000),0,0,1,1) 
quadY <- c(runif(3000),0,1,0,1) 
quad <- cbind(quadX,quadY)

corner1 <- which(quad[,1] < 0+(0.1) & quad[,2] < 0+(0.1))
corner2 <- which(quad[,1] < 0+(0.1) & quad[,2] > 1-(0.1))
corner3 <- which(quad[,1] > 1-(0.1) & quad[,2] < 0+(0.1))
corner4 <- which(quad[,1] > 1-(0.1) & quad[,2] > 1-(0.1))
middle <- which(quad[,1] > (0.5)-(0.05) & quad[,1] < 0.5+(0.05) &
                quad[,2] > (0.5)-(0.05) & quad[,2] < 0.5+(0.05))



## Rotating the quad

theta1 <- -pi/8

rotationX1 <- cbind(cos(theta1),-sin(theta1))
rotationY1 <- cbind(sin(theta1),cos(theta1))
rotationMatrix1 <- rbind(rotationX1,rotationY1)

quad.rotated <- quad%*%rotationMatrix1


## Scaling quad.rotated

scaleX1 <- c(2,0)
scaleY1 <- c(0,2)
scaleMatrix1 <- cbind(scaleX1,scaleY1)

quad.rotated.scaled <- quad.rotated%*%scaleMatrix1


## Naming the first quad in the manifold alignment computation

quad1 <- quad.rotated.scaled




## Scaling another quad

scaleX2 <- c(0.5,0)
scaleY2 <- c(0,2)
scaleMatrix2 <- cbind(scaleX2,scaleY2)

quad.scaled <- quad%*%scaleMatrix2


## Translating quad.scaled 

translateX <- 1
translateY <- 1
translationCoords <- cbind(translateX,translateY)
translationMatrix <- matrix(rep(translationCoords,each=dim(quad.scaled)[1]),ncol=2)

quad.scaled.translated <- quad.scaled + translationMatrix


## Rotating quad.scaled.translated 

theta2 <- pi/4

rotationX2 <- cbind(cos(theta2),-sin(theta2))
rotationY2 <- cbind(sin(theta2),cos(theta2))
rotationMatrix2 <- rbind(rotationX2,rotationY2)

quad.scaled.translated.rotated <- quad.scaled.translated%*%rotationMatrix2


## Naming the second quad in the manifold alignment computation

quad2 <- quad.scaled.translated.rotated




## Plotting the quad data.

quad1.colorVector <- createColorVector(dim(quad1)[1],"blue")
quad2.colorVector <- createColorVector(dim(quad2)[1],"magenta")

listOfIndicies <- list(corner1,corner2,corner3,corner4,middle)
vectorOfColors <- c("green","orange","black","yellow","purple")

quad1.colorVector.alignment <- modifyColorVector(quad1.colorVector,listOfIndicies,vectorOfColors)
quad2.colorVector.alignment <- modifyColorVector(quad2.colorVector,listOfIndicies,vectorOfColors)

quad1Data <- as.data.frame(quad1)
colnames(quad1Data) <- c("x","y")

quad2Data <- as.data.frame(quad2)
colnames(quad2Data) <- c("x","y")

allQuadData <- rbind(quad1Data,quad2Data)
allQuadData.colorVector <- c(quad1.colorVector,quad2.colorVector)
allQuadData.colorVector.alignment <- c(quad1.colorVector.alignment,quad2.colorVector.alignment)


doPlot <- 1

if (doPlot) {

    theme_set(theme_bw(16))

    hull1 <- quad1Data[chull(quad1Data$x, quad1Data$y), ]
    hull2 <- quad2Data[chull(quad2Data$x, quad2Data$y), ]

    quads <- ggplot(allQuadData, aes(x, y)) +
        geom_polygon(data = hull1, fill = "lightblue", colour = "blue") +
            geom_polygon(data = hull2, fill = "pink", colour = "red") +
                annotate(geom = "text", x = 2.3, y = 0.5, label = "R", family="Times",fontface="bold.italic",color = "black",size=8) +
                    annotate(geom = "text", x = 0.5, y = 1.3, label = "S", family="Times",fontface="bold.italic",color = "black",size=8) +
                        scale_x_continuous(limits=c(-1,3.5)) +
                            scale_y_continuous(limits=c(-1.25,3.25)) +
                                theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                                      axis.title.x=element_blank(),
                                      axis.title.y=element_blank(),legend.position="none",
                                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                      panel.grid.minor=element_blank(),plot.background=element_blank()) +
                                          ggtitle("Quadrilaterals Embedded in a Plane\n")

    #dev.new()
    pdf(file=paste0(figureDir,"quads_in_plane.pdf"),width=6,height=6)
    print(quads)
    dev.off()

    convexHulls <- ggplot(allQuadData, aes(x, y)) +
        geom_polygon(data = hull1, fill = "lightblue", colour = "blue") +
            geom_polygon(data = hull2, fill = "pink", colour = "red") +
                annotate(geom = "text", x = 2.3, y = 0.5, label = "R", family="Times",fontface="bold.italic",color = "black",size=8) +
                    annotate(geom = "text", x = 0.5, y = 1.3, label = "S", family="Times",fontface="bold.italic",color = "black",size=8) +
                        scale_x_continuous(limits=c(-1,3.5)) +
                            scale_y_continuous(limits=c(-1.25,3.25)) +
                                ggtitle("Quadrilaterals Situated in a Coordinate System\n") +
                                    theme(axis.text=element_text(size=16), axis.title=element_text(size=18))
    
    #dev.new()
    pdf(file=paste0(figureDir,"quads_in_cartesian_system.pdf"),width=6,height=6)
    print(convexHulls)
    dev.off()

    samplePoints <- ggplot(allQuadData,aes(x=x,y=y)) + geom_point(colour=allQuadData.colorVector) +
        scale_x_continuous(limits=c(-1,3.5)) +
            scale_y_continuous(limits=c(-1.25,3.25)) +
                ggtitle("Quadrilateral Points in a Coordinate System\n") +
                    theme(axis.text=element_text(size=16), axis.title=element_text(size=18))
    
    #dev.new()
    pdf(file=paste0(figureDir,"quads_samples_in_cartesian_system.pdf"),width=6,height=6)
    print(samplePoints)
    dev.off()

    alignmentPoints <- ggplot(allQuadData,aes(x=x,y=y)) + geom_point(colour=allQuadData.colorVector.alignment) +
        scale_x_continuous(limits=c(-1,3.5)) +
            scale_y_continuous(limits=c(-1.25,3.25)) +
                ggtitle("Quadrilateral Points and Alignment Points\n") +
                    theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

    #dev.new()
    pdf(file=paste0(figureDir,"quads_alig_in_cartesian_system.pdf"),width=6,height=6)
    print(alignmentPoints)
    dev.off()

}


## Creating weighted adjacency matrices for both quads

nNeighbors <- 20
adjMatrix1 <- adjacencyRelation(quad1,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)
adjMatrix2 <- adjacencyRelation(quad2,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)

weightedAdjacency1 <- weightedAdjacency(quad1,adjMatrix1,weightType="constant",weightParam=1)
weightedAdjacency2 <- weightedAdjacency(quad2,adjMatrix2,weightType="constant",weightParam=1)


## Creating the alignment matrix

alig1 <- c(corner1,corner2,corner3,corner4,middle)
alig2 <- c(corner1,corner2,corner3,corner4,middle)
alignment <- cbind(alig1,alig2)


## Carrying out the Laplacian Eigenmapping using the shortcut function
## combinedWeightedAdjacencyEigenmap.

embedding <- combinedWeightedAdjacencyEigenmap(weightedAdjacency1,weightedAdjacency2,alignment,mu=8,lapType="unnormalized")
embeddedQuad1 <- embedding[[2]]
embeddedQuad2 <- embedding[[3]]

allEmbeddedQuadData <- rbind(embeddedQuad1,embeddedQuad2)


## Plotting the derived representations.   

if (doPlot) {
  plot3d(allEmbeddedQuadData[,2],allEmbeddedQuadData[,3],allEmbeddedQuadData[,4],
         col=allQuadData.colorVector.alignment,
         xlab="",
         ylab="",
         zlab="",
         size=3,
         axes=FALSE,
         #lab.cex=4,
         #axis.cex=4,
         #main.cex=4,
         #main="Quadrilaterals after Alignment",
         box=FALSE)
  
}
