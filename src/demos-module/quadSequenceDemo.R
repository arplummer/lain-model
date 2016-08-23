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
## This script is a demonstration of the sequence modeling in Plummer
## (2015), presented at the LabPhon workshop.
######################################################################

## Loading required source files

source("../base-model/manifoldFunctions.R")
source("../base-model/visualizationFunctions.R")
source("../base-model/pathOperations.R")

## Setting output file for figures.

figureDir <- "figures/demo-figures/quad-sequence-demo-figures/"


## Generating the quadrilateral data

quadX <- c(runif(3000),0,0,1,1)
quadY <- c(runif(3000),0,1,0,1)
quad <- cbind(quadX,quadY)

corner1 <- which(quad[,1] < 0+(0.1) & quad[,2] < 0+(0.1))
corner2 <- which(quad[,1] < 0+(0.1) & quad[,2] > 1-(0.1))
corner3 <- which(quad[,1] > 1-(0.1) & quad[,2] < 0+(0.1))
corner4 <- which(quad[,1] > 1-(0.1) & quad[,2] > 1-(0.1))
#middle <- which(quad[,1] > (0.5)-(0.05) & quad[,1] < 0.5+(0.05) &
#                quad[,2] > (0.5)-(0.05) & quad[,2] < 0.5+(0.05))



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

scaleX2 <- c(3,0)
scaleY2 <- c(0,3)
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

listOfIndicies <- list(corner1,corner2,corner3,corner4)#,middle)
vectorOfColors <- c("green","orange","black","yellow")#,"purple")

quad1.colorVector.alignment <- modifyColorVector(quad1.colorVector,listOfIndicies,vectorOfColors)
quad2.colorVector.alignment <- modifyColorVector(quad2.colorVector,listOfIndicies,vectorOfColors)

quad1Data <- as.data.frame(quad1)
colnames(quad1Data) <- c("X","Y")

quad2Data <- as.data.frame(quad2)
colnames(quad2Data) <- c("X","Y")

allQuadData <- rbind(quad1Data,quad2Data)
colnames(allQuadData) <- c("X","Y")
allQuadData.colorVector <- c(quad1.colorVector,quad2.colorVector)
allQuadData.colorVector.alignment <- c(quad1.colorVector.alignment,quad2.colorVector.alignment)


theme_set(theme_bw(16))
theme_update(axis.text=element_text(size=22), axis.title=element_text(size=25), title=element_text(size=18),
             axis.title.x = element_blank(),axis.title.y = element_blank())

doPlot <- 1

if (doPlot) {

    hull1 <- quad1Data[chull(quad1Data$X, quad1Data$Y), ]
    hull2 <- quad2Data[chull(quad2Data$X, quad2Data$Y), ]

    quads <- ggplot(allQuadData, aes(X, Y)) +
        geom_polygon(data = hull1, fill = "lightblue", colour = "blue") +
            geom_polygon(data = hull2, fill = "pink", colour = "red") +
                annotate(geom = "text", x = 3.5, y = 0, label = "R", color = "black",size=8) +
                annotate(geom = "text", x = 0.5, y = 1.3, label = "S", color = "black",size=8) +
                    scale_x_continuous(limits=c(-2,6)) +
                        scale_y_continuous(limits=c(-4,4)) +
                            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                  axis.title.x=element_blank(),
                                  axis.title.y=element_blank(),legend.position="none",
                                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                  panel.grid.minor=element_blank(),plot.background=element_blank())
                                      


    pdf(file=paste0(figureDir,"sequence_ex_quads_in_plane.pdf"),width=6,height=6)
    print(quads)
    dev.off()

    convexHulls <- ggplot(allQuadData, aes(X, Y)) +
        geom_polygon(data = hull1,fill = "lightblue", colour = "blue") +
            geom_polygon(data = hull2,  fill = "pink", colour = "red") +
                annotate(geom = "text", x = 3.5, y = 0, label = "R", color = "black",size=8) +
                annotate(geom = "text", x = 0.5, y = 1.3, label = "S", color = "black",size=8) +
                    scale_x_continuous(limits=c(-2,6)) +
                        scale_y_continuous(limits=c(-4,4))

    
    pdf(file=paste0(figureDir,"sequence_ex_quads_in_cartesian_system.pdf"),width=6,height=6)
    print(convexHulls)
    dev.off()

    samplePoints <- ggplot(allQuadData,aes(x=X,y=Y)) + geom_point(colour=allQuadData.colorVector) +
        scale_x_continuous(limits=c(-2,6)) +
            scale_y_continuous(limits=c(-4,4)) +
                ggtitle("Quad. Points in a Coordinate System\n")
    
    pdf(file=paste0(figureDir,"sequence_ex_quads_samples_in_cartesian_system.pdf"),width=6,height=6)
    print(samplePoints)
    dev.off()

    alignmentPoints <- ggplot(allQuadData,aes(x=X,y=Y)) + geom_point(colour=allQuadData.colorVector.alignment,alpha=0.3) +
        scale_x_continuous(limits=c(-2,6)) +
            scale_y_continuous(limits=c(-4,4)) +
                ggtitle("Data and Alignment Pairs\n")

    pdf(file=paste0(figureDir,"sequence_ex_quads_alig_in_cartesian_system.pdf"),width=6,height=6)
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

alig1 <- c(corner1,corner2,corner3,corner4)#,middle)
alig2 <- c(corner1,corner2,corner3,corner4)#,middle)
alignment <- cbind(alig1,alig2)


## Carrying out the Laplacian Eigenmapping using the shortcut function
## combinedWeightedAdjacencyEigenmap.


embedding <- combinedWeightedAdjacencyEigenmap(weightedAdjacency1,weightedAdjacency2,alignment,mu=20,lapType="unnormalized")
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
         cex.axis=12,
         box=FALSE,
         alpha=0.4,
         axes=FALSE)
}



## A basic example of graph and path construction using adjMatix1
## which we construe as the graph g1

g1 <- weightedAdjacency1

hull1 <- quad1Data[chull(quad1Data$X, quad1Data$Y), ]
hull1 <- rbind(hull1,hull1[1,])


#dev.new()
pdf(file=paste0(figureDir,"sequence_ex_cycle_creation.pdf"),width=6,height=6)
plot(quad1Data,col="white")
lines(hull1,col="black")

g1path1to2 <- getShortestPath(g1, init=corner1[1],fin=corner2[1])
lines(quad1[g1path1to2,1],
      quad1[g1path1to2,2],
      col="red")
g1path1to4 <- getShortestPath(g1, init=corner1[1],fin=corner4[1])
lines(quad1[g1path1to4,1],
      quad1[g1path1to4,2],
      col="blue")
g1path3to2 <- getShortestPath(g1, init=corner3[1],fin=corner2[1])
lines(quad1[g1path3to2,1],
      quad1[g1path3to2,2],
      col="orange")
dev.off()


#dev.new()
pdf(file=paste0(figureDir,"sequence_ex_long_path.pdf"),width=6,height=6)
plot(quad1Data,col="white")
lines(hull1,col="black")

longPath3to4 <- consPaths(consPaths(g1path3to2,revPath(g1path1to2)),g1path1to4)
points(quad1[corner3[1],1],quad1[corner3[1],2],col="green",pch=15)
points(quad1[corner4[1],1],quad1[corner4[1],2],col="green",pch=15)
lines(quad1[longPath3to4,1],quad1[longPath3to4,2],col="green")
dev.off()



#dev.new()
pdf(file=paste0(figureDir,"sequence_ex_short_path.pdf"),width=6,height=6)
plot(quad1Data,col="white")
lines(hull1,col="black")

g1path3to4 <- getShortestPath(g1, init=corner3[1],fin=corner4[1])
lines(quad1[g1path3to4,1],
      quad1[g1path3to4,2],
      col="green")
dev.off()






hull2 <- quad2Data[chull(quad2Data$X, quad2Data$Y), ]
hull2 <- rbind(hull2,hull2[1,])

#dev.new()
pdf(file=paste0(figureDir,"sequence_ex_two_quads.pdf"),width=6,height=6)
plot(allQuadData,col="black",xlim=c(-2,6),ylim=c(-4,4))
points(quad1Data,col="blue")
points(quad2Data,col="magenta")
dev.off()

pdf(file=paste0(figureDir,"sequence_ex_two_quads_chull.pdf"),width=6,height=6)
plot(allQuadData,col="white",xlim=c(-2,6),ylim=c(-4,4))
lines(hull1,col="blue")
lines(hull2,col="magenta")
dev.off()






## 3d demonstration

warpedData1 <- embeddedQuad1[,2:4]
aS1 <- adjacencyRelation(warpedData1,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)
gS1 <- weightedAdjacency(warpedData1,aS1,weightType="constant",weightParam=1)

plot3d(allEmbeddedQuadData[,2],allEmbeddedQuadData[,3],allEmbeddedQuadData[,4],
       col=allQuadData.colorVector.alignment,alpha=0.2,
       xlab="",
       ylab="",
       zlab="",
       size=3,
       cex.axis=12,
       box=FALSE,
       axes=FALSE)

gS1path1to2 <- getShortestPath(gS1, init=corner1[1],fin=corner2[1])
lines3d(warpedData1[gS1path1to2,1],
        warpedData1[gS1path1to2,2],
        warpedData1[gS1path1to2,3],
        col="red",lwd=4)

gS1path1to4 <- getShortestPath(gS1, init=corner1[1],fin=corner4[1])
lines3d(warpedData1[gS1path1to4,1],
        warpedData1[gS1path1to4,2],
        warpedData1[gS1path1to4,3],
        col="yellow",lwd=4)

gS1path3to2 <- getShortestPath(gS1, init=corner3[1],fin=corner2[1])
lines3d(warpedData1[gS1path3to2,1],
        warpedData1[gS1path3to2,2],
        warpedData1[gS1path3to2,3],
        col="green",lwd=4)

gS1path3to4 <- getShortestPath(gS1, init=corner3[1],fin=corner4[1])
lines3d(warpedData1[gS1path3to4,1],
        warpedData1[gS1path3to4,2],
        warpedData1[gS1path3to4,3],
        col="darkcyan",lwd=4)




#### plot caregiver trajectories.

warpedData2 <- embeddedQuad2[,2:4]
aR1 <- adjacencyRelation(warpedData2,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)
gR1 <- weightedAdjacency(warpedData2,aR1,weightType="constant",weightParam=1)

plot3d(allEmbeddedQuadData[,2],allEmbeddedQuadData[,3],allEmbeddedQuadData[,4],
       col=allQuadData.colorVector.alignment,alpha=0.2,
       xlab="",
       ylab="",
       zlab="",
       size=3,
       cex.axis=12,
       box=FALSE,
       axes=FALSE)


# e \to i

gR1path1to2 <- getShortestPath(gR1, init=corner1[1],fin=corner2[1])
lines3d(warpedData2[gR1path1to2,1],
        warpedData2[gR1path1to2,2],
        warpedData2[gR1path1to2,3],
        col="red",lwd=4)


# e \to u

gR1path1to4 <- getShortestPath(gR1, init=corner1[1],fin=corner4[1])
lines3d(warpedData2[gR1path1to4,1],
        warpedData2[gR1path1to4,2],
        warpedData2[gR1path1to4,3],
        col="yellow",lwd=4)

# o \to i

gR1path3to2 <- getShortestPath(gR1, init=corner3[1],fin=corner2[1])
lines3d(warpedData2[gR1path3to2,1],
        warpedData2[gR1path3to2,2],
        warpedData2[gR1path3to2,3],
        col="green",lwd=4)

# o \to u

gR1path3to4 <- getShortestPath(gR1, init=corner3[1],fin=corner4[1])
lines3d(warpedData2[gR1path3to4,1],
        warpedData2[gR1path3to4,2],
        warpedData2[gR1path3to4,3],
        col="darkcyan",lwd=4)





#### plot long, composed infant trajectories.


plot3d(allEmbeddedQuadData[,2],allEmbeddedQuadData[,3],allEmbeddedQuadData[,4],
       col=allQuadData.colorVector.alignment,alpha=0.2,
       xlab="",
       ylab="",
       zlab="",
       size=3,
       cex.axis=12,
       box=FALSE,
       axes=FALSE)

longS1Path3to4 <- consPaths(consPaths(gS1path3to2,revPath(gS1path1to2)),gS1path1to4)
lines3d(warpedData1[longS1Path3to4,1],
        warpedData1[longS1Path3to4,2],
        warpedData1[longS1Path3to4,3],
        col="deeppink3",lwd=4)


gS1path3to4 <- getShortestPath(gS1, init=corner3[1],fin=corner4[1])
lines3d(warpedData1[gS1path3to4,1],
        warpedData1[gS1path3to4,2],
        warpedData1[gS1path3to4,3],
        col="darkcyan",lwd=4)



#dev.new()

plot3d(allEmbeddedQuadData[,2],allEmbeddedQuadData[,3],allEmbeddedQuadData[,4],
       col=allQuadData.colorVector.alignment,alpha=0.2,
       xlab="",
       ylab="",
       zlab="",
       size=3,
       cex.axis=12,
       box=FALSE,
       axes=FALSE)

gS1path3to4 <- getShortestPath(gS1, init=corner3[1],fin=corner4[1])
lines3d(warpedData1[gS1path3to4,1],
        warpedData1[gS1path3to4,2],
        warpedData1[gS1path3to4,3],
        col="green",lwd=4)
