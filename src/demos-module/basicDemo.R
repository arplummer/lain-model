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
##  to the introductory hexagon example in Chapter 2 of Plummer
##  (2014).
######################################################################

## Loading required source files

source("../base-model/manifoldFunctions.R")
source("../base-model/visualizationFunctions.R")
source("../base-model/pathOperations.R")


## Setting output file for figures.

figureDir <- "figures/demo-figures/basic-demo-figures/"

## Creating the hexagon data

radA <- 2 
A1 <- c(radA,0)
A2 <- c(radA/2, radA*sqrt(3/2))
A3 <- c(-radA/2, radA*sqrt(3/2))
A4 <- c(-radA,0)
A5 <- c(-radA/2, -radA*sqrt(3/2))
A6 <- c(radA/2, -radA*sqrt(3/2))

hexagon <- rbind(A1,A2,A3,A4,A5,A6)

## Translating hexagon

translateX <- 4
translateY <- 5
translationCoords <- cbind(translateX,translateY)
translationMatrix <- matrix(rep(translationCoords,each=dim(hexagon)[1]),ncol=2)

hexagon.translated <- hexagon + translationMatrix

scaleX <- c(2,0)
scaleY <- c(0,2)
scaleMatrix <- cbind(scaleX,scaleY)

hexagon.translated.scaled <- hexagon.translated%*%scaleMatrix

hexagon1 <- hexagon
hexagon2 <- hexagon.translated.scaled




## Plotting the hexagon data.

doPlot <- 1


if (doPlot) {

    hexagon1Data <- as.data.frame(hexagon1)
    colnames(hexagon1Data) <- c("x","y")

    hexagon2Data <- as.data.frame(hexagon2)
    colnames(hexagon2Data) <- c("x","y")



    hexagon1.colorVector <- createColorVector(dim(hexagon1)[1],color="red")
    hexagon2.colorVector <- createColorVector(dim(hexagon2)[1],color="blue") 


    allHexagonData <- rbind(hexagon1Data,hexagon2Data)
    allHexagonData.colorVector <- c(hexagon1.colorVector,hexagon2.colorVector)

    theme_set(theme_bw(16))


    hull1 <- hexagon1Data[chull(hexagon1Data$x, hexagon1Data$y), ]
    hull2 <- hexagon2Data[chull(hexagon2Data$x, hexagon2Data$y), ]


    hexagones <- ggplot(allHexagonData, aes(x, y)) +
        geom_polygon(data = hull1, fill = "pink", colour = "red") +
            geom_polygon(data = hull2, fill = "lightblue", colour = "blue") +
                annotate(geom = "text", x = 0, y = 0, label = "P", family="Times",fontface="bold.italic",color = "black",size=8) +
                    annotate(geom = "text", x = 8, y = 10, label = "Q", family="Times",fontface="bold.italic",color = "black",size=8) +
                        theme(axis.line=element_blank(),axis.text.x=element_blank(),
                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                              axis.title.x=element_blank(),
                              axis.title.y=element_blank(),legend.position="none",
                              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),plot.background=element_blank()) +
                                  ggtitle("Hexagons Embedded in a Plane\n")

##  dev.new()
    pdf(file=paste0(figureDir,"hexagons_in_plane.pdf"),width=6,height=6)
    print(hexagones)
    dev.off()



    convexHulls <- ggplot(allHexagonData, aes(x, y)) +
        geom_polygon(data = hull1, fill = "pink", colour = "red") +
            geom_polygon(data = hull2, fill = "lightblue", colour = "blue") +
                annotate(geom = "text", x = 0, y = 0, label = "P", family="Times",fontface="bold.italic",color = "black",size=8) +
                    annotate(geom = "text", x = 8, y = 10, label = "Q", family="Times",fontface="bold.italic",color = "black",size=8) +
                        ggtitle("Hexagons Situated in a Coordinate System\n") +
                            theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

##  dev.new()
    pdf(file=paste0(figureDir,"hexagons_in_cartesian_system.pdf"),width=6,height=6)
    print(convexHulls)
    dev.off()



    plabs <- c("italic(p[1])","italic(p[2])","italic(p[3])","italic(p[4])","italic(p[5])","italic(p[6])")
    qlabs <- c("italic(q[1])","italic(q[2])","italic(q[3])","italic(q[4])","italic(q[5])","italic(q[6])")

    samplePoints <- ggplot(allHexagonData,aes(x=x,y=y)) + geom_point(color=allHexagonData.colorVector,size=4) +
        annotate("text", x=hexagon1Data[,1]*1.4, y=hexagon1Data[,2]*1.4, label=plabs, parse=T, size=6) +
            geom_point(data=hexagon1Data,aes(x,y),color="red",size=2) +
                annotate("text", x=hexagon1Data[,1]*2.4+8, y=hexagon1Data[,2]*2.4+10, label=qlabs, parse=T, size=6) +
                    geom_point(data=hexagon2Data,aes(x,y),color="blue",size=2) +
                        ggtitle("Hexagon Corner Points in a Coordinate System\n") +
                            theme(axis.text=element_text(size=16), axis.title=element_text(size=18))
    
##  dev.new()
    pdf(file=paste0(figureDir,"corners_in_cartesian_system.pdf"),width=6,height=6)
    print(samplePoints)
    dev.off()



##    nearestNeighborsPlot <- samplePoints +
##        geom_polygon(data = hull1, fill = F, colour = "red") +
##            geom_polygon(data = hull2, fill = F, colour = "blue") +
##                ggtitle("Corner Points Connected to 2 Nearest Neighbors\n") +
##                    theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

##    dev.new()
##    pdf(file="figures/basic-figures/neighbors_in_cartesian_system.pdf",width=6,height=6)
##    print(nearestNeighborsPlot)
##    dev.off()


##    alignmentPoints <- ggplot(allHexagonData,aes(x=x,y=y)) + geom_point(colour=allHexagonData.colorVector.alignment) +
##      scale_x_continuous(limits=c(-1,4)) +
##      scale_y_continuous(limits=c(-2,3))
##    dev.new()
##    print(alignmentPoints)

}



## Creating the adjacency matrices

nNeighbors <- 2
adjMatrix1 <- adjacencyRelation(hexagon1,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)
adjMatrix2 <- adjacencyRelation(hexagon2,adjType="nn",adjParam=nNeighbors,includeLoops=FALSE)


## Plotting the adjacency relations

disjointHexagons <- combinedAdjacency(adjMatrix1,adjMatrix2,c(),mu=0)
hexGraph <- weightMatrix2igraph(disjointHexagons)

if  (doPlot) {
    pdf(file=paste0(figureDir,"disjoint_hexagons_graph.pdf"),width=7,height=7)
    plot(hexGraph,
         vertex.color=c("red","red","red","red","red","red","blue","blue","blue","blue","blue","blue"),
         vertex.label.color="white",
         vertex.frame.color=c("red","red","red","red","red","red","blue","blue","blue","blue","blue","blue"),
         vertex.label=c("P1","P2","P3","P4","P5","P6","Q1","Q2","Q3","Q4","Q5","Q6"),
         vertex.size=20,
         vertex.label.cex=1.25,
         edge.color=c("red","red","red","red","red","red","blue","blue","blue","blue","blue","blue"),
         edge.width=4)
    box("figure")
    dev.off()
}


## Creating the weighted adjacency matrices

weightedAdjacency1 <- weightedAdjacency(hexagon1,adjMatrix1,weightType="constant",weightParam=1)
weightedAdjacency2 <- weightedAdjacency(hexagon2,adjMatrix2,weightType="constant",weightParam=1)


## Creating the alignment matrix

alig1 <- c(1:6)
alig2 <- c(1:6)
alignment <- cbind(alig1,alig2)


## Creating the combined adjacency matrices using the alignment matrix.
## This is the computation that models vowel normalization in our
## acquisition model.

combinedWeightedAdj <- combinedWeightedAdjacency(weightedAdjacency1,weightedAdjacency2,alignment,mu=1)
combinedWeightedGraph <- weightMatrix2igraph(combinedWeightedAdj)


if  (doPlot) {

    pdf(file=paste0(figureDir,"aligned_hexagons_graph.pdf"),width=7,height=7)
    plot(combinedWeightedGraph,
         vertex.color=c("red","red","red","red","red","red","blue","blue","blue","blue","blue","blue"),
         vertex.label.color="white",
         vertex.frame.color=c("red","red","red","red","red","red","blue","blue","blue","blue","blue","blue"),
         vertex.label=c("P1","P2","P3","P4","P5","P6","Q1","Q2","Q3","Q4","Q5","Q6"),
         vertex.size=20,
         vertex.label.cex=1.25,
         edge.color=c("red","red","purple","red","purple","red",
             "purple","red","purple","red","purple","purple",
             "blue","blue","blue","blue","blue","blue"),
         edge.width=4)
    box("figure")
    dev.off()
}


## Carrying out the Laplacian Eigenmapping using the shortcut function
## combinedWeightedAdjacencyEigenmap

embedding <- combinedWeightedAdjacencyEigenmap(weightedAdjacency1,weightedAdjacency2,alignment,mu=1)
embeddedHex1 <- embedding[[2]]
embeddedHex2 <- embedding[[3]]

allEmbeddedHexData <- rbind(embeddedHex1,embeddedHex2)
allEmbeddedHexData <- as.data.frame(allEmbeddedHexData[,c(2:3)])
colnames(allEmbeddedHexData) <- c("MediateX","MediateY")


## Plotting representations in a new reference frame.  



if (doPlot) {
  
  fplabs <- c("italic(f(p[1]))","italic(f(p[2]))","italic(f(p[3]))","italic(f(p[4]))","italic(f(p[5]))","italic(f(p[6]))")
  fqlabs <- c("italic(f(q[1]))","italic(f(q[2]))","italic(f(q[3]))","italic(f(q[4]))","italic(f(q[5]))","italic(f(q[6]))")

  embeddedHex <- ggplot(data=allEmbeddedHexData,aes(x=MediateX,y=MediateY)) + geom_point(color="purple",size=4) +
    annotate("text", x=embeddedHex1[,2]-0.1, y=embeddedHex1[,3]+0.02, label=fplabs, parse=T, size=6) +
    annotate("text", x=embeddedHex2[,2]+0.1, y=embeddedHex2[,3]+0.02, label=fqlabs, parse=T, size=6) +
    xlab("x'") + ylab("y'") +
    scale_x_continuous(limits=c(-0.6,0.6)) + scale_y_continuous(limits=c(-0.6,0.6)) +
    ggtitle("Corner Points After Alignment\n") +
    theme(axis.text=element_text(size=16), axis.title=element_text(size=18))
  
## dev.new()
  pdf(file=paste0(figureDir,"embedded_in_cartesian_system.pdf"),width=6,height=6)
  print(embeddedHex)
  dev.off() 
}
