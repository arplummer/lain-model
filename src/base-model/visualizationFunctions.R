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
##  This is a collection of functions that facilitate visualization of
##  the representations and relations provided by the manifold
##  alignment computations in manifoldAlignmentFunctions.R
######################################################################

## loading required packages.

library("rgl")
library("igraph")
#library("network")
library("scales")
library("ggplot2")
library("akima")


###########################################################################
## Helper functions
###########################################################################

## creation of color vectors for plotting points. 

createColorVector <- function(numberOfDatum,color) {
  colorVector <- rep(color,numberOfDatum)
  return(colorVector)
}


## modifying a color vector with categorical information

modifyColorVector <- function(colorVector,listOfIndicies,vectorOfColors) {

  for (i_list in 1:(length(listOfIndicies))) {
    colorVector[listOfIndicies[[i_list]]] <- vectorOfColors[i_list]
  }

  return(colorVector)
}



###########################################################################
## Basic visualization functions for alignment.
###########################################################################

## visualization of adjacency matricies using igraph


plotAdjacencyMatrix.igraph <- function(adjMatrix,type="plot") {

  graph <- graph.adjacency(adjMatrix,mode="undirected")

  dev.new()
  if (type=="plot") {
    plot(graph)
  }
  else if (type=="tkplot") {
    tkplot(graph)
  }
  else if (type=="rglplot") {
    rglplot(graph)
  }  
}


plotData <- function(modelObject,dataSetAddresses=list(),paramNames=c(),
                     xlab="",ylab="",zlab="",xlim=c(),ylim=c(),zlim=c(),main="",colorNames=c(""),
                     plotDim="2d",alphaValue=1,formantPlane=FALSE) {

    allPoints <- do.call("rbind", lapply(dataSetAddresses,
                                         function(dataSetAddress) modelObject[[dataSetAddress]][,paramNames]))

    dataLengths <- lapply(dataSetAddresses,
                          function(dataSetAddress) dim(modelObject[[dataSetAddress]][,paramNames])[1])

    colorVec <- rep(colorNames,times=dataLengths)

        
    if (plotDim=="2d") {

        if (formantPlane) {
            plot(allPoints[,1],allPoints[,2],
                 main=main, 
                 xlab=xlab,
                 ylab=ylab,
                 xlim=rev(range(allPoints[,1])),
                 ylim=rev(range(allPoints[,2])),
                 col=alpha(colorVec,alphaValue),
                 pch=16,cex=1.3,
                 cex.axis=1.5,
                 cex.lab=1.4)
        }

        else {
        
            plot(allPoints[,1],allPoints[,2],
                 main=main, 
                 xlab=xlab,
                 ylab=ylab,
                 xlim=xlim,
                 ylim=ylim,
                 col=alpha(colorVec,alphaValue),
                 pch=16,cex=1.3,
                 cex.axis=1.5,
                 cex.lab=1.4)
        }

    }

    else if (plotDim=="3d") {
        plot3d(allPoints[,1],allPoints[,2],allPoints[,3],
               xlab=xlab,
               ylab=ylab,
               zlab=zlab,
               box=FALSE,
               size=3,
               alpha=alphaValue,
               lwd=2,
               cex.axis=12,
               col=colorVec)
    }
}


plotStimuli <- function(modelObject,stimSetAddress=c(""),
                        paramNames=c(),
                        color="white",textColor="black",textLabels="",
                        xlab="",ylab="",zlab="",main="",plotDim="2d",formantPlane=FALSE) {

    stims <- modelObject[[stimSetAddress]][,paramNames]


    if (plotDim=="2d") {

        if (formantPlane) {

            plot(stims[,1],stims[,2],
                 main=main,
                 xlim=rev(range(stims[,1])),
                 ylim=rev(range(stims[,2])),
                 xlab=xlab,
                 ylab=ylab,pch=16,cex=1.3,col=color,
                 cex.axis=1.5,
                 cex.lab=1.4)
            
            text(stims[,1],stims[,2],
                 labels=textLabels,
                 cex=1.5,
                 cex.axis=1.5,
                 cex.lab=1.4)

        }

        else {
        
            plot(stims[,1],stims[,2],
                 main=main, 
                 xlab=xlab,
                 ylab=ylab,pch=16,cex=1.3,col=color,
                 cex.axis=1.5,
                 cex.lab=1.4)
            
            text(stims[,1],stims[,2],
                 labels=textLabels,
                 cex=1.5,
                 cex.axis=1.5,
                 cex.lab=1.4)
        }
    }

    else if (plotDim=="3d") {
        plot3d(stims[,1],stims[,2],stims[,3],
               xlab=xlab,
               ylab=ylab,
               zlab=zlab,
               box=FALSE,
               size=3,
               lwd=2,
               cex.axis=12,
               col=color)

        text3d(stims[,1],stims[,2],stims[,3],
               labels=textLabels,
               cex=1.5,
               cex.axis=1.5,
               cex.lab=1.4)
    }

}



plotStimuliResponseFunction <- function(modelObject,responseFieldAddress=c(""),
                                        paramNames=c(),
                                        categoryName="",
                                        xlab="",ylab="",
                                        xlim=c(),ylim=c(),
                                        main="",color="",
                                        pointScale=1,
                                        plotDim="2d",
                                        formantPlane=FALSE) {
  
    allPoints <- modelObject[[responseFieldAddress]][,paramNames]
    responseValues <- modelObject[[responseFieldAddress]][,categoryName]
    responseValues <- (pointScale)*responseValues
    
    if (plotDim=="2d") {

        if (formantPlane) {
            plot(allPoints[,1],allPoints[,2],
                 main=main, 
                 xlab=xlab,
                 ylab=ylab,
                 xlim=rev(range(allPoints[,1])),
                 ylim=rev(range(allPoints[,2])),
                 pch=16,
                 cex=responseValues,
                 col=color,
                 cex.axis=1.5,
                 cex.lab=1.4)
        }

        else  {
            plot(allPoints[,1],allPoints[,2],
                 main=main, 
                 xlab=xlab,
                 ylab=ylab,
                 xlim=xlim,
                 ylim=ylim,
                 pch=16,
                 cex=responseValues,
                 col=color,
                 cex.axis=1.5,
                 cex.lab=1.4)
        }
    }
}



plotResponseSurface <- function(modelObject,
                                responseFieldAddress=c(""),
                                paramNames=c(),                              
                                categoryName="",
                                xlab="",ylab="",
                                xlim=c(),ylim=c(),
                                main="",formantPlane=FALSE) {

    s <- interp(x=modelObject[[responseFieldAddress]][,paramNames[1]],
                y=modelObject[[responseFieldAddress]][,paramNames[2]],
                z=modelObject[[responseFieldAddress]][,categoryName],duplicate="strip")


    if (formantPlane) {
    
        filled.contour(s$x,s$y,s$z,
                       main=main, 
                       xlab=xlab,
                       ylab=ylab,
                       xlim=rev(range(s$x)),
                       ylim=rev(range(s$y)),
                       color = topo.colors,
                       key.title = title(sub="Scale\nLevel\n\n\n",cex.main=0.9),
                       cex.axis=1.5,
                       cex.lab=1.4)
    }

    else {

        filled.contour(s$x,s$y,s$z,
                       main=main, 
                       xlab=xlab,
                       ylab=ylab,
                       xlim=xlim,
                       ylim=ylim,
                       color = topo.colors,
                       key.title = title(sub="Scale\nLevel\n\n\n",cex.main=0.9),
                       cex.axis=1.5,
                       cex.lab=1.4)
    }

        
}



plotPairing <- function(modelObject,
                        dataSetAddress1=c(""),
                        dataSetAddress2=c(""),
                        paramNames=c(),
                        pairingAddress=c(""),
                        categoryNames=c(""),
                        dataColorNames=c(""),
                        categoryColorNames=c(""),
                        xlab1="",ylab1="",main1="",
                        xlab2="",ylab2="",main2="",
                        zoom=FALSE,zoomFactor=1,plotDim="2d",formantPlane=FALSE) {

    mapList <- list()
    for (categoryName in categoryNames) {
        mapList[[categoryName]] <- modelObject[[pairingAddress]][[categoryName]]
    }


    numOfPairs <- lapply(categoryNames,
                          function(catName) dim(mapList[[catName]])[1])

    colorVec <- rep(categoryColorNames,times=numOfPairs)
    
    mapInds <- do.call("rbind",mapList)[,c(2,4)]
    mapInds1 <- mapInds[,1]
    mapInds2 <- mapInds[,2]


    dataSet1 <- modelObject[[dataSetAddress1]][,paramNames]
    dataSet2 <- modelObject[[dataSetAddress2]][,paramNames]

    xlims <- range(c(range(dataSet1[,1]),range(dataSet2[,1])))
    ylims <- range(c(range(dataSet1[,2]),range(dataSet2[,2])))

    if (formantPlane) {
        xlims <- rev(xlims)
        ylims <- rev(ylims)
    }
        
    if (plotDim=="2d") {
    
        
        if (zoom==FALSE) {
            
            par(mfrow=c(1,2))
            
            plot(dataSet1[,1],dataSet1[,2],
                 main=main1, 
                 xlab=xlab1,
                 ylab=ylab1,
                 xlim=xlims,
                 ylim=ylims,
                 pch=16,
                 col=dataColorNames[1],
                 cex.axis=1.5,
                 cex.lab=1.4)
            points(dataSet1[mapInds1,1],dataSet1[mapInds1,2],col=colorVec,pch=19)
            
            plot(dataSet2[,1],dataSet2[,2],
                 main=main2, 
                 xlab=xlab2,
                 ylab=ylab2,
                 xlim=xlims,
                 ylim=ylims,
                 pch=16,
                 col=dataColorNames[2],
                 cex.axis=1.5,
                 cex.lab=1.4)
            points(dataSet2[mapInds2,1],dataSet2[mapInds2,2],col=colorVec,pch=19)
        }

        
        else if (zoom=="TRUE") {
            
            numberOfCats <- length(mapList)
            par(mfcol=c(2,numberOfCats))
            
            colInd <- 1
            
            categoryColorList <- as.list(categoryColorNames)
            names(categoryColorList) <- categoryNames
            
            names(numOfPairs) <- categoryNames
            
            for (categoryName in categoryNames) {


                xlim1 <- (1/zoomFactor)*(c(-1,1))+range(dataSet1[mapList[[categoryName]][,2],1])
                ylim1 <- (1/zoomFactor)*(c(-1,1))+range(dataSet1[mapList[[categoryName]][,2],2])

                if (formantPlane) {
                    xlim1 <- rev(xlim1)
                    ylim1 <- rev(ylim1)
                }
                
                plot(dataSet1[,1],dataSet1[,2],
                     main=categoryName, 
                     xlab="",
                     ylab="",
                     pch=16,
                     xlim=xlim1,
                     ylim=ylim1,
                     col=dataColorNames[1],
                     cex.axis=1.5,
                     cex.lab=1.4)
                points(dataSet1[mapList[[categoryName]][,2],1],
                       dataSet1[mapList[[categoryName]][,2],2],
                       col=rep(categoryColorList[[categoryName]],numOfPairs[[categoryName]]),pch=20,cex=7)
                text(dataSet1[mapList[[categoryName]][,2],1],
                     dataSet1[mapList[[categoryName]][,2],2],
                     col="white",labels=as.character(1:(numOfPairs[[categoryName]])),cex=1.5)


                xlim2 <- (1/zoomFactor)*(c(-1,1))+range(dataSet2[mapList[[categoryName]][,4],1])
                ylim2 <- (1/zoomFactor)*(c(-1,1))+range(dataSet2[mapList[[categoryName]][,4],2])

                if (formantPlane) {
                    xlim2 <- rev(xlim2)
                    ylim2 <- rev(ylim2)
                }
                
                
                plot(dataSet2[,1],dataSet2[,2],
                     main=categoryName, 
                     xlab="",
                     ylab="",
                     pch=16,
                     xlim=xlim2,
                     ylim=ylim2,
                     col=dataColorNames[2],
                     cex.axis=1.5,
                     cex.lab=1.4)
                points(dataSet2[mapList[[categoryName]][,4],1],
                       dataSet2[mapList[[categoryName]][,4],2],
                       col=rep(categoryColorList[[categoryName]],numOfPairs[[categoryName]]),pch=20,cex=7)
                text(dataSet2[mapList[[categoryName]][,4],1],
                     dataSet2[mapList[[categoryName]][,4],2],
                     col="white",labels=as.character(1:(numOfPairs[[categoryName]])),cex=1.5)
                
                colInd <- colInd + 1
            }
        }  
    }

    else if (plotDim=="3d") {

        dataColorVec1 <- c(rep(dataColorNames[1],dim(dataSet1)[1]))
        dataColorVec2 <- c(rep(dataColorNames[2],dim(dataSet2)[1]))

        dataColorVec1[mapInds1] <- colorVec
        dataColorVec2[mapInds2] <- colorVec

        dataColorVec <- c(dataColorVec1,dataColorVec2)
        
        plot3d(x=c(dataSet1[,1],dataSet2[,1]),
               y=c(dataSet1[,2],dataSet2[,2]),
               z=c(dataSet1[,3],dataSet2[,3]),
               main="",
               xlab="",
               ylab="",
               zlab="",
               col=dataColorVec,
               box=FALSE,
               axes=TRUE,
               size=3,
               lwd=2,
               cex.axis=12)
    }

}


plotPaths <- function(modelObject,
                      pathSetAddresses=list(),
                      dataSetAddresses=list(),
                      ambientSpaceAddresses=list(),
                      paramNames=c(),
                      pathColorNames=c(""),
                      ambientColorNames=c(""),
                      xlab="",ylab="",zlab="",
                      main="",
                      formantPlane=FALSE,
                      plotDim="2d",alphaValue=1,
                      lineWidth=4) {

    plotData(modelObject,
             dataSetAddresses=ambientSpaceAddresses,
             paramNames=paramNames,
             xlab=xlab,ylab=ylab,zlab=zlab,
             main=main,
             colorNames=ambientColorNames,
             plotDim=plotDim,
             alphaValue=alphaValue,
             formantPlane=formantPlane)

    for (nameInd in 1:(length(pathSetAddresses))) {
    
        pathCoords <- modelObject[[pathSetAddresses[[nameInd]]]]
        dataSet <- modelObject[[dataSetAddresses[[nameInd]]]]
        
        for (pathInd in 1:(length(pathCoords))) {

            if (plotDim=="2d") {
                lines(dataSet[pathCoords[[pathInd]],paramNames],lwd=lineWidth,col=pathColorNames[nameInd])
            }

            else if (plotDim=="3d") {
                lines3d(dataSet[pathCoords[[pathInd]],paramNames],col=pathColorNames[nameInd],lwd=lineWidth)
            }           
        }
    }
}
