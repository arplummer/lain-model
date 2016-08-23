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
##  This script is a demonstration of response field construction and
##  comparison using a quadrilateral space.
######################################################################

## Loading required packages

library(mnormt)


## Loading required source files

source("../base-model/modelObjectFunctions.R",chdir=TRUE)
source("../base-model/visualizationFunctions.R")


## Setting output file name

figureDir <- "figures/demo-figures/quad-response-field-demo-figures/"

## Initiating a blank "quadrilateral object"

quadObject <- initiateModelObject()


## Generating some quadrilateral data and adding it to the quadObject.

quadX <- c(runif(3000),0,0,1,1)
quadY <- c(runif(3000),0,1,0,1)
quad <- cbind(quadX,quadY)

quadData <- as.data.frame(quad)
colnames(quadData) <- c("x","y")

quadObject <- addNewComponent(quadObject,
                              newComponent=quadData,
                              componentAddress=c("dataSets","unitQuad"))


pdf(file=paste0(figureDir,"quad_in_plane.pdf"),width=6,height=6)
plotData(quadObject,dataSetAddresses=list(c("dataSets","unitQuad")),paramNames=c("x","y"),
          xlab="",ylab="",zlab="",main="",colorNames=c("blue"),plotDim="2d")     
dev.off()


## Generating a "stimulus grid" that will later be used in "perception
## experiments" to obtain information about a subject's knowledge of
## the locations of the corners of the quads, and adding it to
## quadObject.

stimulusVec <- seq(from=0.05,to=0.95,by=0.05)

stimulusGrid <- as.matrix(expand.grid(stimulusVec,stimulusVec))
stimulusGridData <- as.data.frame(stimulusGrid)
colnames(stimulusGridData) <- c("x","y")

quadObject <- addNewComponent(quadObject,
                              newComponent=stimulusGridData,
                              componentAddress=c("stimulusSets","stimulusGrid"))


## In order to develop our methods of response surface generation and
## comparison, we'll first create three copies of quadObject, each of
## which will hold the response surfaces of three different
## "subjects".

quadSubject1 <- quadObject
quadSubject2 <- quadObject
quadSubject3 <- quadObject


## plotting the stimulus grid

pdf(file=paste0(figureDir,"stimulusGrid.pdf"),width=12,height=6)
plotStimuli(quadSubject1,stimSetAddress=c("stimulusSets","stimulusGrid"),
            paramNames=c("x","y"),
            color="white",textColor="black",textLabels=as.character(1:361),
            xlab="",ylab="",main="",plotDim="2d")
dev.off()



## Creating "category response data" that simulates several agents'
## reporting of their knowledge of the salient aspects (i.e., corners
## and sides) of the quad in terms of responses to the grid stimuli.

stimulusCorners <- stimulusGrid[c(1,19,343,361),]
stimulusCornersList <-  as.list(as.data.frame(t(stimulusCorners)))

stimulusSides <- stimulusGrid[c(10,190,172,352),]
stimulusSidesList <-  as.list(as.data.frame(t(stimulusSides)))


## Subject 1 from the "corner language".

varcovMatCornerSub1 <- matrix(c(0.01,0,0,0.01), ncol=2)

cornerValuesSub1 <- as.data.frame(mapply(dmnorm,stimulusCornersList,
                                         MoreArgs=list(x=stimulusGrid,varcov=varcovMatCornerSub1)))

colnames(cornerValuesSub1) <- c("corner1","corner2","corner3","corner4")

quadSubject1 <- addNewComponent(quadSubject1,
                                newComponent=cornerValuesSub1,
                                componentAddress=c("categoryResponseSets","categoryResponseSet"))



## Subject 2 from the "corner language".

varcovMatCornerSub2 <- matrix(c(0.04,0,0,0.04), ncol=2)

cornerValuesSub2 <- as.data.frame(mapply(dmnorm,stimulusCornersList,
                                         MoreArgs=list(x=stimulusGrid,varcov=varcovMatCornerSub2)))

colnames(cornerValuesSub2) <- c("corner1","corner2","corner3","corner4")


quadSubject2 <- addNewComponent(quadSubject2,
                                newComponent=cornerValuesSub2,
                                componentAddress=c("categoryResponseSets","categoryResponseSet"))



## Subject 3 from the "side language".

varcovMatSidesSub3 <- matrix(c(0.001,0,0,0.001), ncol=2)

sideValuesSub3 <- as.data.frame(mapply(dmnorm,stimulusSidesList,
                                         MoreArgs=list(x=stimulusGrid,varcov=varcovMatSidesSub3)))

colnames(sideValuesSub3) <- c("side1","side2","side3","side4")
                                                  
quadSubject3 <- addNewComponent(quadSubject3,
                                newComponent=sideValuesSub3,
                                componentAddress=c("categoryResponseSets","categoryResponseSet"))





## Creating "response fields" over the stimulusGridData using the
## category responses.

quadSubject1 <- generateStimulusResponseField(quadSubject1,
                                              stimulusSetAddress=c("stimulusSets","stimulusGrid"),
                                              categoryResponseSetAddress=c("categoryResponseSets","categoryResponseSet"),
                                              stimulusResponseFieldAddress=c("stimulusResponseFields","stimulusResponseField"))


pdf(file=paste0(figureDir,"stimulusResponse_sub1_corner1.pdf"),width=6,height=6)
plotStimuliResponseFunction(quadSubject1,responseFieldAddress=c("stimulusResponseFields", "stimulusResponseField"),
                            paramNames=c("x","y"),
                            categoryName="corner1",
                            xlab="",ylab="",main="",color="black",
                            plotDim="2d",pointScale=0.2)
dev.off()



quadSubject2 <- generateStimulusResponseField(quadSubject2,
                                              stimulusSetAddress=c("stimulusSets","stimulusGrid"),
                                              categoryResponseSetAddress=c("categoryResponseSets","categoryResponseSet"),
                                              stimulusResponseFieldAddress=c("stimulusResponseFields","stimulusResponseField"))


pdf(file=paste0(figureDir,"stimulusResponse_sub2_corner1.pdf"),width=6,height=6)
plotStimuliResponseFunction(quadSubject2,responseFieldAddress=c("stimulusResponseFields", "stimulusResponseField"),
                            paramNames=c("x","y"),
                            categoryName="corner1",
                            xlab="",ylab="",main="",color="black",
                            plotDim="2d",pointScale=0.2)
dev.off()


quadSubject3 <- generateStimulusResponseField(quadSubject3,
                                              stimulusSetAddress=c("stimulusSets","stimulusGrid"),
                                              categoryResponseSetAddress=c("categoryResponseSets","categoryResponseSet"),
                                              stimulusResponseFieldAddress=c("stimulusResponseFields","stimulusResponseField"))


pdf(file=paste0(figureDir,"stimulusResponse_sub3_side1.pdf"),width=6,height=6)
plotStimuliResponseFunction(quadSubject3,responseFieldAddress=c("stimulusResponseFields", "stimulusResponseField"),
                            paramNames=c("x","y"),
                            categoryName="side1",
                            xlab="",ylab="",main="",color="black",
                            plotDim="2d",pointScale=0.025)
dev.off()




## Creating "response fields" over the quadData using the stimulus
## grid stimulus response field values.

                                                      
quadSubject1 <- generateResponseField(quadSubject1,
                                      dataSetAddress=c("dataSets","unitQuad"),
                                      stimulusFieldAddress=c("stimulusResponseFields","stimulusResponseField"),
                                      dataResponseFieldAddress=c("dataResponseFields","dataResponseField"),
                                      responseVars=list("corner1","corner2","corner3","corner4"),
                                      smoothFormula="s(x,k=3)+s(y,k=3)")


pdf(file=paste0(figureDir,"categoryResponse_sub1_corner1.pdf"),width=6,height=6)
plotResponseSurface(quadSubject1,
                    paramNames=c("x","y"),
                    responseFieldAddress=c("dataResponseFields","dataResponseField"),
                    categoryName="corner1_pred",xlab="",ylab="",main="")
dev.off()


quadSubject2 <- generateResponseField(quadSubject2,
                                      dataSetAddress=c("dataSets","unitQuad"),
                                      stimulusFieldAddress=c("stimulusResponseFields","stimulusResponseField"),
                                      dataResponseFieldAddress=c("dataResponseFields","dataResponseField"),
                                      responseVars=list("corner1","corner2","corner3","corner4"),
                                      smoothFormula="s(x,k=3)+s(y,k=3)")
                                

pdf(file=paste0(figureDir,"categoryResponse_sub2_corner1.pdf"),width=6,height=6)
plotResponseSurface(quadSubject2,
                    paramNames=c("x","y"),
                    responseFieldAddress=c("dataResponseFields","dataResponseField"),
                    categoryName="corner1_pred",xlab="",ylab="",main="")
dev.off()


quadSubject3 <- generateResponseField(quadSubject3,
                                      dataSetAddress=c("dataSets","unitQuad"),
                                      stimulusFieldAddress=c("stimulusResponseFields","stimulusResponseField"),
                                      dataResponseFieldAddress=c("dataResponseFields","dataResponseField"),
                                      responseVars=list("side1","side2","side3","side4"),
                                      smoothFormula="s(x,k=3)+s(y,k=3)")
                                      

pdf(file=paste0(figureDir,"categoryResponse_sub3_side1.pdf"),width=6,height=6)
plotResponseSurface(quadSubject3,
                    paramNames=c("x","y"),
                    responseFieldAddress=c("dataResponseFields","dataResponseField"),
                    categoryName="side1_pred",xlab="",ylab="",main="")
dev.off()



## distances between fields

## Across languages

dist1 <- getResponseFieldDistance(quadSubject1,quadSubject3,
                                  responseFieldAddress1=c("dataResponseFields","dataResponseField"),
                                  responseFieldAddress2=c("dataResponseFields","dataResponseField"),
                                  categoryNames1=c("corner1_pred"),
                                  categoryNames2=c("side1_pred"),
                                  distanceType="L1")

dist2 <- getResponseFieldDistance(quadSubject1,quadSubject3,
                                  responseFieldAddress1=c("dataResponseFields","dataResponseField"),
                                  responseFieldAddress2=c("dataResponseFields","dataResponseField"),
                                  categoryNames1=c("corner1_pred","corner2_pred"),
                                  categoryNames2=c("side1_pred","side2_pred"),
                                  distanceType="L1")

## Within languages

dist3 <- getResponseFieldDistance(quadSubject1,quadSubject2,
                                  responseFieldAddress1=c("dataResponseFields","dataResponseField"),
                                  responseFieldAddress2=c("dataResponseFields","dataResponseField"),
                                  categoryNames1=c("corner1_pred","corner2_pred"),
                                  categoryNames2=c("corner1_pred","corner2_pred"),
                                  distanceType="L1")

