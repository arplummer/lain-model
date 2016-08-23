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
##  This script is a demonstration of manifold alignment based on
##  response field construction.  
######################################################################



## Loading required packages

library(mnormt)


## Loading required source files

source("../base-model/modelObjectFunctions.R",chdir=TRUE)
source("../base-model/manifoldFunctions.R")
source("../base-model/visualizationFunctions.R")
source("../base-model/pathOperations.R")


## Setting output file for figures.

figureDir <- "figures/demo-figures/quad-sequence-response-field-demo-figures/"



## Initiating a blank "quadrilateral object"

quadObject <- initiateModelObject()



## Generating the quadrilateral data

stimulusVec <- seq(from=0.05,to=0.95,by=0.05)
stimulusGrid <- as.matrix(expand.grid(stimulusVec,stimulusVec))

quadX <- c(runif(3000),0,0,1,1)
quadY <- c(runif(3000),0,1,0,1)
quad <- cbind(quadX,quadY)

quad <- rbind(stimulusGrid,quad)



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


## Organizing the quad data and stimulus data

stimulusGridData1 <- as.data.frame(quad1[1:dim(stimulusGrid)[1],])
colnames(stimulusGridData1) <- c("x","y")

stimulusGridData2 <- as.data.frame(quad2[1:dim(stimulusGrid)[1],])
colnames(stimulusGridData2) <- c("x","y")

quadObject <- addNewComponent(quadObject,
                              newComponent=stimulusGridData1,
                              componentAddress=c("stimulusSets","stimulusGrid1"))

quadObject <- addNewComponent(quadObject,
                              newComponent=stimulusGridData2,
                              componentAddress=c("stimulusSets","stimulusGrid2"))


quad1Data <- as.data.frame(quad1[(dim(stimulusGrid)[1]+1):dim(quad1)[1],])
colnames(quad1Data) <- c("x","y")

quad2Data <- as.data.frame(quad2[(dim(stimulusGrid)[1]+1):dim(quad2)[1],])
colnames(quad2Data) <- c("x","y")


quadObject <- addNewComponent(quadObject,
                              newComponent=quad1Data,
                              componentAddress=c("dataSets","quad1"))


quadObject <- addNewComponent(quadObject,
                              newComponent=quad2Data,
                              componentAddress=c("dataSets","quad2"))

quadSubject1 <- quadObject


pdf(file=paste0(figureDir,"quads_in_plane.pdf"),width=6,height=6)
plotData(quadSubject1,dataSetAddresses=list(c("dataSets","quad1"),c("dataSets","quad2")),paramNames=c("x","y"),
          xlab="",ylab="",zlab="",main="",colorNames=c("blue","magenta"),plotDim="2d")     
dev.off()

pdf(file=paste0(figureDir,"stimulusGrid1_in_plane.pdf"),width=12,height=12)
plotStimuli(quadSubject1,stimSetAddress=c("stimulusSets","stimulusGrid1"),
            paramNames=c("x","y"),
            color="white",textColor="black",textLabels=as.character(1:361),
            xlab="",ylab="",main="",plotDim="2d")
dev.off()


## Creating "category response data" that simulates a subject's
## reporting of their knowledge of the salient aspects (i.e., corners)
## of the quad in terms of responses to the grid stimuli.

stimulusCorners1 <- stimulusGridData1[c(1,19,343,361),]
stimulusCornersList1 <-  as.list(as.data.frame(t(stimulusCorners1)))


## Subject 1 from the "corner language".

varcovMatCornerQuad1 <- matrix(c(0.05,0,0,0.05), ncol=2)

cornerValuesQuad1 <- as.data.frame(mapply(dmnorm,stimulusCornersList1,
                                         MoreArgs=list(x=stimulusGridData1,varcov=varcovMatCornerQuad1)))

colnames(cornerValuesQuad1) <- c("corner1","corner2","corner3","corner4")

quadSubject1 <- addNewComponent(quadSubject1,
                                newComponent=cornerValuesQuad1,
                                componentAddress=c("categoryResponseSets","quad1ResponseSet"))

stimulusCorners2 <- stimulusGridData2[c(1,19,343,361),]
stimulusCornersList2 <-  as.list(as.data.frame(t(stimulusCorners2)))

varcovMatCornerQuad2 <- matrix(c(0.07,0,0,0.07), ncol=2)

cornerValuesQuad2 <- as.data.frame(mapply(dmnorm,stimulusCornersList2,
                                         MoreArgs=list(x=stimulusGridData2,varcov=varcovMatCornerQuad2)))

colnames(cornerValuesQuad2) <- c("corner1","corner2","corner3","corner4")

quadSubject1 <- addNewComponent(quadSubject1,
                                newComponent=cornerValuesQuad2,
                                componentAddress=c("categoryResponseSets","quad2ResponseSet"))


## Creating stimulus response fields over the stimulusGridData using
## the category responses.


quadSubject1 <- generateStimulusResponseField(quadSubject1,
                                              stimulusSetAddress=c("stimulusSets","stimulusGrid1"),
                                              categoryResponseSetAddress=c("categoryResponseSets","quad1ResponseSet"),
                                              stimulusResponseFieldAddress=c("stimulusResponseFields","stimulusResponseField1"))



quadSubject1 <- generateStimulusResponseField(quadSubject1,
                                              stimulusSetAddress=c("stimulusSets","stimulusGrid2"),
                                              categoryResponseSetAddress=c("categoryResponseSets","quad2ResponseSet"),
                                              stimulusResponseFieldAddress=c("stimulusResponseFields","stimulusResponseField2"))


pdf(file=paste0(figureDir,"stimulus1Response.pdf"),width=6,height=6)
plotStimuliResponseFunction(quadSubject1,responseFieldAddress=c("stimulusResponseFields","stimulusResponseField1"),
                            paramNames=c("x","y"),
                            categoryName="corner1",
                            xlab="",ylab="",main="",color="black",
                            plotDim="2d",pointScale=1.5)
dev.off()


## Creating "response fields" over the quadData using the stimulus
## grid stimulus response field values.

                                                      
quadSubject1 <- generateResponseField(quadSubject1,
                                      dataSetAddress=c("dataSets","quad1"),
                                      stimulusFieldAddress=c("stimulusResponseFields","stimulusResponseField1"),
                                      dataResponseFieldAddress=c("dataResponseFields","quad1ResponseField"),
                                      responseVars=list("corner1","corner2","corner3","corner4"),
                                      smoothFormula="s(x,k=3)+s(y,k=3)")     

quadSubject1 <- generateResponseField(quadSubject1,
                                      dataSetAddress=c("dataSets","quad2"),
                                      stimulusFieldAddress=c("stimulusResponseFields","stimulusResponseField2"),
                                      dataResponseFieldAddress=c("dataResponseFields","quad2ResponseField"),
                                      responseVars=list("corner1","corner2","corner3","corner4"),
                                      smoothFormula="s(x,k=3)+s(y,k=3)")     


## dev.new()
pdf(file=paste0(figureDir,"categoryResponse.pdf"),width=6,height=6)
plotResponseSurface(quadSubject1,
                    paramNames=c("x","y"),
                    responseFieldAddress=c("dataResponseFields","quad1ResponseField"),
                    categoryName="corner1_pred",xlab="",ylab="",main="")
dev.off()


## Generating Pairing Sets using the response fields

quadSubject1 <- generateCategoryPairing(quadSubject1,
                                        responseFieldAddress1=c("dataResponseFields","quad1ResponseField"),
                                        responseFieldAddress2=c("dataResponseFields","quad2ResponseField"),
                                        pairingAddress=c("pairingSets","test1"), 
                                        categoryNames=c("corner1_pred","corner2_pred","corner3_pred","corner4_pred"),
                                        pairingType="maxOrd",
                                        pairingNum=20)


## dev.new()
pdf(file=paste0(figureDir,"pairingStructure.pdf"),width=12,height=6)
plotPairing(quadSubject1,
            dataSetAddress1=c("dataSets","quad1"),
            dataSetAddress2=c("dataSets","quad2"),
            paramNames=c("x","y"),
            pairingAddress=c("pairingSets","test1"),
            categoryNames=c("corner1_pred","corner2_pred","corner3_pred","corner4_pred"),
            dataColorNames=c("blue","magenta"),
            categoryColorNames=c("green","orange","purple","black"),
            zoom=FALSE,zoomFactor=10)
dev.off()


pdf(file=paste0(figureDir,"pairingStructureZoom.pdf"),width=12,height=6)
plotPairing(quadSubject1,
            dataSetAddress1=c("dataSets","quad1"),
            dataSetAddress2=c("dataSets","quad2"),
            paramNames=c("x","y"),
            pairingAddress=c("pairingSets","test1"),
            categoryNames=c("corner1_pred","corner2_pred","corner3_pred","corner4_pred"),
            dataColorNames=c("blue","magenta"),
            categoryColorNames=c("green","orange","purple","black"),
            zoom=TRUE,zoomFactor=10)
dev.off()


## Generating alignment structures from the pairing sets

quadSubject1 <- pairingToAlignment(quadSubject1,
                                   pairingAddress=c("pairingSets","test1"),
                                   alignmentAddress=c("alignmentSets","aligQuad1Quad2"), 
                                   categoryList=list("corner1_pred","corner2_pred","corner3_pred","corner4_pred"),
                                   alignmentType="constant",
                                   alignmentWeight=10)




## Constructing Manifolds over the quad data sets

quadSubject1 <- generateManifold(quadSubject1,
                                 dataSetAddress=c("dataSets","quad1"),
                                 manifoldAddress=c("structureSet","quad1manifold"),
                                 adjType="nn",
                                 adjParam=20,
                                 includeLoops=FALSE,
                                 weightType="constant",
                                 weightParam=1)
                                 

quadSubject1 <- generateManifold(quadSubject1,
                                 dataSetAddress=c("dataSets","quad2"),
                                 manifoldAddress=c("structureSet","quad2manifold"),
                                 adjType="nn",
                                 adjParam=20,
                                 includeLoops=FALSE,
                                 weightType="constant",
                                 weightParam=1)
                                  

## Aligning the manifolds using the alignment structure

quadSubject1 <- generateEigenmapFromAlignment(quadSubject1,
                                              manifoldAddress1=c("structureSet","quad1manifold"),
                                              manifoldAddress2=c("structureSet","quad2manifold"),
                                              alignmentAddress=c("alignmentSets","aligQuad1Quad2"),
                                              mappingAddress=c("mappingSet","quadMapping"),
                                              aligRepName1="quad1Alig",
                                              aligRepName2="quad2Alig") 




quadSubject1 <- addNewComponent(quadSubject1,
                                newComponent=quadSubject1[["mappingSet"]][["quadMapping"]][["quad1Alig"]][,c(2:4)],
                                componentAddress=c("dataSets","quad1Alig"))

quadSubject1 <- addNewComponent(quadSubject1,
                                newComponent=quadSubject1[["mappingSet"]][["quadMapping"]][["quad2Alig"]][,c(2:4)],
                                componentAddress=c("dataSets","quad2Alig"))


## dev.new()

plotPairing(quadSubject1,
            dataSetAddress1=c("dataSets","quad1Alig"),
            dataSetAddress2=c("dataSets","quad2Alig"),
            paramNames=c(1:3),
            pairingAddress=c("pairingSets","test1"),
            categoryNames=c("corner1_pred","corner2_pred","corner3_pred","corner4_pred"),
            dataColorNames=c("blue","magenta"),
            categoryColorNames=c("green","orange","purple","black"),
            zoom=FALSE,plotDim="3d")


plotData(quadSubject1,dataSetAddresses=list(c("dataSets","quad1Alig"),c("dataSets","quad2Alig")),paramNames=c(1:3),
          xlab="",ylab="",zlab="",main="",colorNames=c("blue","magenta"),plotDim="3d")     



## Generating paths 

## A path in quad1manifold

quadSubject1 <- generatePairingPaths(quadSubject1,
                                     pairingAddress=c("pairingSets","test1"),
                                     manifoldAddress=c("structureSet","quad1manifold"),
                                     pathSetAddress=c("pathSets","paths0"),
                                     categoryNames=c("corner1_pred","corner4_pred"),
                                     pairingIndices=c(1))


## 2D plot of the path

pdf(file=paste0(figureDir,"path2d.pdf"),width=6,height=6)
plotPaths(quadSubject1,dataSetAddresses=list(c("dataSets","quad1"),c("dataSets","quad1")),
          ambientSpaceAddresses=list(c("dataSets","quad1")),
          paramNames=c("x","y"),
          pathSetAddresses=list(c("pathSets","paths0")),
          xlab="",ylab="",zlab="",main="",
          ambientColorNames=c("blue"),
          pathColorNames=c("orange"),
          plotDim="2d",alphaValue=0.3)     
dev.off()



## Generating manifolds over the aligned data

quadSubject1 <- generateManifold(quadSubject1,
                                 dataSetAddress=c("dataSets","quad1Alig"),
                                 manifoldAddress=c("structureSet","quad1Aligmanifold"),
                                 adjType="nn",
                                 adjParam=20,
                                 includeLoops=FALSE,
                                 weightType="constant",
                                 weightParam=1)
                               
                               
quadSubject1 <- generateManifold(quadSubject1,
                                 dataSetAddress=c("dataSets","quad2Alig"),
                                 manifoldAddress=c("structureSet","quad2Aligmanifold"),
                                 adjType="nn",
                                 adjParam=20,
                                 includeLoops=FALSE,
                                 weightType="constant",
                                 weightParam=1)
                               


## Generating paths within the manifolds

quadSubject1 <- generatePairingPaths(quadSubject1,
                                     pairingAddress=c("pairingSets","test1"),
                                     manifoldAddress=c("structureSet","quad1Aligmanifold"),
                                     pathSetAddress=c("pathSets","paths1"),
                                     categoryNames=c("corner2_pred","corner3_pred"),
                                     pairingComponent=1,
                                     pairingIndices=c(1,2))

quadSubject1 <- generatePairingPaths(quadSubject1,
                                     pairingAddress=c("pairingSets","test1"),
                                     manifoldAddress=c("structureSet","quad2Aligmanifold"),
                                     pathSetAddress=c("pathSets","paths2"),
                                     categoryNames=c("corner2_pred","corner3_pred"),
                                     pairingComponent=2,
                                     pairingIndices=c(1,2))


## 3D plot of the paths

plotPaths(quadSubject1,dataSetAddresses=list(c("dataSets","quad1Alig"),c("dataSets","quad2Alig")),
          ambientSpaceAddresses=list(c("dataSets","quad1Alig"),c("dataSets","quad2Alig")),
          paramNames=c(1:3),
          pathSetAddresses=list(c("pathSets","paths1"),c("pathSets","paths2")),
          xlab="",ylab="",zlab="",main="",
          ambientColorNames=c("blue","magenta"),
          pathColorNames=c("green","orange"),
          plotDim="3d",alphaValue=0.3)     
