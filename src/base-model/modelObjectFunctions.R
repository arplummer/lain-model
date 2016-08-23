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
## Several functions that initiate and modify "objects" used to model
## agents within Lain.
######################################################################

## Loading required packages

library(mgcv)


## Loading required source

source("manifoldFunctions.R")
source("pathOperations.R")


## The function initiateModelObject creates a "model object" which
## contains a set of salient data sets, a set of stimulus sets drawn
## from these data sets, a set of perceptual categorizations of these
## stimuli by a subject, a set of "response fields" constructed over
## the stimulus sets, a set of response fields constructed over the
## data sets (typically using the stimulus response fields), a set of
## pairing structures derived from response fields over two data sets
## that link the data sets in some way, a set of structures formed
## over the data sets, a set of paths within these structures, and a
## list of information about the model object.

initiateModelObject <- function(objectInformation=list(),
                                dataSets=list(),
                                stimulusSets=list(),
                                rawResponseSets=list(),
                                categoryResponseSets=list(),
                                dataResponseFields=list(),
                                stimulusResponseFields=list(),
                                pairingSets=list(),
                                alignmentSets=list(),
                                structureSet=list(),
                                mappingSet=list(),
                                pathSets=list()) {

    newObject <- list()
    newObject[["info"]] <- objectInformation
    newObject[["dataSets"]] <- dataSets
    newObject[["stimulusSets"]] <- stimulusSets
    newObject[["rawResponseSets"]] <- rawResponseSets
    newObject[["categoryResponseSets"]] <- categoryResponseSets
    newObject[["dataResponseFields"]] <- dataResponseFields
    newObject[["stimulusResponseFields"]] <- stimulusResponseFields 
    newObject[["pairingSets"]] <- pairingSets
    newObject[["alignmentSets"]] <- alignmentSets
    newObject[["structureSet"]] <- structureSet
    newObject[["mappingSet"]] <- mappingSet
    newObject[["pathSets"]] <- pathSets
    
    return(newObject)

}


## The function addNewComponent provides for adding new components,
## e.g., data sets, stimulus sets, structures, etc., to a model
## object.


addNewComponent <- function(modelObject, newComponent, componentAddress=c("")) {

    modelObject[[componentAddress]] <- newComponent
    return(modelObject)

}


## The function generateStimulusResponseField takes a stimulus set and
## a corresponding category response set and combines them to form a
## stimulus response field.

generateStimulusResponseField <- function(modelObject,
                                          stimulusSetAddress=c(""),
                                          categoryResponseSetAddress=c(""),
                                          stimulusResponseFieldAddress=c("")) {

    stimulusSet <- modelObject[[stimulusSetAddress]]
    categoryResponseSet <- modelObject[[categoryResponseSetAddress]]

    stimulusResponseField <- cbind(stimulusSet,categoryResponseSet)
    modelObject <- addNewComponent(modelObject,stimulusResponseField,
                                   componentAddress=stimulusResponseFieldAddress)
    return(modelObject)
}


## A helper function that assigns category response values to data a
## frame using a stimulus response field.

generateResponseValues <- function(dataFrame,
                                   stimulusField,
                                   responseVar="z",
                                   modelType="gam",
                                   smoothFormula="s(x,k=3)+s(y,k=3)") {

    if (modelType=="gam") {
    
        formulaLHS <- paste(responseVar,"~")
        formulaRHS <- smoothFormula
        regressionFormula <- as.formula(paste(formulaLHS,formulaRHS))
        
        responseFit <- gam(regressionFormula,data=stimulusField)
        responseValues <- as.data.frame(predict.gam(responseFit,dataFrame))
        colnames(responseValues) <- paste0(responseVar,"_pred")    

        return(responseValues)
    }
    
    else { print("Specify a proper model type") }
   
}


## The function generateResponseField creates a response field over a
## data set using a stimulus response field.

generateResponseField <- function(modelObject,
                                  dataSetAddress=c(""),
                                  stimulusFieldAddress=c(""),
                                  dataResponseFieldAddress=c(""),
                                  responseVars=list(),
                                  modelType="gam",
                                  smoothFormula="s(x,k=3)+s(y,k=3)") {

    inputDataFrame <- modelObject[[dataSetAddress]]

    inputStimulusField <- modelObject[[stimulusFieldAddress]]
    
    responseFieldList <- mapply(generateResponseValues,responseVars,
                                MoreArgs=list(dataFrame=inputDataFrame,
                                    stimulusField=inputStimulusField,
                                    modelType=modelType,
                                    smoothFormula=smoothFormula))

    responseFieldValues <- do.call(cbind.data.frame, responseFieldList)

    responseField <- cbind(inputDataFrame,responseFieldValues)

    modelObject <- addNewComponent(modelObject,responseField,
                                   componentAddress=dataResponseFieldAddress)

    return(modelObject)

}



## distance between response fields

getResponseFieldDistance <- function(modelObject1,modelObject2,
                                     responseFieldAddress1=c(""),responseFieldAddress2=c(""),
                                     categoryNames1=c(),categoryNames2=c(),
                                     distanceType="L1") {

    responseField1 <- modelObject1[[responseFieldAddress1]]
    responseField2 <- modelObject2[[responseFieldAddress2]]

    responseField1Reduced <- responseField1[,categoryNames1]
    responseField2Reduced <- responseField2[,categoryNames2]
    
    if (distanceType=="L1") {

        if (length(categoryNames1) == 1) {
            responseFieldDistances <- sum(abs(responseField1Reduced - responseField2Reduced))
        }
        else {
            responseFieldDistances <- colSums(abs(responseField1Reduced - responseField2Reduced))
        }
    }
        
    return(responseFieldDistances)
}



## pairing creation.

generateCategoryPairing <- function(modelObject,
                                    responseFieldAddress1=c(""),
                                    responseFieldAddress2=c(""),
                                    pairingAddress=c(""),
                                    categoryNames=c(),pairingType="maxOrd",
                                    pairingNum=20) {
    
    pairingList <- list()

    if (pairingType=="maxOrd") {

        for (categoryName in categoryNames) {

            rf1 <- modelObject[[responseFieldAddress1]]
            rf2 <- modelObject[[responseFieldAddress2]]

            rf1Sorted <- sort(rf1[,categoryName],decreasing=TRUE,index.return=TRUE)
            rf1Ratings <- rf1Sorted$x[1:pairingNum]
            rf1Indices <- rf1Sorted$ix[1:pairingNum]
            
            rf2Sorted <- sort(rf2[,categoryName],decreasing=TRUE,index.return=TRUE)
            rf2Ratings <- rf2Sorted$x[1:pairingNum]
            rf2Indices <- rf2Sorted$ix[1:pairingNum]

            pairingStructure <- cbind(rf1Ratings,rf1Indices,rf2Ratings,rf2Indices)
            colnames(pairingStructure) <- c(paste0(tail(responseFieldAddress1,n=1),"_ratings"),
                                            paste0(tail(responseFieldAddress1,n=1),"_index"),
                                            paste0(tail(responseFieldAddress2,n=1),"_ratings"),
                                            paste0(tail(responseFieldAddress2,n=1),"_index"))
                                            
            
            pairingList[[categoryName]] <- pairingStructure
        }
    }

    modelObject <- addNewComponent(modelObject,pairingList,
                                       componentAddress=pairingAddress)
    
    return(modelObject)
    
}



## transering pairing information from one model object to another. 

transferPairing <- function(modelObject1,modelObject2,
                            pairingAddress=c("")){
    
    modelObject2[[pairingAddress]] <-  modelObject1[[pairingAddress]]
    
    return(modelObject2)
}


## a function that creates an alignment from a given pairing.  

pairingToAlignment <- function(modelObject,
                               pairingAddress=c(""),alignmentAddress=c(""),
                               categoryList=list(),
                               alignmentType="constant",
                               alignmentWeight=10) {

    indList <- list()
    
    for (categoryName in categoryList) {
        indList[[categoryName]] <- modelObject[[pairingAddress]][[categoryName]]
    }

    aligRaw <- do.call("rbind",indList)
    aligNoReps <- aligRaw[!duplicated(aligRaw[,2]),]
    aligNoReps <- aligNoReps[!duplicated(aligNoReps[,4]),]

    alignmentPairs <- aligNoReps[,c(2,4)]
    

    if (alignmentType=="constant") {

        alignmentWeights <- rep(alignmentWeight,dim(alignmentPairs)[1])
    }

    alignmentPairing <- cbind(alignmentPairs,alignmentWeights)

    modelObject <- addNewComponent(modelObject,
                                   alignmentPairing,
                                   componentAddress=alignmentAddress)

    return(modelObject)
    
}


## the function generateManifold creates a manifold using a specified
## data set within a model object.

generateManifold <- function(modelObject,
                             dataSetAddress=c(""),
                             manifoldAddress=c(""),
                             adjType="nn",
                             adjParam=10,
                             includeLoops=FALSE,
                             weightType="constant",
                             weightParam=1) {

    dataSet <- modelObject[[dataSetAddress]]
    newAdjMatrix <- adjacencyRelation(dat=dataSet,adjType=adjType,adjParam=adjParam,includeLoops=includeLoops) 
    newWeightMatrix <- weightedAdjacency(dat=dataSet,adjMatrix=newAdjMatrix,weightType=weightType,weightParam=weightParam)

    newStructure <- list(inputSet=dataSet,graphStructure=newWeightMatrix)
    
    modelObject <- addNewComponent(modelObject,newStructure,
                                       componentAddress=manifoldAddress)
    
    return(modelObject)
}
    
## align manifolds within a model object using a specified alignment
## to generate an eigenmap.

generateEigenmap <- function(modelObject,
                             manifoldAddress1=c(""),
                             mappingName1=c(""),   
                             mappingAddress=c("")) {

    manifold1 <- modelObject[[manifoldAddress1]]

    weightMatrix1 <- manifold1$graphStructure
    
    
    eigenMapValues <- weightedAdjacencyEigenmap(weightMatrix1,lapType="unnormalized")
    eigenVals <- eigenMapValues[[1]]
    eigenReps1 <- eigenMapValues[[2]]

    
    eigenMap <- list(manifoldAddress1,eigenVals,eigenReps1)
    names(eigenMap)[1] <- "manifoldAddress1"
    names(eigenMap)[2] <- "eigenvalues"
    names(eigenMap)[3] <- mappingName1

    
    modelObject <- addNewComponent(modelObject,
                                   eigenMap,
                                   componentAddress=mappingAddress)
    
    return(modelObject)
}



generateEigenmapFromAlignment <- function(modelObject,
                                          manifoldAddress1=c(""),
                                          manifoldAddress2=c(""),
                                          alignmentAddress=c(""),
                                          mappingAddress=c(""),
                                          aligRepName1="",
                                          aligRepName2="") {

    manifold1 <- modelObject[[manifoldAddress1]]
    manifold2 <- modelObject[[manifoldAddress2]]

    weightMatrix1 <- manifold1$graphStructure
    weightMatrix2 <- manifold2$graphStructure
    
    alignmentPairing <- modelObject[[alignmentAddress]]

    aligPairs <- alignmentPairing[,c(1,2)]
    aligWeights <- alignmentPairing[,3]
    
    eigenMapValues <- combinedWeightedAdjacencyEigenmap(weightMatrix1,weightMatrix2,aligPairs,mu=aligWeights,lapType="unnormalized")
    eigenVals <- eigenMapValues[[1]]
    eigenReps1 <- eigenMapValues[[2]]
    eigenReps2 <- eigenMapValues[[3]]
    
    eigenMap <- list(manifoldAddress1,manifoldAddress2,alignmentAddress,eigenVals,eigenReps1,eigenReps2)
    names(eigenMap)[1] <- "manifoldAddress1"
    names(eigenMap)[2] <- "manifoldAddress2"
    names(eigenMap)[3] <- "alignmentAddress"
    names(eigenMap)[4] <- "eigenvalues"
    names(eigenMap)[5] <- aligRepName1
    names(eigenMap)[6] <- aligRepName2

    modelObject <- addNewComponent(modelObject,
                                   eigenMap,
                                   componentAddress=mappingAddress)
    
    return(modelObject)
}



## a function that generates paths within manifolds using pairings.  

generatePairingPaths <- function(modelObject,
                                 pairingAddress=c(""),
                                 manifoldAddress=c(""),
                                 pathSetAddress=c(""),
                                 categoryNames=c(),
                                 pairingComponent=1,
                                 pairingIndices=c()) {

    ## pairing indices indicate which points in the pairing are used
    ## to generate paths

    ## pairing component indicates which set of points in the pairing
    ## (first or second) for each category is to be used to generate
    ## the paths.

    
    manifold <- modelObject[[manifoldAddress]]
    weightMatrix <- manifold$graphStructure
    
    pairingSet <- modelObject[[pairingAddress]]
    
    initCategory <- categoryNames[1]
    termCategory <- categoryNames[2]

    initCategoryPairing <- pairingSet[[initCategory]]
    termCategoryPairing <- pairingSet[[termCategory]] 

    initialVertices <- initCategoryPairing[pairingIndices,pairingComponent*2]
    terminalVertices <- termCategoryPairing[pairingIndices,pairingComponent*2]


    shortestPaths <- mapply(getShortestPath,
                            init=initialVertices,
                            fin=terminalVertices,
                            MoreArgs=list(weightMatrix=weightMatrix),
                            SIMPLIFY=FALSE)

    modelObject <- addNewComponent(modelObject,shortestPaths,
                                   componentAddress=pathSetAddress)
    
    return(modelObject)    
}
