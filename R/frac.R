#' FRaK: Feature Modeling Approach to Anomaly Detection
#'
#'@author Steve Bronder
#'@description The FRaC Algorithm
#'@usage frac(x,  models, n.cv=2, keys = NULL grid=NULL, allowParallel = FALSE, tuneList = NULL)
#'
#'@details Version 0.0.3 Alpha
#'
#'@param x An NxT matrix with T observations of N varialbles
#'
#'@param keys A vector of integers representing places that the data set has time variables. This is only here for short term bug fixes
#'
#'@param tuneList A list of models for train functions 
#'
#'@param models a string vector of models available in the caret package
#'
#'@param n.cv An integer specifying the number of cross-validations
#'
#'@param grid A list containing the different parameter values for each models iterations. Can be set to default NULL
#'
#'@param allowParallel Logical definining whether or not the user would like to use a parallel backend if one is set up
#'
#'@return values or sup: the normalised suprisal score for each observation. Higher scores equate to a higher chance of an observation being an outlier
#'
#'@references K. Noto, C. E. Brodley, and D. Slonim. 
#'FRaC: A Feature-Modeling Appraoch for Semi-Supervised and Unsupervised Anomaly Detection. 
#'Data Mining and Knowledge Discovery, 25(1), pp.109—133, 2011.
#'
frac <- function(x, models = c("C5.0","glm", "rf","pls"),keys = NULL, n.cv=10, allowParallel = FALSE, tuneList=NULL, ...){
  # make X into a matrix
  # Having the time variables come in made everything all goofy doofy? You can set them as keys
  # So they are not the y, but will allows them to be predictors.
  if (!is.null(keys)){
    xKeys <- x[,keys]
    
    x <- as.data.frame(x[,-keys])
    
  }else{
  x <- as.data.frame(x)
  }
  
  
  # gather data length and width
  D <- dim(x)[2]
  N <- dim(x)[1]
  
  #number of bins to be used later
  sq.N <- as.integer(ceiling(sqrt(N)))
  
  # Create the variable for the probability distribution of each error term
  AllErrorProbs <- NULL
  
  # Classify out which models are regresssions
  RegModels <-  which(
    unlist(
      sapply(models, function(k){
        modelLookup(k)$forReg[1]
      })
    ) == "TRUE"
  )
  
  TuneRegModels <-  which(
    unlist(
      sapply(1:length(tuneList), function(k){
        modelLookup(tuneList[[k]][[1]])$forReg[1]
      })
    ) == "TRUE"
  )
  
  
  # for i in one to number of features
  ErrorProbability <- lapply(1:D, function(i) {
    
    if (!is.null(keys)){
      y <- x[,i]
      
      x.m <- cbind(x[-i], xKeys)
      
    }else{
    # Grab y variable
    y <- x[,i]
    
    # grab rest of the matrix
    x.m <- x[,-i]
    }
    # Controls for caret
    controls <- trainControl(method = "cv",
                             number=n.cv,
                             savePredictions=TRUE,
                             index = createFolds(y,k=n.cv,returnTrain=TRUE),
                             allowParallel = allowParallel)
    
    # If the y variable is numeric then cut out
    # all the classification models and vice versa
    if (is.numeric(y) == "TRUE"){
      ModelToDo <- models[RegModels]
      TuneToDo <- tuneList[TuneRegModels]
      
    }else{
      ModelToDo <- models[-RegModels]
      TuneToDo <- tuneList[-TuneRegModels]
    }
    
    #Do models together
    TrainMethod <- suppressMessages(suppressWarnings(caretList(y=y,x=x.m,
                                              methodList=ModelToDo,
                                              trControl = controls,
                             tuneList=TuneToDo)))
    
    
    PredClass <- predict(TrainMethod,x.m)
    
    if (is.numeric(y) != "TRUE") PredClass <- as.data.frame(factor(PredClass,levels=levels(y),labels=levels(y)))
    # make data frame of errors and row numbers
    #      PredClass <- do.call(cbind, lapply(1:length(ModelToDo),function(i){
    #        if (TrainMethod[[i]]$bestTune[1] == "none"){
    #          
    #          PreSort <- TrainMethod[[i]]$pred[,c("pred","rowIndex")]
    #    
    #          PostSort <- PreSort[ order(PreSort[,2]),1]
    #    
    #         
    #        }else{
    #          # Really wacky function to get find best resampling parameters
    #          PreSort <- TrainMethod[[i]]$pred[
    #              which(TrainMethod[[i]]$pred[
    #                colnames(TrainMethod[[i]]$bestTune)] = TrainMethod[[i]]$bestTune),
    #            c("pred","rowIndex")
    #            ]
    #           
    #          PostSort <- PreSort[ order(PreSort[,2]),1]
    #          
    #        }
    #      }
    #      )
    #      )
    # Set Numeric and Class Probs
    NumericProbs <- NULL
    ClassProbs <- NULL
    
    if (is.numeric(y) == "TRUE"){
      
      # Begin an lapply to go over the predictions from each model
      NumericProbs <- lapply(1:I(length(ModelToDo)+length(TuneToDo)), function(j){
        
        # make the error term
        Error <- y - PredClass[,j]
        
        # Make a data frame that consists of bins (key) and obs for sorting later
        ErrorBins <- data.frame( key = cut(Error,sq.N,labels=FALSE), obs = 1:N)
        
        # Find the density of each bin like in FRaC
        ErrorDensity <- density(ErrorBins$key,n=sq.N,kernel = "gaussian")$y
        
        # Make a data frame for containing bins (key) and the density of each bin
        ErrorDensityKey <- data.frame(key = 1:sq.N, value = ErrorDensity)
        
        # Merge together the Bins for each observation with the density of each bin
        NumericMerge <- merge(ErrorBins, ErrorDensityKey, "key" )[,c("obs","value")]
        
        # Order the newly merged data by the row observations, keeping only the value variable
        NumericMerge <- as.matrix(NumericMerge[ do.call(order, NumericMerge),2])
        
        # change column name to correspond to each model
        colnames(NumericMerge) <- paste0(ModelToDo[j],"Probs",i)
        
        return(NumericMerge)
      })
      return(NumericProbs)
    }else{
      
      # Begin an apply to find class probabilities for each observation
      ClassProbs <- lapply(1:I(length(ModelToDo)+length(TuneToDo)), function(j){
        
        
        
        # Make confusion matrix for each model
        ConfuseTable <- confusionMatrix(PredClass[,j],y)$table
        
        # Get cound for each row of confusion table 
        RowCount <- apply(ConfuseTable,1,sum)
        
        # normalize the cound of each confusion table
        # Noto adds 1 to each value, but I'm questionable about this?
        ConfuseNorm <- (ConfuseTable + 1) / RowCount
        
        ConfuseNorm[which(ConfuseNorm==Inf)] <- 0.001
        # Melt the data frame to get ready for merge
        ConfuseMelt <- reshape2::melt(ConfuseNorm)
        
        # Create key of class/class and associated probability (Right Merger)
        RightMerge<- data.frame(key = paste0(ConfuseMelt[,1], ConfuseMelt[,2]),value = ConfuseMelt$value)
        
        # Create a key of class/class and associated observation (Left Merger)
        LeftMerge <- data.frame(key = paste0(as.character(y),as.character(PredClass[,j])),obs = 1:N)
        
        # Push through merge by key. Drops everything but observation and value
        ConfuseMerge <- merge(LeftMerge, RightMerge, "key",all.x = TRUE, all.y =FALSE)[,c("obs", "value")]
        
        # Sort the class proababilities by observation, leaving only the probs
        ConfuseMerge <- as.matrix(ConfuseMerge[ do.call(order,ConfuseMerge),2])
        
        #make column name assocated with model
        colnames(ConfuseMerge) <- paste0(ModelToDo[j],"Probs",i)
        
        return(ConfuseMerge)
        
      })
      
      return(ClassProbs)
    }
  }
)
  # ??????????????????
  
  AllErrorProbs<-do.call(cbind,as.data.frame(ErrorProbability))

  #define suprisal for each model. Still need to account for NAs
  Suprise <- data.frame( Suprise = apply(apply(AllErrorProbs,2,function(x) -log2(x)),1,sum), obs = 1:N)
  
  
  # I'm not sure if this is necessary. On page 6 the definiton of entrophy starts getting strange...
  ProportionsKey <- as.data.frame(table(Suprise$Suprise))
  

  ProportionsKey$Freq <- ProportionsKey$Freq / length(as.numeric(Suprise$Suprise))
  
  colnames(ProportionsKey) <- c("Suprise", "Freq")
  
  ErrorProportions <- merge(Suprise,ProportionsKey,by = "Suprise")[,2:3]
  
  ErrorProportions <- as.matrix(ErrorProportions[ do.call(order,ErrorProportions),2])
  
  # define entrophy of models
  Entropy <- sum(-ErrorProportions * log2(ErrorProportions))
  
  
  norm.suprise <- Suprise$Suprise - Entropy
  
  
  
  values <- list(sup=norm.suprise)
  
  return(values)
}
