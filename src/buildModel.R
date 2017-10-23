# This file contains functions for building the model

# Name: buildModel
# Parameters: 
#   * trainigData(type: data.frame): the set of training samples to train the model on
# Result: the random forest regressor for each of the 3 metrcis
# Description: this function creates the random forest regressor model
buildModel <- function(trainingData)
{
  # Make sure we do not run into errors when building the model, because the model cannot be build if there 
  # are NA values in the data set
  trainingData[is.na(trainingData)] <- 0
  
  predictors <- featureSelection(trainingData)
  
  randomForest1 <- randomForest(x = trainingData[, c(1:(length(trainingData) - 3))],
                                y = trainingData$Interest,
                                data = predictors[1],
                                ntree = 500,
                                mtry = floor((length(trainingData) - 3) / 3),
                                nodesize = 5)
  
  randomForest2 <- randomForest(x = trainingData[, c(1:(length(trainingData) - 3))],
                                y = trainingData$Novelty_Complexity,
                                data = predictors[2],
                                ntree = 500,
                                mtry = floor((length(trainingData) - 3) / 3),
                                nodesize = 5)
  
  randomForest3 <- randomForest(x = trainingData[, c(1:(length(trainingData) - 3))],
                                y = trainingData$Comprehensibility,
                                data = predictors[3],
                                ntree = 500,
                                mtry = floor((length(trainingData) - 3) / 3),
                                nodesize = 5)
  
  list(Forest1 = randomForest1, Forest2 = randomForest2, Forest3 = randomForest3)
}

# Name: featureSelection
# Parameters: 
#   * trainigData(type: data.frame): the set of training samples to train the model on
# Result: the set of relevant features for predicting the scores for the interest, the novelty-complexity 
#         and the comprehensibility of a text separately
# Description: this function performs recursive feature elimination to determine which features are relevant 
#              in buidling the model for predicting the scores for the interest, the novelty-complexity and the 
#              comprehensibility of a text separately
featureSelection <- function(trainingData)
{
  # The control settings for the recursive feature elimination algorithm
  rfeControl <- rfeControl(functions = rfFuncs, method = "boot", number = 25)
  # The results of the recursive feature elimination algorithm for the text interest
  rfeResults1 <- rfe(x = trainingData[, c(1:(length(trainingData) - 3))], 
                     y = trainingData$Interest,
                     rfeControl = rfeControl)
  # The results of the recursive feature elimination algorithm for the novelty-complexity of the text
  rfeResults2 <- rfe(x = trainingData[, c(1:(length(trainingData) - 3))], 
                     y = trainingData$Novelty_Complexity,
                     rfeControl = rfeControl)
  # The results of the recursive feature elimination algorithm for the text comprehensibility
  rfeResults3 <- rfe(x = trainingData[, c(1:(length(trainingData) - 3))], 
                     y = trainingData$Comprehensibility,
                     rfeControl = rfeControl)
  
  # Return the list of all the predictors for each metric
  list(Predictors1 = predictors(rfeResults1), 
       Predictors2 = predictors(rfeResults2), 
       Predictors3 = predictors(rfeResults3))
}

# Name: predictScores
# Parameters: 
#   * samples(type: data.frame): the set of samples for which to predict the scores for the interest, 
#                                novelty-complexity and comprehensibility of the text
#   * forest(type: list): the list of random forest regressors, one random forest regressor for each of the scores 
#                         that we want to predict
# Result: a list of vectors of the predicted scores for each sample
# Description: this function computes the predictions of the scores for all of the samples
predictScores <- function(samples, forest)
{
  # The list to store the predictions for each sample
  predictions <- list()
  
  # Loop over each sample and predict the scores
  for (index in c(1:nrow(samples))) 
  {
    predictions[[index]] <- predictScoresSingle(samples[index,], forest)
  }
  
  predictions
}

# Name: predictScoresSingle
# Parameters: 
#   * sample(type: data.frame): a single sample for which to predict the scores for the interest, novelty-complexity 
#                               and comprehensibility of the text
#   * forest(type: list): the list of random forest regressors, one random forest regressor for each of the scores 
#                         that we want to predict
# Result: a vector of the predicted scores for the given sample
# Description: this function computes the predictions of the scores for the sample
predictScoresSingle <- function(sample, forest)
{
  # Predict the scores
  scores <- c(predict(forest$Forest1, sample), predict(forest$Forest2, sample), predict(forest$Forest3, sample))
  # Label each score with the name of the measure to which it corresponds
  names(scores) <- c("Interest", "Novelty_Complexity", "Comprehensibility")
  scores
}