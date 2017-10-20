# This file is used for calculating the required statistics

# Name: rSquared
# Parameters: 
#   * observedScores(type: data.frame): the observed scores for the interest, novelty-complexity and 
#                                       comprehensibility of a text
#   * predictedScores(type: list): the predicted scores for the interest, novelty-complexity and 
#                                  comprehensibility of a text
# Result: the R Squared value for the given observed and predicted scores
# Description: this function computes the R Squared value
rSquared <- function(observedScores, predictedScores)
{
  # Check that the number of actual classifications matches the number of predicted classifications
  if (nrow(observedScores) != length(predictedScores)) 
  {
    return(NA)
  }
  
  # Convert the predicted scores to a data.frame, so that observedScores and predictedScores are of the same type
  predictedScores <- convertPredictedScores(predictedScores)
  
  # Compute the mean vector of the observed scores
  meanObservedScores <- c(mean(observedScores[,1]), mean(observedScores[,2]), mean(observedScores[,3])) 
  
  # Store the total sum of squares
  totalSumOfSquares <- 0
  # Store the residual sum of squares
  residualSumOfSquares <- 0
  
  # Loop over all actual classifications and all predicted classifications at once
  for (index in c(1:nrow(observedScores))) 
  {
    # Update the total sumof squares
    totalSumOfSquares <- totalSumOfSquares + (dist(rbind(observedScores[index,], meanObservedScores), 
                                                   method = "euclidean")^2)
    # Update the residual sum of squares
    residualSumOfSquares <- residualSumOfSquares + (dist(rbind(observedScores[index,], predictedScores[index,]), 
                                                         method = "euclidean")^2)
  }
  
  # Compute and return the R Squared value
  as.numeric(1 - (residualSumOfSquares/totalSumOfSquares))
}

# Name: standardErrorOfRegression
# Parameters: 
#   * observedScores(type: data.frame): the observed scores for the interest, novelty-complexity and 
#                                       comprehensibility of a text
#   * predictedScores(type: list): the predicted scores for the interest, novelty-complexity and 
#                                  comprehensibility of a text
# Result: the standard error of the regression for the given observed and predicted scores
# Description: this function computes the standard error of the regression
standardErrorOfRegression <- function(observedScores, predictedScores)
{
  # Check that the number of actual classifications matches the number of predicted classifications
  if (nrow(observedScores) != length(predictedScores)) 
  {
    return(NA)
  }
  
  # Convert the predicted scores to a data.frame, so that observedScores and predictedScores are of the same type
  predictedScores <- convertPredictedScores(predictedScores)

  # Store the residual sum of squares
  residualSumOfSquares <- 0
  
  # Loop over all actual classifications and all predicted classifications at once
  for (index in c(1:nrow(observedScores))) 
  {
    # Update the residual sum of squares
    residualSumOfSquares <- residualSumOfSquares + (dist(rbind(observedScores[index,], predictedScores[index,]), 
                                                         method = "euclidean")^2)
  }
  
  # Compute and return the R Squared value
  as.numeric(sqrt(residualSumOfSquares/nrow(observedScores)))
}

# Name: convertPredictedScores
# Parameters: 
#   * predictedScores(type: list): the predicted scores for the interest, novelty-complexity and 
#                                  comprehensibility of a text
# Result: a data.frame of predicted scores
# Description: this function converts the list of predicted scores to a data.frame of predicted scores
convertPredictedScores <- function(predictedScores)
{
  # Create an empty data.frame for the predicted scores
  predictions <- data.frame(Interest = double(),
                            Novelty_Complexity = double(),
                            Comprehensibility = double())
  
  # Create a row in the data frame for each vector of observed scores
  for(index in c(1:length(predictedScores))) 
  {
    predictions[index,] <- c(predictedScores[[index]][1], predictedScores[[index]][2], predictedScores[[index]][3])
  }
  
  predictions
}