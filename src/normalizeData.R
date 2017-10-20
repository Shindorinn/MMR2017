# This file contains functions for normalizing the training data and for normalizing the test data

# Name: normalize
# Parameters: 
#   * value(type: numeric): the value that needs to be normalized
#   * mean(type: double): the mean with which to normalize the given value
#   * sd(type: double): the standard deviation with which to normalize the given value
# Result: the normalized representation of the given value
# Description: this function normalizes the given value
normalize <- function(value, mean, sd)
{
  (value - mean) / sd
}

# Name: normalizeTrainingData
# Parameters: 
#   * trainigData(type: data.frame): the set of training samples to train the model on
# Result: the normalized training data set, a vector of means of the non-normalized training data and a vector 
#         of standard deviations of the non-normalized training data
# Description: this function normalizes the training data and computes vectors of the mean and standard deviation 
#              of all the features in the non-normalized training data set
normalizeTrainingData <- function(trainingData)
{
  # Compute a vector of the mean of all the features in the non-normalized training data set
  meanVector <-sapply(trainingData[,c(1:(length(trainingData) - 3))], mean)
  # Compute a vector of the standard deviation of all the features in the non-normalized training data set
  sdVector <- sapply(trainingData[, c(1:(length(trainingData) - 3))], sd)
  
  # Normalize each sample in the training data set
  for (index in c(1:nrow(trainingData))) 
  {
    trainingData[index, c(1:(length(trainingData) - 3))] <- normalizeDataSample(trainingData[index, c(1:(length(trainingData) - 3))], meanVector, sdVector)
  }
  
  list(TrainingData = trainingData, MeanVector = meanVector, SDVector = sdVector)
}

# Name: normalizeDataSample
# Parameters: 
#   * sample(type: data.frame): a single data sample that needs to be normalized
#   * meanVector(type: vector): a vector of the means with which to normalize the features in the sample
#   * sdVector(type: vector): a vector of the standard deviations with which to normalize the features in the sample
# Result: a normalized sample
# Description: this function normalizes the given data sample
normalizeDataSample <- function(sample, meanVector, sdVector)
{
  # The sample must containing the same number of values as both the meanVector and the sdVector
  if (!(ncol(sample) == length(meanVector)) & (length(meanVector) == length(sdVector)))
  {
    return(NA)
  }
  
  # Normalize all the features in the sample
  for(index in c(1:ncol(sample))) 
  {
    sample[1, index] <- normalize(sample[1, index], meanVector[index], sdVector[index])
  }
  
  sample
}