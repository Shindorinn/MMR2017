runSplitTests <- function()
{
  results <- runTests1()
  runTests2(results$NormalizedData$TrainingData, results$NormalizedData$MeanVector, 
            results$NormalizedData$SDVector, results$Model)
}

runTests1 <- function()
{
  # Read in the data
  trainingData <- readData()
  # Normalize the data
  normalizedData <- normalizeTrainingData(trainingData)
  # Remove NA values
  normalizedData[is.na(normalizedData)] <- 0
  # Build the model
  model <- buildModel(normalizedData$TrainingData)
  
  list(Model = model, NormalizedData = normalizedData)
}

runTests2 <- function(data, meanVector, sdVector, model)
{
  # The name of the directory for saving the test results
  directoryName <- paste(dirname(getwd()), "/results", sep = "")
  # Create the directory if it does not exist yet
  dir.create(directoryName, showWarnings = FALSE, recursive = TRUE)
  # The name of the file to write the result to
  resultsFileName <- paste(directoryName, "/standard_result.txt", sep = "")
  # The name of the file to write the mean vector for normalization to
  meanVectorFileName <- paste(directoryName, "/mean_vector.txt", sep = "")
  # The name of the file to write the standard deviation vector for normalization to
  sdVectorFileName <- paste(directoryName, "/sd_vector.txt", sep = "")
  

  # Predict the scores for interest, novelty-complexity and comprehensibility for each sample in the training data 
  # from the model
  predictions <- predictScores(data, model)
  # Compute the results of the test and store them in a data.frame
  results <- data.frame(Time = as.character(Sys.time()),
                        R_Squared = rSquared(data[,c((ncol(data) - 2):ncol(data))], predictions),
                        Standard_Error_Of_Regression = standardErrorOfRegression(data
                                                                                 [,c((ncol(data) - 2):ncol(data))], 
                                                                                 predictions))
  
  
  # Write the results to a file
  write.csv(x = results, file = resultsFileName, row.names = FALSE)
  # Write the mean vector for normalization to a file
  write(x = meanVector, file = meanVectorFileName)
  # Write the standard deviation vector for normalization to a file
  write(x = sdVector, file = sdVectorFileName)
}