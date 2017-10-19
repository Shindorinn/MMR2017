# This file contains functionality for reading in all necessary data

# Name: readData
# Parameters: has no parameters
# Result: has no return value
# Description: this function will read in all data and call the appropriate functions for working with this data
readData <- function()
{
  # The path to the data directory, i.e. to the directory where all data on the experiments and on the participant 
  # ratings can be found
  dataDirectoryPath <- paste(dirname(getwd()), "/data", sep = "")
  
  # List all file names of the experiment data files, i.e. all file names of the data on the eye movements for 
  # each test subject
  experimentFiles <- list.files(path = paste(dataDirectoryPath, "/eye-tracking_data", sep = ""), pattern = "*.txt")
  
  # Create a data frame for the training data that will be used for training the model
  trainingData <- data.frame(Fixations = integer(),
                             Fixations_Progressive_Fraction = double(),
                             Fixations_Regressive_Fraction = double(),
                             Fixation_Progressive_Duration_Mean = double(),
                             Fixation_Regressive_Duration_Mean = double(),
                             Fixation_Progressive_Duration_SD = double(),
                             Fixation_Regressive_Duration_SD = double(),
                             Saccades = integer(),
                             Saccade_Duration_Mean = double(),
                             Saccade_Amplitude_Mean = double(),
                             Saccade_Velocity_Mean = double(),
                             Saccade_Duration_SD = double(),
                             Saccade_Amplitude_SD = double(),
                             Saccade_Velocity_SD = double(),
                             Blinks = integer(),
                             Blink_Duration_Mean = double(),
                             Blink_Duration_SD = double(),
                             Interest = double(),
                             Novelty_Complexity = double(),
                             Comprehensibility = double())
  
  # Index for adding new training samples to the set of training data for the model at the correct position 
  trainingDataIndex <- 1
  
  # Read the data files with participant ratings
  participantRatings <- read.xlsx(paste(dataDirectoryPath, "/participant_ratings.xlsx", sep = ""), sheetIndex = 1, 
                                  colIndex = c(1:6))
  
  # Loop over all participants to process their eye tracking data one at a time
  for(index in c(1:length(unique(participantRatings$person.ID))))
  {
    # Retrieve the ID of the current participant, i.e. of the current test subject
    participant <- unique(participantRatings$person.ID)[index]
    
    # Process the eye tracking data of the current test subject
    trainingSamples <-processData(experimentFile = paste(dataDirectoryPath, "/eye-tracking_data/", 
                                                         experimentFiles[index], sep = ""), 
                                  participantRatings = subset(participantRatings, 
                                                              participantRatings$person.ID == participant))
    
    for(sampleIndex in c(1:length(trainingSamples))) 
    {
      trainingData[trainingDataIndex,] <- trainingSamples[[sampleIndex]]
      trainingDataIndex <- trainingDataIndex + 1
    }
  }
  
  trainingData
}