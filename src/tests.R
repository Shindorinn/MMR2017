# This file contains the functions for testing the model and for assessing the workload of the different components 
# of the processing pipeline (for computing the empirical computational complexity)

# Name: runTests
# Parameters: has no parameters
# Result: has no return value
# Description: this function will perform the tests, i.e. model the entire processing pipeline and output
#              the R Squared value and the standard error of the regression
runTests <- function()
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
  
  # Read in the data
  data <- readData()
  # Normalize the data
  normalizedData <- normalizeTrainingData(data)
  # Build the model
  model <- buildModel(normalizedData$TrainingData)
  # Predict the scores for interest, novelty-complexity and comprehensibility for each sample in the training data 
  # from the model
  predictions <- predictScores(normalizedData$TrainingData, model)
  # Compute the results of the test and store them in a data.frame
  results <- data.frame(Time = as.character(Sys.time()),
                        R_Squared = rSquared(normalizedData$TrainingData[,c((ncol(normalizedData$TrainingData) - 2):
                                                                              ncol(normalizedData$TrainingData))], 
                                             predictions),
                        Standard_Error_Of_Regression = standardErrorOfRegression(normalizedData$TrainingData
                                                                                 [,c((ncol(normalizedData$TrainingData) 
                                                                                      - 2):
                                                                                       ncol(normalizedData$
                                                                                              TrainingData))], 
                                                                                 predictions))
  

  # Write the results to a file
  write.csv(x = results, file = resultsFileName, row.names = FALSE)
  # Write the mean vector for normalization to a file
  write(x = normalizedData$MeanVector, file = meanVectorFileName)
  # Write the standard deviation vector for normalization to a file
  write(x = normalizedData$SDVector, file = sdVectorFileName)
}

# Name: runLoadTests
# Parameters: 
#   * workLoads(type: integer): the number of different workloads to test
# Result: has no result
# Description: this function will compute the time it takes to run different parts of the system for different 
#              workloads. These results are then written to a file
runLoadTests <- function(workLoads = 3)
{
  # Create a data.frame to store the timing information for the different workloads
  timeData <- data.frame(Load = double(),
                         ReadTime = double(), 
                         FilterTime = double(), 
                         EventDetectionTime = double(), 
                         VisualizationTime = double(), 
                         MetricsComputationTime = double(), 
                         ModelBuildingTime = double())
  
  # Perform the computations for each of the specified workloads
  for(iteration in c(1:workLoads)) 
  {
    timeData[iteration,] <- c((iteration / workLoads), loadTest(workLoad = (iteration / workLoads)))
  }
  
  # The name of the directory for saving the results
  directoryName <- paste(dirname(getwd()), "/results", sep = "")
  # Create the directory if it does not exist yet
  dir.create(directoryName, showWarnings = FALSE, recursive = TRUE)
  # The name of the file to write the result to
  resultsFileName <- paste(directoryName, "/workload_result.txt", sep = "")
  
  # Write the results to a file
  write.csv(x = timeData, file = resultsFileName, row.names = FALSE)
}

# Name: loadTest
# Parameters: 
#   * workLoad(type: double): the relative size of the data for a single text that we want to keep
#   * times(type: integer): The number of iterations for testing the empirical computational complexity of the system
# Result: a list containing the time it takes to read in the data, to filter the data, to detect the events from the 
#         data, to visualize the data, to compute the aggregate metrics and to build the model
# Description: this function will compute the time it takes to run different parts of the system
loadTest <- function(workLoad, times = 1)
{
  # Create a data.frame to store the timing information
  timeData <- data.frame(ReadTime = double(), 
                         FilterTime = double(), 
                         EventDetectionTime = double(), 
                         VisualizationTime = double(), 
                         MetricsComputationTime = double(), 
                         ModelBuildingTime = double())
  
  # Perform the required amount of retries for computing the timing information
  for (iteration in c(1:times)) 
  {
    startTime <- Sys.time()
    # Read in the data
    data <- readDataLoadTest(workLoad)
    readDataTime <- Sys.time() - startTime
    
    startTime <- Sys.time()
    # Filter the data
    eyeTrackingData <-filterData(data$EyeTrackingData)
    filterTime <- Sys.time() - startTime
    
    startTime <- Sys.time()
    # Compute the events
    eventsData <- transformData(eyeTrackingData, 100)
    eventComputationTime <- Sys.time() - startTime
    
    # Filter out all negative eye positions
    eyeTrackingData <- subset(eyeTrackingData, eyeTrackingData$`L POR X [px]` >= 0 & 
                                eyeTrackingData$`L POR Y [px]` >= 0 &
                                eyeTrackingData$`R POR X [px]` >= 0 & 
                                eyeTrackingData$`R POR Y [px]` >= 0)
    
    startTime <- Sys.time()
    # Visualize the data
    visualizeDataLoadTest(eyeTrackingData, eventsData, 1, 6474)
    visualizationTime <- Sys.time() - startTime
    
    startTime <- Sys.time()
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
    # Compute the training sample for building the model
    trainingData[1,] <- c(computeMetrics(eventsData, 100), 
                          as.vector(list(Interest = data$ParticipantRatings$interest,
                                         Novelty_Complexity = data$ParticipantRatings$complexity,
                                         Comprehensibility = data$ParticipantRatings$comprehension)))
    metricsComputationTime <- Sys.time() - startTime
    
    # Expand the training data set, otherwise we cannot build th model with just a single sample
    for (index in c(2:10)) 
    {
      trainingData[index,] <- trainingData[1,]
    }
    
    startTime <- Sys.time()
    # Build the model. No normalization of the data, because that does not work with only a single training sample
    model <- buildModel(trainingData)
    modelBuildingTime <- Sys.time() - startTime
    
    # Add the timing information of the current iteration to the timeData data.frame
    timeData[iteration,] <- c(readDataTime, filterTime, eventComputationTime, visualizationTime, 
                              metricsComputationTime, modelBuildingTime)
  }
  
  list(ReadTime = mean(timeData[,1]), FilterTime = mean(timeData[,2]), EventDetectionTime = mean(timeData[,3]), 
       VisualizationTime = mean(timeData[,4]), MetricsComputationTime = mean(timeData[,5]), 
       ModelBuildingTime = mean(timeData[,6]))
}

#####################################################################################################################
##### The following functions are copies of some of the other functions which are slightly altered for the purpose 
##### of testing the empirical computational complexity of the system. 
#####################################################################################################################

# Name: readDataLoadTest
# Parameters: 
#   * workLoad(type: double): the relative size of the data for a single text that we want to keep
# Result: has no return value
# Description: this function will read in the data 
readDataLoadTest <- function(workLoad)
{
  # The path to the data directory, i.e. to the directory where all data on the experiments and on the participant 
  # ratings can be found
  dataDirectoryPath <- paste(dirname(getwd()), "/data", sep = "")
  
  # List all file names of the experiment data files, i.e. all file names of the data on the eye movements for 
  # each test subject
  experimentFiles <- list.files(path = paste(dataDirectoryPath, "/eye-tracking_data", sep = ""), pattern = "*.txt")

  # Read the data files with participant ratings
  participantRatings <- read.xlsx(paste(dataDirectoryPath, "/participant_ratings.xlsx", sep = ""), sheetIndex = 1, 
                                  colIndex = c(1:6))
  
  # Obtain the rating of a single text for a single paricipant
  participantRatings <- subset(participantRatings, participantRatings$person.ID == 1 & 
                                 participantRatings$text.ID == 6474)
  
  # Read in the eye tracking data, the irrelevant columns are not obtained
  eyeTrackingData <- fread(input = paste(dataDirectoryPath, "/eye-tracking_data/", experimentFiles[1], sep = ""), 
                           sep = "\t", header = TRUE, fill = TRUE, colClasses = c("double", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", NA, NA, NA, NA, "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL",
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "NULL", "NULL", "NULL", "NULL", 
                                                                                  "integer", "NULL"))
  
  # Remove all samples for which no stimulus_id has been observed, i.e. all samples that do not belong to one of the 
  # 18 texts that the test subject has read
  eyeTrackingData <- subset(eyeTrackingData, eyeTrackingData$stimulus_id %in% participantRatings$text.ID)
  
  # Subset the eye tracking data for the required text
  eyeTrackingData <- subset(eyeTrackingData, eyeTrackingData$stimulus_id == 6474)
  
  # Extract a part of the data set for the purpose of comparing different sizes of the data set
  eyeTrackingData <- eyeTrackingData[c(1:floor(nrow(eyeTrackingData) * workLoad)),]
  
  list(EyeTrackingData = eyeTrackingData, ParticipantRatings = participantRatings)
}

# Name: visualizeDataLoadTest
# Parameters:
#   * rawData(type: data.frame): the raw data of the eye movements
#   * eventsData(type: data.frame): the data on the events of the eye movements
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function writes the visualizations of the eye tracking data to a file
visualizeDataLoadTest <- function(rawData, eventsData, participant, text)
{
  visualizeHeatmapLoadTest(rawData, participant, text)
  visualizeScanpathLoadTest(eventsData$Fixations, participant, text)
  visualizeFrequenciesLoadTest(rawData, participant, text)
}

# Name: visualizeHeatmapLoadTest
# Parameters:
#   * rawData(type: data.frame): the raw data of the eye movements
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates a heat map of the eye movements and it writes the heat map to a file
visualizeHeatmapLoadTest <- function(rawData, participant, text)
{
  # Set the name of the file to save the scanpath visualization in
  fileName <- paste(dirname(getwd()), "/visualizations/loadTests/heatmaps/heatmap_number(", participant, 
                    ")_id(", text, ").png", sep = "")
  # Create the directory if it does not exist yet
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = fileName, width = 1280, height = 1024)
  # Create an empty plot
  plot.new()
  # Create a new plot window
  plot.window(xlim = c(0, 1280), ylim = c(0, 1024))
  # Create an empty plot with the correct horizontal-axis and correct vertical-axis
  plot(NULL, NULL, type = "n", axes = TRUE, ann = FALSE, xlim = c(0,1280), ylim = c(1024,0))
  
  mba.int <- mba.surf(fixationData[,c(4,5,3)], 300, 300, n = 1, m = 1, h = 8, extend = TRUE, sp = FALSE, 0, 1280, 0, 1024)$xyz.est
  image.plot(mba.int)
  
  # End the plotting
  dev.off()
}

# Name: visualizeScanpathLoadTest
# Parameters:
#   * fixationsData(type: data.frame): the fixation events
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates a scanpath visualization of the fixations and writes this visualization 
#              to a file
visualizeScanpathLoadTest <- function(fixationsData, participant, text)
{
  # Set the name of the file to save the scanpath visualization in
  fileName <- paste(dirname(getwd()), "/visualizations/loadTests/scanpaths/scanpath_number(", participant, 
                    ")_id(", text, ").png", sep = "")
  # Create the directory if it does not exist yet
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = fileName, width = 1280, height = 1024)
  # Create an empty plot
  plot.new()
  # Create a new plot window
  plot.window(xlim = c(0, 1280), ylim = c(0, 1024))
  # Create an empty plot with the correct horizontal-axis and correct vertical-axis
  plot(NULL, NULL, type = "n", axes = TRUE, ann = FALSE, xlim = c(0,1280), ylim = c(1024,0))
  # Plot the fixation points
  points(fixationsData$x, fixationsData$y, col = "red", cex = 1.5)
  # Plot lines, which represent saccades, between the fixation points
  lines(fixationsData$x, fixationsData$y)
  # End the plotting
  dev.off()
}

# Name: visualizeFrequencies
# Parameters:
#   * rawData(type: data.frame): the raw data of the eye movements
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates visualizations of the frequencies of the metrics of the raw eye movements. 
#              These visualizations are writen to files
visualizeFrequenciesLoadTest <- function(rawData, participant, text)
{
  # The name of the directory for saving the visualizations of the frequencies of the raw data
  directoryName <- paste(dirname(getwd()), "/visualizations/loadTests/frequencies", sep = "")
  # Create the directory if it does not exist yet
  dir.create(directoryName, showWarnings = FALSE, recursive = TRUE)
  
  ###################################################################################################################
  # Create the directory for histograms of the x-coordinate of the eyes if it does not exist yet
  dir.create(paste(directoryName, "/X_histograms", sep = ""), showWarnings = FALSE, recursive = TRUE)
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = paste(directoryName, "/X_histograms/histogram_X_number(", participant, ")_id(", text, ").png", 
                       sep = ""))
  # Create an empty plot
  plot.new()
  # Create the histogram of the x-coordinate of the eyes. Because the x-coordinate of the left eye is equal to 
  # the x-coordinate of the right eye, we only need to create a histogram for one of the eyes
  hist(rawData$`L POR X [px]`, xlab = "X-coordinate of the eyes", main = "Histogram of the x-coordinate of the eyes")
  # End the plotting
  dev.off()
  
  ###################################################################################################################
  # Create the directory for histograms of the y-coordinate of the eyes if it does not exist yet
  dir.create(paste(directoryName, "/Y_histograms", sep = ""), showWarnings = FALSE, recursive = TRUE)
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = paste(directoryName, "/Y_histograms/histogram_Y_number(", participant, ")_id(", text, ").png", 
                       sep = ""))
  # Create an empty plot
  plot.new()
  # Create the histogram of the y-coordinate of the eyes. Because the y-coordinate of the left eye is equal to 
  # the x-coordinate of the right eye, we only need to create a histogram for one of the eyes
  hist(rawData$`L POR Y [px]`, xlab = "Y-coordinate of the eyes", main = "Histogram of the y-coordinate of the eyes")
  # End the plotting
  dev.off()
}
