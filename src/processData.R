# This file contains functionality for processing the data

# Name: processData
# Parameters:
#   * experimentFile(type: character): the path to the file with the experiment data for the corresponding test subject
#   * participantRatings(type: data.frame): the participant ratings for the test subject for which we need to 
#                                           process the data
# Result: A vector of training samples, one training sample for each text that has been read by the test subject
# Description: this function processes all the experiment data of the test subject
processData <- function(experimentFile, participantRatings)
{
  # Read in the eye tracking data, the irrelevant columns are not obtained
  eyeTrackingData <- fread(input = experimentFile, sep = "\t", header = TRUE, fill = TRUE, 
                           colClasses = c("double", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                                          "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL",
                                          "NULL", "NULL", "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", 
                                          "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                                          "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                                          "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", 
                                          "character", "NULL", "NULL", "integer", "NULL"))
  
  # Remove all samples for which no stimulus_id has been observed, i.e. all samples that do not belong to one of the 
  # 18 texts that the test subject has read
  eyeTrackingData <- subset(eyeTrackingData, eyeTrackingData$stimulus_id %in% participantRatings$text.ID)
  
  # Process each text separately
  for(textID in unique(participantRatings$text.ID))
  {
    # Subset the eye tracking data for the current text
    textData <- subset(eyeTrackingData, eyeTrackingData$stimulus_id == textID)
    textData <- filterData(textData)
    eventsData <- transformData(textData)
    visualizeData(textData, eventsData, unique(participantRatings$person.ID), textID)
  }
}

# Name: visualizeData
# Parameters:
#   * rawData(type: data.frame): the raw data of the eye movements
#   * eventsData(type: data.frame): the data on the events of the eye movements
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates a heat map of the eye movements and it writes the heat map to a file
visualizeData <- function(rawData, eventsData, participant, text)
{
  visualizeHeatmap(rawData, participant, text)
  visualizeScanpath(eventsData$Fixations, participant, text)
  visualizeFrequencies(rawData, participant, text)
}

# Name: visualizeHeatmap
# Parameters:
#   * rawData(type: data.frame): the raw data of the eye movements
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates a heat map of the eye movements and it writes the heat map to a file
visualizeHeatmap <- function(rawData, participant, text)
{
  # Set the name of the file to save the scanpath visualization in
  fileName <- paste(dirname(getwd()), "/visualizations/heatmaps/heatmap_number(", participant, 
                    ")_id(", text, ").png", sep = "")
  # Create the directory if it does not exist yet
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  
  ###################################################################################################################
  #####TODO#####
  ###################################################################################################################
}

# Name: visualizeScanpath
# Parameters:
#   * fixationsData(type: data.frame): the fixation events
#   * participant(type: integer): the id number of the participant 
#   * text(type: integer): the id number of the text
# Result: this function does not return a result
# Description: this function creates a scanpath visualization of the fixations and writes this visualization 
#              to a file
visualizeScanpath <- function(fixationsData, participant, text)
{
  # Set the name of the file to save the scanpath visualization in
  fileName <- paste(dirname(getwd()), "/visualizations/scanpaths/scanpath_number(", participant, 
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
visualizeFrequencies <- function(rawData, participant, text)
{
  # The name of the directory for saving the visualizations of the frequencies of the raw data
  directoryName <- paste(dirname(getwd()), "/visualizations/frequencies", sep = "")
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
  
  ###################################################################################################################
  # Create the directory for barplots of the L event info of the raw eye movement data
  dir.create(paste(directoryName, "/LEvent_barplots", sep = ""), showWarnings = FALSE, recursive = TRUE)
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = paste(directoryName, "/LEvent_barplots/barplot_LEvent_number(", participant, ")_id(", text, 
                       ").png", sep = ""))
  # Create an empty plot
  plot.new()
  # Create the barplot of the left eye event
  barplot(table(rawData$`L Event Info`), xlab = "Left eye event", main = "Histogram of the left eye event")
  # End the plotting
  dev.off()
  
  ###################################################################################################################
  # Create the directory for barplots of the R event info of the raw eye movement data
  dir.create(paste(directoryName, "/REvent_barplots", sep = ""), showWarnings = FALSE, recursive = TRUE)
  # Set the file as output for the plot, i.e. do not create a plot window but save the plot directly to a file
  png(filename = paste(directoryName, "/REvent_barplots/barplot_REvent_number(", participant, ")_id(", text, 
                       ").png", sep = ""))
  # Create an empty plot
  plot.new()
  # Create the barplot of the right eye event
  barplot(table(rawData$`R Event Info`), xlab = "Right eye event", main = "Histogram of the right eye event")
  # End the plotting
  dev.off()
}

# Name: transformData
# Parameters:
#   * rawData(type: data.frame): the raw data samples of the eye movements
# Result: A list containing a data.frame of fixation events, a data.frame of saccade events and a data.frame of 
#         blink events
# Description: this function computes the fixation events, the saccade events and the blink events from the 
#              raw eye movement data 
transformData <- function(rawData)
{
  # The fixation events
  fixations <- fixationsData(rawData, 25, 6)
  
  list(Fixations = fixations, Saccades = saccadeData(fixations), Blinks = blinkData(rawData))
}

# Name: fixationsData
# Parameters:
#   * rawData(type: data.frame): the raw data samples of the eye movements
#   * dispersion(type: integer): the maximal dispersion allowed in pixels for the fixation detection
#   * minSamples(type: integer): the minimal number of samples for a fixation event
# Result: A data.frame of fixation events
# Description: this function computes the fixation events from the raw eye movement data 
fixationsData <- function(rawData, dispersion, minSamples)
{
  # Compute all fixation events
  emov.idt(rawData$Time, rawData$`L POR X [px]`, rawData$`L POR Y [px]`, dispersion, minSamples)
}

# Name: saccadeData
# Parameters:
#   * fixationsData(type: data.frame): the fixation events
# Result: A data.frame of saccade events 
# Description: this function computes the saccade events from the fixation events 
saccadeData <- function(fixationsData)
{
  # Stores all saccade events
  saccadeData <- data.frame(Start = integer(),
                            End = integer(),
                            Duration = integer(),
                            Start_X = double(),
                            Start_Y = double(),
                            End_X = double(),
                            End_Y = double(),
                            Amplitude = double(),
                            Velocity = double())
  
  # Create a saccade event fOr every pair of consecutive fixation events
  for(index in c(1:(nrow(fixationsData) - 1)))
  {
    # Add a saccade event to the list of all saccade events
    saccadeData[index,] <- (c(fixationsData[index,]$end + 1, fixationsData[index + 1,]$start - 1, 
                              (fixationsData[index + 1,]$start - 1) - (fixationsData[index,]$end + 1), 
                              fixationsData[index,]$x, fixationsData[index,]$y, fixationsData[index + 1,]$x, 
                              fixationsData[index + 1,]$y, 
                              dist(rbind(c(fixationsData[index,]$x, fixationsData[index,]$y), 
                                         c(fixationsData[index + 1,]$x, fixationsData[index + 1,]$y)), 
                                   method = "euclidean"), 
                              (dist(rbind(c(fixationsData[index,]$x, fixationsData[index,]$y), 
                                          c(fixationsData[index + 1,]$x, fixationsData[index + 1,]$y)), 
                                    method = "euclidean")) / 
                                ((fixationsData[index + 1,]$start - 1) - (fixationsData[index,]$end + 1))))
  }
  
  saccadeData
}

# Name: blinkData
# Parameters:
#   * rawData(type: data.frame): the raw data samples of the eye movements
# Result: A data.frame of blink events
# Description: this function computes the blink events from the raw eye movement data  
blinkData <- function(rawData)
{
  # Stores all blink events
  blinkData <- data.frame(Start = integer(),
                          End = integer(),
                          Duration = integer())
  
  # The index for adding new blinks to the event list
  blinkNumber <- 1
  
  # The first sample in a sequence of consecutive blink samples
  blinkStartSample <- NA
  
  # Loop over all samples to determine sequences of blink events
  for(index in c(1:(nrow(rawData))))
  {
    # We should have registrated a blink event for at least one of the eyes
    if(rawData[index]$`L Event Info` == "Blink" | rawData[index]$`R Event Info` == "Blink")
    {
      # Check whether or not this is the first sample in the sequence of blinks currently under investigation
      if(is.na(blinkStartSample)) 
      {
        # Register that this is the first sample in the sequence of blinks currently under investigation
        blinkStartSample <- rawData[index]
      }
    }
    
    # Neither have we registrated a blink event for the left eye nor have we registrated a blink event for the 
    # right eye
    else
    {
      # Check whether or not a sequence of blink events was currently under construction
      if(!is.na(blinkStartSample)) 
      {
        # Add the blink event to the list of blink events
        blinkData[blinkNumber,] <- c(blinkStartSample$Time, rawData[index]$Time - 1, 
                                     (rawData[index]$Time - 1) - blinkStartSample$Time)
        
        # Update the index for adding new blinks to the event list
        blinkNumber <- blinkNumber + 1
        
        # Resetthe first sample in the sequence of consecutive blink samples
        blinkStartSample <- NA
      }
    }
  }
  
  blinkData
}
  
# Name: filterData
# Parameters:
#   * textData(type: data.frame): the eye tracking data for a single text and for a single user
# Result: A data.frame of filtered eye tracking data
# Description: this function filters the data of a single text that was read by a single user
filterData <- function(textData)
{
  # Filter out all samples where data is missing. It does not look like this is ever the case, but to be sure we 
  # filter out these samples anyway
  filteredData <- subset(textData, !is.na(textData$Time) & !is.na(textData$`L POR X [px]`) & 
                           !is.na(textData$`L POR Y [px]`) & !is.na(textData$`R POR X [px]`) & 
                           !is.na(textData$`R POR Y [px]`)) 
  
  # Filter out all data where at least one of the positions of the eyes has a negative value
  filteredData <- subset(filteredData, filteredData$`L POR X [px]` >= 0 & filteredData$`L POR Y [px]` >= 0 & 
                           filteredData$`R POR X [px]` >= 0 & filteredData$`R POR Y [px]` >= 0)
  
  # Filter out all data samples where the position of the left eye is different from the position of the right eye. 
  # It does not look like this is ever the case, but to be sure we filter out these samples anyway
  filteredData <- subset(filteredData, filteredData$`L POR X [px]` == filteredData$`R POR X [px]` & 
                                       filteredData$`L POR Y [px]` == filteredData$`R POR Y [px]`)
  
  filteredData
}