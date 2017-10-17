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
    transformData(textData)
  }
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