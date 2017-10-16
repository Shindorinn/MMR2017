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
    filterData(textData)
  }
}

# Name: filterData
# Parameters:
#   * textData(type: data.frame): the eye tracking data for a single text and for a single user
# Result: A data.frame of filtered eye tracking data
# Description: this function filters the data of a single text that was read by a single user
filterData <- function(textData)
{
  # Filter out all data where at least one of the positions of the eyes has a negative value
  filteredData <- subset(textData, textData$`L POR X [px]` >= 0 & textData$`L POR Y [px]` >= 0 & 
                         textData$`R POR X [px]` >= 0 & textData$`R POR Y [px]` >= 0)
}