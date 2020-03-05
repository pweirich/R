##
## Title: Check Accuracy of Free Classification Coding
##
## Author: Phillip Weirich
## Date: 9 March 2019
##
## Use: This script checks that token groupings coded from a PowerPoint-based 
##      free-classification task include all and only the number of tokens included
##      in the task.
##
## Input: This script takes a tab-delimited .txt file in the format given in the example below
## 
##     Subject     Experiment     Version         Slide  Group   Tokens
##     Inperc-32   V              StereotypeA     1      1       1, 15, 20, 30, 32, 44, 46
##     Inperc-32   V              StereotypeA     1      2       29, 10, 12, 16, 21, 45, 38, 39
##     Inperc-32   V              StereotypeA     1      3       34, 2, 6, 11, 18, 17, 24


# Set working directory 

setwd("~/Desktop")


# Load table 

theData <- read.table("FreeClassPPTdata.txt", sep = "\t", header = TRUE)


# Convert responses into numerals 

tokensToNumerals <- function(x) as.numeric(unlist(strsplit(as.character(x), split=", ")))   # convert character vector into numerals while removing comma delimiter
theData$TokenNumerals <- lapply(theData$Tokens, tokensToNumerals)                           

uniqueParticipants <- vector()    # initialize vector to store participant names once
combinedTokens <- list()          # initialize list to store the combined token lists for each participant
tokenIndex <- 1                   # an index to keep track of each token in the following loop (It never gets reset to zero because the tokens are ordered sequenctially in vector)
partIndex <- 1                    # an index to keep track of each participant in the following loop

for (participant in theData$Subject) {                           # loop through all rows of Subject column
  if (!(participant %in% uniqueParticipants)) {                  # check if particular Subject has been encountered yet
    uniqueParticipants <- c(uniqueParticipants, participant)     # If not, add Subject to list
    combinedTokens[[participant]] <- unlist(theData$TokenNumerals[tokenIndex])    # Since this is the first time through the loop for a participant, add the 
    tokenIndex <- tokenIndex + 1
    partIndex <- partIndex + 1
  } else {
    combinedTokens[[participant]] <- sort(c(combinedTokens[[participant]],                 # if the participant has been encountered before, add the next set of tokens to their tag in the list
                                            unlist(theData$TokenNumerals[tokenIndex])))
    tokenIndex <- tokenIndex + 1
  }
}
print(uniqueParticipants)
print(combinedTokens)


# Check if vectors contain all and only numbers 1 through 48 

goal <- (1:48)
failedCheck <- vector()
for (i in 1:length(combinedTokens)) {
  if (!(all(unlist(combinedTokens[[i]]) == goal))) {             # if the list of tokens for each participant doesn't contain all and only the numbers from 1 to 48
    failedCheck <- c(failedCheck, (names(combinedTokens[i])))    # store the Participant ID                                   # and print 
  }
}   
print(failedCheck)                                               # Print the list of incorrectly coded Participants

