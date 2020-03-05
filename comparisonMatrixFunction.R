## Function:  Create similarity or distance matrix from
##            Free Classification data
##
## Authors: Aaron Albin wrote the important parts
##          Phillip Weirich made it into a function
##
## Input: The function requires 1) the Free Classification 
##        data, 2) a Lookup table that maps the Free
##        Classification token numbers to meaningful
##        analytic categories, 3) a 'type' of output, 
##        either a similarity or distance matrix, similarity
##        is the default, and 4) a number of stimuli if a 
##        distance matrix is desired.
##
## Output:  
##
##
###########################################################
###########################################################

###################################


comparisonMatrix <- function(Dataset, Lookup, type = "similarity") {

# BEGIN THE ACTUAL ANALYSIS TO CREATE THE GIGANTIC SIMILARITY MATRIX

# Similarity is by count, not percent.

nSubjects = length(unique(Dataset$Subject))

nGroups = length(unique(Lookup$Code))

nRow = nrow(Dataset) # 683

AllCodes = unique(as.character(Lookup$Code)[order(Lookup$Experiment,as.character(Lookup$Code))])

OutputMatrix = matrix(0, nrow=length(AllCodes), ncol=length(AllCodes), dimnames=list(AllCodes,AllCodes))

for(EachRow in 1:nRow){ # EachRow=1
  
  CurrentRow=Dataset[EachRow,]
  
  Experiment=as.character(CurrentRow[,"Experiment"])
  Version=as.character(CurrentRow[,"Version"])
  # Slide=CurrentRow[,"Slide"]
  Group=as.character(CurrentRow[,"Group"])
  Tokens=as.character(CurrentRow[,"Tokens"])
  SplitTokens = as.integer(strsplit(Tokens,split=",")[[1]])
  
  nTokens=length(SplitTokens)
  Recoded = rep(NA,times=nTokens)
  for(EachToken in 1:nTokens){ # EachToken=1
    
    CurrentToken=SplitTokens[EachToken]
    
    #if(Version=="StereotypeA"){ColumnName="ASlide"}else{ColumnName="BSlide"}
    
    Target= as.character(Lookup$Experiment)=="StereotypeA" &
      #        Lookup[,ColumnName]==Slide & 
      Lookup$IconNumber==CurrentToken
    
    Recoded[EachToken] <- as.character(Lookup[Target,"Code"])
    
  } # End 'EachToken' loop
  
  Combinations = t(combn(Recoded,m=2))
  Columns=Combinations[,1]
  Rows=Combinations[,2]
  
  nCombinations = nrow(Combinations)
  for(EachCombination in 1:nCombinations){ # EachCombination=1
    Sorted = sort(c(Columns[EachCombination],Rows[EachCombination]))
    ThisRow=Sorted[1]
    ThisColumn=Sorted[2]
    
    OutputMatrix[ThisRow,ThisColumn] <- OutputMatrix[ThisRow,ThisColumn] + 1
  } # End 'each combination' row
  
} # End 'each row' loop

# Write this out to the hard disk

if (type == "similarity") {
  similarityMatrix <- OutputMatrix

  write.table(similarityMatrix,file="similarityMatrix.txt",sep="\t",quote=FALSE)
  
  return (similarityMatrix)
}


## Convert similarities (calculated above) to dissimilarities 

if (type == "distance") {
  distanceMatrix <- (1 - (OutputMatrix/(nSubjects*(nrow(OutputMatrix)-1))))        # Calculate distance ratio from 0 (most similar) to 1 (most different) by subracting the relative similarity (number of times two tokens were grouped together divided by the total number of opportunities to be grouped (i.e., the total number of participants times total number of token comparisons (48-1 because the identity comparison is not possible))) from the maximum dissimilarity value (1)
  distanceMatrix[lower.tri(distanceMatrix, diag = TRUE)] <- 0     # Make all values in the diagonal and lower half of triangle 0 (I tried to set it to NA, but this seemed to cause problems)
  distanceMatrix <- t(distanceMatrix)                             # Transform the matrix so the upper triangle becomes the lower triangle
  
  write.table(distanceMatrix,file="distanceMatrix.txt",sep="\t",quote=FALSE)   # Save the distance matrix
  
  return (distanceMatrix)
}

} # end function
