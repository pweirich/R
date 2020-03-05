#####################################################
#####################################################
#
# Title: Simulate 4AFC Data
#
# Description:  This script defined a function to
#               create a network graph. It then
#               simulates data for a perceptual
#               categorization task based on a
#               task designed for my dissertation.
#               Different scenarios are simulated
#               and the results visualized as network 
#               graphs.
#
# Author: Phillip Weirich (phillipweirich@yahoo.com)
#
#####################################################
#####################################################


## This code simulates responses to the 4AFC

#######################
#
# Define a function to
# easily plot network
# relationships
#
#######################
library(igraph)

plotNetwork <- function(cm, edgeWidth = addmargins(cm[[2]])[5,5]/96, balance = TRUE) {
  if (balance == TRUE) {                                             # "balance" weights the responses according to the prevalence of the stimuli to make visualizations more intuitive
    cm[[2]][,1] <- cm[[2]][,1]/sum(cm[[2]][,1])
    cm[[2]][,2] <- cm[[2]][,2]/sum(cm[[2]][,2])
    cm[[2]][,3] <- cm[[2]][,3]/sum(cm[[2]][,3])
    cm[[2]][,4] <- cm[[2]][,4]/sum(cm[[2]][,4])
  }
  links <- data.frame(
    source <- c(rep("A", 4), rep("C", 4), rep("B", 4), rep("D", 4)),  # the source and target are reference and prediction from the confusion matrix
    target <- c(rep(c("A", "C", "B", "D"), 4)),
    responseFrequency <- as.vector(cm[[2]])                                  # this pulls in the confusion matrix as a vector that matches the order of the source and target vectors above
  )

  nodes <- data.frame(
    name <- unique(source)                  # The nodes of the graph are just the levels of the input
  )

  # Turn it into igraph object
  network=graph_from_data_frame(d=links, vertices=nodes, directed = T)  # Directed means that individual lines are drawn for each pair, (e.g. both I -> M and M -> I)


  coords <- layout_in_circle(network, # Get the coordinates of the graph so it's consistent every time
                             order = c(2,3,1,4)
  )

  loops <- which_loop(network)            # Find which edges are loops
  loopAngles <- replace(loops, loops==TRUE, c(3*pi/2, pi/2, 2*pi, pi))  # set the angles for loops

  colrs <- c("green", "orange", "blue", "red")   # set colors of nodes
  V(network)$color <- colrs

  edge.start <- ends(network, es=E(network), names=F)[,1]   # Color edges according to the originating node
  edge.col <- V(network)$color[edge.start]

  #                      I      M      N      S
  inLabCoords.x <- c(-1,     0.45, -0.40, -0.28)   # Manually set locations of
  inLabCoords.y <- c( 0.44,  0.15,  0.80, -0.50)   # edge labels
  mLabCoords.x <-  c(-0.45,  1,     0.28,  0.40)
  mLabCoords.y <- c( -0.16, -0.44,  0.50, -0.80)
  nLabCoords.x <- c( -0.50,  0.80,  0.47,  0.18)
  nLabCoords.y <- c(  0.28,  0.40,  1,    -0.45)
  sLabCoords.x <- c( -0.80,  0.50, -0.18, -0.47)
  sLabCoords.y <- c( -0.40,  -0.28, 0.45, -1   )

  par(bg="grey32", mar=c(0,0,0,0))    # Set some graphical parameters
  plot(network,
       edge.label = format(round(responseFrequency, 2), nsmall = 2), # Edge labels have exactly two decimal places
       edge.label.color = "white",
       edge.label.family = "Helvetica",
       #       edge.label.font = 2,  # 2 makes it bold
       edge.label.x = c(inLabCoords.x, mLabCoords.x, nLabCoords.x, sLabCoords.x),
       edge.label.y = c(inLabCoords.y, mLabCoords.y, nLabCoords.y, sLabCoords.y),
       edge.color=edge.col,            # sets color of edges
       edge.width = responseFrequency/edgeWidth,     # sets thickness of lines based on response frequency. The raw values are too big, so it needs to be divided by an appropriate factor
       arrow.mode = 2,                 # 2 means arrows point away
       edge.loop.angle = loopAngles,   # sets angle of the loops
       edge.curved = 0.2,      # The edges (or lines) need to be curved, otherwise the two directional edges overlap
       layout = coords        # This keeps the graph in the same orientation
  )
}


#################################################################################

library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")


#################################################################################



library(caret)

# Select number of participants to simulate
nParticipants <- 20
nStimuli <- 96

# Set seed for random number generation so my comments on this code will make sense if it's run again
set.seed(80424)

# Create "correct" responses
correctStimuli <- c(replicate(16, "A"),
                    replicate(32, "B"),
                    replicate(16, "C"),
                    replicate(32, "D"))

# Create list of correct regions for each participant
correctRegion <- factor(replicate(nParticipants, correctStimuli))

# Create list of response regions
responseRegion <- c("A", "B", "C", "D")

# Simulate responses
set.seed(80424)
clicked <- as.factor(sample(responseRegion,
                            size = nParticipants*nStimuli,
                            replace = TRUE))

# Compute confusion matrix
cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="Random", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_Random','.pdf'))
dev.off()


#########
# Basically Random Responses
#########

# Change correct response 80% of the time

set.seed(80425)
change <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.70, 0.30), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if (change[i] == TRUE) {
    clicked[i] <- sample(levels(correctRegion), 1)
  } else
    clicked[i] <- correctRegion[i]
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="30% Correct", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_BasicallyGuess','.pdf'))
dev.off()






#########
# Basically Correct Responses
#########

# Change correct response 20% of the time

set.seed(80424)
change <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.50, 0.50), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if (change[i] == TRUE) {
    clicked[i] <- sample(levels(correctRegion), 1)
  } else
    clicked[i] <- correctRegion[i]
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="50% Correct", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_BasicallyCorrect','.pdf'))
dev.off()






#########
# Random Northern Bias (with extra noise)
#########

# Randomly Respond "North" 30% of the time and select random region 50%
# This will simulate guessing a lot of the time but also having a "go to" guess.

set.seed(80424)
regionBias <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.30, 0.70), replace = TRUE)
changeRandom <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.70, 0.30), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if (regionBias[i] == TRUE) {
    clicked[i] <- "B"
  } else {
    if (changeRandom[i] == TRUE) {
      clicked[i] <- sample(levels(correctRegion), 1)
    } else
      clicked[i] <- correctRegion[i]
  }
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="Default Bias", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_DefaultBias','.pdf'))
dev.off()



#########
# Correct North Bias
# "specific correct response bias"
#########

# Get "North" correct 50% of the time with 50% noise to make it interesting

set.seed(80424)
regionBias <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.50, 0.50), replace = TRUE)
changeRandom <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.85, 0.15), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if ((regionBias[i] == TRUE) & (correctRegion[i] == "C")) {
    clicked[i] <- "C"
  } else {
    if (changeRandom[i] == TRUE) {
      clicked[i] <- sample(levels(correctRegion), 1)
    } else
      clicked[i] <- correctRegion[i]
  }
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="Selective\nSensitivity\nBias", col.main="white", cex.main = 1.5, adj = 0.05, line = -5)

dev.copy(pdf,paste0('4AFC_Simulation_SpecificBias','.pdf'))
dev.off()



#########
# North/Inland Confusion (pure)
#########

# Randomly respond either "North" or "Inland" any time either "North" or "Inland" occur
# So, when "North" is correct, "North" or "Inland" are equally likely to be the response
# Additional noise is added

set.seed(80424)
regionBias <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.50, 0.50), replace = TRUE)
changeRandom <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.85, 0.15), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if ((regionBias[i] == TRUE) & ((correctRegion[i] == "C") | (correctRegion[i] == "A"))) {
    clicked[i] <- sample(c("C", "A"), 1)
  } else {
    if (changeRandom[i] == TRUE) {
      clicked[i] <- sample(levels(correctRegion), 1)
    } else
      clicked[i] <- correctRegion[i]
  }
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="Confusion Bias\nwith Guessing", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_ConfusionGuessing','.pdf'))
dev.off()



#########
# North/Inland Confusion (Inland bias)
#########

# When North is correct, respond "Inland" 80% of the time

set.seed(80424)
regionBias <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.50, 0.50), replace = TRUE)
changeRandom <- sample(c(TRUE, FALSE), length(correctRegion), prob = c(0.85, 0.15), replace = TRUE)
clicked <- factor(levels = levels(correctRegion))

for (i in 1:length(correctRegion)){
  if ((regionBias[i] == TRUE) & (correctRegion[i] == "C")) {
    clicked[i] <- "A"
  } else {
    if (changeRandom[i] == TRUE) {
      clicked[i] <- sample(levels(correctRegion), 1)
    } else
      clicked[i] <- correctRegion[i]
  }
}

cm <- confusionMatrix(clicked, correctRegion, mode = "everything")
cm

summaryTable <- t(cm[[4]][,c(1,2,5,10,11)])
summaryTable[,c(2,3)] <- summaryTable[,c(3,2)]
xtable(summaryTable)

plotNetwork(cm)
title(main="Confusion Bias\nwith Default", col.main="white", cex.main = 1.5, adj = 0.05, line = -4)

dev.copy(pdf,paste0('4AFC_Simulation_ConfusionDefault','.pdf'))
dev.off()
