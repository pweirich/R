###############################################################################
###############################################################################
##
##  Script for analyzing data from Indiana Culture Survey
##
##  This script is for data that I collected with the first distribution of
##  the Indiana Culture Survey (collected mid-September--early-October).
##
##  The script uses data that has already been processed by the scripts:
##  Orientation[ProcessRawData], and
##  Orientation[SubsetAndCategorize]
##
##  This script looks in the working directory for the most recent output
##  of Orientation[SubsetAndCategorize].
##
##  All of the statistical analyses of the data, including graphic
##  representations, should be done with with script. Only analysis-specific
##  subsetting should be done. Subsetting for more general application should
##  be done in the script Orientation[SubsetAndCategorize] so that relevant
##  information is readily and consistently available to other analyses.
##
##  Output: This script generates statistical analyses and graphical output
##          that can be included in the write-up of this analysis.
##
###############################################################################
###############################################################################
#
#  Written by: Phillip Weirich
#  Email: pweirich@indiana.edu
#  Date: Fall 2016
#
###############################################################################
###############################################################################

## Set working directory
setwd("~/Google Drive/Indiana/Projects/Orientation Survey/Data")

# Find the most recent version of the output of Orientation[SubsetAndCategorize]
details = file.info(list.files(pattern="DataForAnalysis"))
details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]
mostRecentVersion = rownames(details)[1]

## Import most recent verstion of the processed data
# (output of Orientation[ProcessRawData])
DataForAnalysis = read.csv( file=paste0(mostRecentVersion))

## Load required packages

# These commands might be necessary to get
# ggmap working
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")

# install.packages("ggmap")
require(ggmap)
require(ggplot2)
require(Hmisc)
#source("DBDA2E-utilities.R")

# Load packages for likert description
# http://jason.bryer.org/likert/
install.packages("devtools")
require(devtools)
install_github('likert', 'jbryer')
install.packages("scales")
require(scales)
require(likert)
library(plyr)

##############################
##                          ##
## Basic summary of results ##
##  by construct            ##
##                          ##
##############################

## Demographic info ##

# school
summary(DataForAnalysis$Current_university)

# year in school
summary(DataForAnalysis$Year_in_school)

# major
summary(DataForAnalysis$Major)

# ethnicity
summary(DataForAnalysis$Ethnicity)

# ancestry
summary(DataForAnalysis$Ancestry)

# religion
summary(DataForAnalysis$Religious_tradition)

## Residence History ##
# location lived the longest
# lived outside of Indiana
# lived outside of the US

## Religion ##
# Distribution of religions
summary(DataForAnalysis$Religious_tradition)

# Service attendence
summary(DataForAnalysis$Religious_participation)
plot(DataForAnalysis$Religious_participation)

attendence <- ggplot(DataForAnalysis,
                     aes(factor(DataForAnalysis$Religious_participation,
                         levels=c("Daily", "Weekly", "Monthly", "Yearly", "Never"))))

religiousParticipation <- attendence +
  geom_bar() +
  xlab("Religious Participation" ) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.5)

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/religiousParticipation.pdf",width=5.5,height=7)
religiousParticipation
dev.off()

## Sports ##
summary(DataForAnalysis$Professional_basketball)
summary(DataForAnalysis$Professional_baseball)
summary(DataForAnalysis$Professional_hockey)
summary(DataForAnalysis$Professional_football)
summary(DataForAnalysis$NCAA_teams)


###########################
##                       ##
## Basic Geographic Info ##
##                       ##
###########################


# Define boundaries of map
myLocation <- c(-88.25, 38.5, -85, 42)

# Designate Northen, Middle, or Southern based on 'Place lived longest'
# This chunk of code for determining nothern and southern boundaries
# is taken from the earlier script, Orientation[SubsetAndCategorize]
# For more details on why I divided it this way, see that script.

furthestSouth <- 37.77
furthestNorth <- 41.78

# calculate lattitude boundaries for geographic northern and southern regions
indianaThirds <- (furthestNorth - furthestSouth)/3

southernBoundary <- (furthestSouth + indianaThirds)
northernBoundary <- (furthestNorth - indianaThirds)


# Download map (see ggmap cheat sheet for more info)
myMap <- get_map(location=myLocation, source="stamen", maptype="toner", crop=FALSE)

# myLocation <- c(-88, 36, -84.5, 44)
# myLocation <- "Indiana"
# myMap <- get_map(location=myLocation, source="google", maptype="terrain", crop=FALSE)


# Generate the map with Respondent locations
map <- ggmap(myMap)+
  geom_point(aes(x = DataForAnalysis$lon,
                 y = DataForAnalysis$lat),                  # what columns the data comes from
             data = DataForAnalysis,                        # which data frame the coordinate date are in
             alpha = .25,                                   # transparency
             color="darkred",                               # color of points
             size = 3,                                      # size of points
             position=position_jitter(w = 0.01, h = 0.01) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )
map

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/respondentLocations.pdf",width=5.5,height=7)
map
dev.off()

# Generate the map with Respondent locations and geographic boundary lines
ggmap(myMap)+
  geom_point(aes(x = DataForAnalysis$lon,
                 y = DataForAnalysis$lat),                  # what columns the data comes from
             data = DataForAnalysis,                            # which data frame the coordinate date are in
             alpha = .25,                                   # transparency
             color="darkred",                               # color of points
             size = 3,                                      # size of points
             position=position_jitter(w = 0.01, h = 0.01) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )+
  geom_hline(yintercept = c(southernBoundary, northernBoundary))

#####################
##                 ##
## Factor Analyses ##
##                 ##
#####################

## Language Attitudes

# Put the language attitude questions into their own data frame for convenience
languageAttitudes <- cbind(DataForAnalysis$Beliefs_of_speech_in_your_region_fast1_slow7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_polite1_rude7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_down_to_earth1_snobbish7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_educated1_uneducated7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_normal1_abnormal7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_smart1_dumb7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_formal1_casual7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_goodEnglish1_badEnglish7,
                           DataForAnalysis$Beliefs_of_speech_in_your_region_friendly1_unfriendly7
)


# Correlations/covariances among numeric variables in
# data frame mtcars. Use listwise (instead of pairwise) deletion of missing data.
pearsonCor <- cor(languageAttitudes, use="complete.obs", method="pearson")

## Principal Components Analysis

# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(pearsonCor, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

## Exploratory factor analysis

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors (scree plot of PCA showed
# an 'elbow' at 3 PCs),
# with varimax rotation
fit <- factanal(languageAttitudes, 3, rotation="varimax")
print(fit, digits=2, cutoff=.5, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load) # set up plot
text(load,labels=names(languageAttitudes),cex=.7) # add variable names


## Hometown Satisfaction

# Put the language attitude questions into their own data frame for convenience
hometownSatisfaction <- cbind.data.frame(DataForAnalysis$Satisfaction_with_hometown_family,
                           DataForAnalysis$Satisfaction_with_hometown_friends,
                           DataForAnalysis$Satisfaction_with_hometown_social_life,
                           DataForAnalysis$Satisfaction_with_hometown_economy,
                           DataForAnalysis$Satisfaction_with_hometown_job_prospects,
                           DataForAnalysis$Satisfaction_with_hometown_education,
                           DataForAnalysis$Satisfaction_with_hometown_weather,
                           DataForAnalysis$Satisfaction_with_hometown_food,
                           DataForAnalysis$Satisfaction_with_hometown_safety
)


# Correlations/covariances among numeric variables in
# data frame mtcars. Use listwise (instead of pairwise) deletion of missing data.
pearsonCor <- cor(hometownSatisfaction, use="complete.obs", method="pearson")

## Principal Components Analysis

# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(pearsonCor, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

## Exploratory factor analysis

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors (scree plot of PCA showed
# an 'elbow' at 3 PCs),
# with varimax rotation
fit <- factanal(hometownSatisfaction, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load) # set up plot
text(load,labels=names(hometownSatisfaction),cex=.7) # add variable names


########################
##                    ##
## Construct Analyses ##
##                    ##
########################

## Regional Pride

# How much do you like living in your hometown

likeHometown <- DataForAnalysis$Like_living_in_hometown_gradient
likeHometown[likeHometown==99] <- NA
likeHometownClean <- na.omit(likeHometown)

summary(likeHometownClean)
summary(as.factor(likeHometownClean))
100*(summary(as.factor(likeHometownClean))/length(likeHometownClean)) # percent of total responses
plot(likeHometownClean)
barplot(table(likeHometownClean), main = "Do you like living in your hometown")

likeHometownBinary <- DataForAnalysis$Like_living_in_hometown_binary # The data has no missing responses
100*(summary(likeHometownBinary)/length(likeHometownBinary))


# Describing the hometown satisfaction responses

hometownSatisfaction <- cbind.data.frame(hometownSatisfaction, DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Like_living_in_hometown_binary)

describe(hometownSatisfaction)

ncol(hometownSatisfaction)

library(plyr)
hometownSatisfaction <- rename(hometownSatisfaction,
                               c("DataForAnalysis$Satisfaction_with_hometown_family" = "Family",
                               "DataForAnalysis$Satisfaction_with_hometown_friends" = "Friends",
                               "DataForAnalysis$Satisfaction_with_hometown_social_life" = "Social Life",
                               "DataForAnalysis$Satisfaction_with_hometown_economy" = "Economy",
                               "DataForAnalysis$Satisfaction_with_hometown_job_prospects" = "Job Prospects",
                               "DataForAnalysis$Satisfaction_with_hometown_education" = "Education",
                               "DataForAnalysis$Satisfaction_with_hometown_weather" = "Weather",
                               "DataForAnalysis$Satisfaction_with_hometown_food" = "Food",
                               "DataForAnalysis$Satisfaction_with_hometown_safety" = "Safety",
                               "DataForAnalysis$From_which_region_in_Indiana" = "Region",
                               "DataForAnalysis$Like_living_in_hometown_binary" = "Like Hometown"
                               ))

hometownSatisfaction$Region <- revalue(hometownSatisfaction$Region, c("Other (please specify)"="Other"))

head(hometownSatisfaction)

hometownSatisfaction[hometownSatisfaction==99] <- NA   # Replace 99 data with NA
hometownSatisfaction <- na.omit(hometownSatisfaction)  # Delete rows that contain NA

# Convert all columns to factors
hometownSatisfaction$Family <- as.factor(hometownSatisfaction$Family)
hometownSatisfaction$Friends <- as.factor(hometownSatisfaction$Friends)
hometownSatisfaction$`Social Life` <- as.factor(hometownSatisfaction$`Social Life`)

hometownSatisfaction$Economy <- as.factor(hometownSatisfaction$Economy)
hometownSatisfaction$`Job Prospects` <- as.factor(hometownSatisfaction$`Job Prospects`)
hometownSatisfaction$Education <- as.factor(hometownSatisfaction$Education)

hometownSatisfaction$Weather <- as.factor(hometownSatisfaction$Weather)
hometownSatisfaction$Food <- as.factor(hometownSatisfaction$Food)
hometownSatisfaction$Safety <- as.factor(hometownSatisfaction$Safety)

likertHometownSatisfaction <- likert(hometownSatisfaction[,1:9])
summary(likertHometownSatisfaction)

likertHometownSatisfactionPlot <- plot(likertHometownSatisfaction)

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertHometownSatisfactionPlot.pdf",width=5.5,height=7)
likertHometownSatisfactionPlot
dev.off()

plot(likertHometownSatisfaction, centered = FALSE, wrap = 30)

plot(likertHometownSatisfaction, type = "density")

plot(likertHometownSatisfaction, type = "heat")

likertHometownSatisfactionRegion <- likert(hometownSatisfaction[,1:9], grouping = hometownSatisfaction[,10])
print(likertHometownSatisfactionRegion)

plot(likertHometownSatisfactionRegion)

likertHometownSatisfactionRegionPlot <- plot(likertHometownSatisfactionRegion, type = "density")

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertHometownSatisfactionRegionPlot.pdf",width=5.5,height=7)
likertHometownSatisfactionRegionPlot
dev.off()

likertHometownSatisfactionLikeHometown <- likert(hometownSatisfaction[,1:9], grouping = hometownSatisfaction[,11])
print(likertHometownSatisfactionLikeHometown)

likertHometownSatisfactionLikeHometownPlot <- plot(likertHometownSatisfactionLikeHometown, type = "density")

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertHometownSatisfactionLikeHometownPlot.pdf",width=5.5,height=7)
likertHometownSatisfactionLikeHometownPlot
dev.off()

## Homebody

# How likely are you to move outside of the following regions
moveAway <- cbind.data.frame(DataForAnalysis$Likelihood_of_moving_outside_of_hometown,
                             DataForAnalysis$Likelihood_of_moving_outside_of_Indiana,
                             DataForAnalysis$Likelihood_of_moving_outside_of_USA,
                             DataForAnalysis$Has_passport)

moveAway[moveAway==99] <- NA
moveAwayClean <- na.omit(moveAway)

summary(moveAwayClean)

summary(as.factor(moveAwayClean$`DataForAnalysis$Likelihood_of_moving_outside_of_hometown`))
summary(as.factor(moveAwayClean$`DataForAnalysis$Likelihood_of_moving_outside_of_Indiana`))
summary(as.factor(moveAwayClean$`DataForAnalysis$Likelihood_of_moving_outside_of_USA`))


## frequency table of "move out of Indiana" and "has passport"

mytable <- table(moveAwayClean$`DataForAnalysis$Likelihood_of_moving_outside_of_Indiana`, moveAwayClean$`DataForAnalysis$Has_passport`)

prop.table(mytable) # cell percentages

mytable

## frequency table of "move out of USA" and "has passport"

mytable <- table(moveAwayClean$`DataForAnalysis$Likelihood_of_moving_outside_of_USA`, moveAwayClean$`DataForAnalysis$Has_passport`)

prop.table(mytable) # cell percentages

mytable


# Which regions would you like to visit
visit <- cbind.data.frame(DataForAnalysis$Regions_to_visit_Indiana,
                             DataForAnalysis$Regions_to_visit_state,
                             DataForAnalysis$Regions_to_visit_country)

visit[visit==99] <- NA
visitClean <- na.omit(visit)

summary(visitClean)


100*(length(DataForAnalysis$Regions_to_visit_Indiana[DataForAnalysis$Regions_to_visit_Indiana == TRUE])/length(DataForAnalysis$Regions_to_visit_Indiana))
100*(length(DataForAnalysis$Regions_to_visit_state[DataForAnalysis$Regions_to_visit_state == TRUE])/length(DataForAnalysis$Regions_to_visit_state))
100*(length(DataForAnalysis$Regions_to_visit_country[DataForAnalysis$Regions_to_visit_country == TRUE])/length(DataForAnalysis$Regions_to_visit_country))

# Which regions would you like to live

live <- cbind.data.frame(DataForAnalysis$Regions_to_live_Indiana,
                          DataForAnalysis$Regions_to_live_state,
                          DataForAnalysis$Regions_to_live_country)

live[live==99] <- NA
liveClean <- na.omit(live)

summary(liveClean)


100*(length(DataForAnalysis$Regions_to_live_Indiana[DataForAnalysis$Regions_to_live_Indiana == TRUE])/length(DataForAnalysis$Regions_to_live_Indiana))
100*(length(DataForAnalysis$Regions_to_live_state[DataForAnalysis$Regions_to_live_state == TRUE])/length(DataForAnalysis$Regions_to_live_state))
100*(length(DataForAnalysis$Regions_to_live_country[DataForAnalysis$Regions_to_live_country == TRUE])/length(DataForAnalysis$Regions_to_live_country))

## Do you have a passport

summary(DataForAnalysis$Has_passport)


## Cardinality

summary(DataForAnalysis$From_which_region_in_Indiana)

fromCentral <- grepl(c("entral"), DataForAnalysis$From_which_region_in_Indiana_other)
length(fromCentral[fromCentral == TRUE])

# respondents regions
DataForAnalysis$From_which_region_in_Indiana <-    # relabel level to be appropriate
  revalue(DataForAnalysis$From_which_region_in_Indiana, c("Other (please specify)"="Other"))

map <- ggmap(myMap)+
  geom_point(aes(x = DataForAnalysis$lon,
                 y = DataForAnalysis$lat,
                 colour = DataForAnalysis$From_which_region_in_Indiana),                  # what columns the data comes from
             data = DataForAnalysis,                            # which data frame the coordinate date are in
             alpha = 0.5,                                   # transparency
             size = 2,                                      # size of points
             position=position_jitter(w = 0.2, h = 0.2) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )+
  labs(colour="Regions") +
  geom_hline(yintercept = c(southernBoundary, northernBoundary)) +
  theme(legend.position="bottom")
map


pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/regions.pdf",width=5.5,height=7)
map
dev.off()

## Breadth of region

summary(DataForAnalysis$Breadth_of_region)

## Personal Accentedness

summary(as.factor(DataForAnalysis$Personal_distinct_regional_accent_no1_yes7))


summary(DataForAnalysis$Personal_distinct_accent_told)

accentMatrix <- cor(DataForAnalysis$Personal_distinct_regional_accent_no1_yes7,
    as.numeric(DataForAnalysis$Personal_distinct_accent_told), use = "complete.obs",
    method = "pearson")

cor.test(DataForAnalysis$Personal_distinct_regional_accent_no1_yes7,
         as.numeric(DataForAnalysis$Personal_distinct_accent_told))

library(Hmisc)
rcorr(cbind(DataForAnalysis$Personal_distinct_regional_accent_no1_yes7,
        as.numeric(DataForAnalysis$Personal_distinct_accent_told)), type="pearson") # type can be pearson or spearman



# Home Region Accentedness
# (where is the data for "Does eveyone have the same accent? It must have been lost at the
#  ProcessRawData level, because it isn't in the labels there)
# Update: This question was not included in the Qualtrics survey

summary(DataForAnalysis$Is_your_region_good_to_learn_neutral_American_accent)


# Describing the regional accent responses

regionalAccent <- cbind.data.frame(DataForAnalysis$Beliefs_of_speech_in_your_region_fast1_slow7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_smart1_dumb7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_polite1_rude7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_formal1_casual7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_normal1_abnormal7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_educated1_uneducated7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_friendly1_unfriendly7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_down_to_earth1_snobbish7,
                                   DataForAnalysis$Beliefs_of_speech_in_your_region_goodEnglish1_badEnglish7,
                                   DataForAnalysis$From_which_region_in_Indiana,
                                   DataForAnalysis$Like_living_in_hometown_binary)

head(regionalAccent)
describe(regionalAccent)

ncol(regionalAccent)

library(plyr)
regionalAccent <- rename(regionalAccent,
                               c("DataForAnalysis$Beliefs_of_speech_in_your_region_fast1_slow7" = "Fast:Slow",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_smart1_dumb7" = "Smart:Dumb",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_polite1_rude7" = "Polite:Rude",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_formal1_casual7" = "Formal:Casual",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_normal1_abnormal7" = "Normal:Abnormal",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_educated1_uneducated7" = "Educated:Uneducated",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_friendly1_unfriendly7" = "Friendly:Unfriendly",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_down_to_earth1_snobbish7" = "Down-to-earth:Snobbish",
                                 "DataForAnalysis$Beliefs_of_speech_in_your_region_goodEnglish1_badEnglish7" = "Good English:Bad English",
                                 "DataForAnalysis$From_which_region_in_Indiana" = "Region",
                                 "DataForAnalysis$Like_living_in_hometown_binary" = "Like Hometown"
                               ))

regionalAccent$Region <- revalue(regionalAccent$Region, c("Other (please specify)"="Other"))



head(regionalAccent)

regionalAccent[regionalAccent==99] <- NA   # Replace 99 data with NA
regionalAccent <- na.omit(regionalAccent)  # Delete rows that contain NA

# Convert all columns to factors
regionalAccent$`Fast:Slow` <- as.factor(regionalAccent$`Fast:Slow`)
regionalAccent$`Smart:Dumb` <- as.factor(regionalAccent$`Smart:Dumb`)
regionalAccent$`Polite:Rude` <- as.factor(regionalAccent$`Polite:Rude`)

regionalAccent$`Formal:Casual` <- as.factor(regionalAccent$`Formal:Casual`)
regionalAccent$`Normal:Abnormal` <- as.factor(regionalAccent$`Normal:Abnormal`)
regionalAccent$`Educated:Uneducated` <- as.factor(regionalAccent$`Educated:Uneducated`)

regionalAccent$`Friendly:Unfriendly` <- as.factor(regionalAccent$`Friendly:Unfriendly`)
regionalAccent$`Down-to-earth:Snobbish` <- as.factor(regionalAccent$`Down-to-earth:Snobbish`)
regionalAccent$`Good English:Bad English` <- as.factor(regionalAccent$`Good English:Bad English`)

levels(regionalAccent$`Normal:Abnormal`) <- c(1:7)           # Make sure this one has 7 levels
likertRegionalAccent <- likert(regionalAccent[,1:9])
summary(likertRegionalAccent)

likertRegionalAccentPlot <- plot(likertRegionalAccent)

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertRegionalAccentPlot.pdf",width=6.5,height=7)
likertRegionalAccentPlot
dev.off()

plot(likertRegionalAccent, centered = FALSE, wrap = 30)

plot(likertRegionalAccent, type = "density")

plot(likertRegionalAccent, type = "heat")

likertRegionalAccentRegion <- likert(regionalAccent[,1:9], grouping = regionalAccent[,10])
print(likertRegionalAccentRegion)

plot(likertRegionalAccentRegion)

likertRegionalAccentRegionPlot <- plot(likertRegionalAccentRegion, type = "density")

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertRegionalAccentRegionPlot.pdf",width=5.5,height=6)
likertRegionalAccentRegionPlot
dev.off()

likertRegionalAccentLikeHometown <- likert(regionalAccent[,1:9], grouping = regionalAccent[,11])
print(likertRegionalAccentLikeHometown)

likertRegionalAccentLikeHometownPlot <- plot(likertRegionalAccentLikeHometown, type = "density")

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/likertRegionalAccentLikeHometownPlot.pdf",width=5.5,height=7)
likertRegionalAccentLikeHometownPlot
dev.off()








## respondents regions

# set a different color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# remove blank values
NeutralMapData <- subset(DataForAnalysis, Is_your_region_good_to_learn_neutral_American_accent!='',
                  select=c(lon, lat, Is_your_region_good_to_learn_neutral_American_accent))

map <- ggmap(myMap)+
  geom_point(aes(x = NeutralMapData$lon,
                 y = NeutralMapData$lat,
                 colour = NeutralMapData$Is_your_region_good_to_learn_neutral_American_accent),                  # what columns the data comes from
             data = NeutralMapData,                            # which data frame the coordinate date are in
             alpha = 0.75,                                   # transparency
             size = 2,                                      # size of points
             position=position_jitter(w = 0.2, h = 0.2) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )+
  labs(colour="Region has \nneutral accent") +
  geom_hline(yintercept = c(southernBoundary, northernBoundary)) +
  theme(legend.position="bottom")
map

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/goodLearnNeutralAccent.pdf",width=5.5,height=7)
map
dev.off()

## Regional Accent label

summary(DataForAnalysis$Which_accent_you_usually_speak)

# respondents regions
# remove blank values
AccentMapData <- subset(DataForAnalysis, Which_accent_you_usually_speak!='',
                         select=c(lon, lat, Which_accent_you_usually_speak))


map <- ggmap(myMap)+
  geom_point(aes(x = AccentMapData$lon,
                 y = AccentMapData$lat,
                 colour = AccentMapData$Which_accent_you_usually_speak),                 # what columns the data comes from
             data = AccentMapData,                            # which data frame the coordinate date are in
             alpha = 0.75,                                   # transparency
             size = 2,                                      # size of points
             position=position_jitter(w = 0.2, h = 0.2) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )+
  labs(colour="Usual Accent") +
  geom_hline(yintercept = c(southernBoundary, northernBoundary)) +
  theme(legend.position="bottom")
map

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/usualAccent.pdf",width=5.5,height=7)
map
dev.off()


## Linguistic Security

# Places you use a neutral accent




# Put the neutral accent questions into their own data frame for convenience
neutralAccent <- cbind.data.frame( DataForAnalysis$Situations_you_use_neutral_accent_job,
                              DataForAnalysis$Situations_you_use_neutral_accent_parents,
                              DataForAnalysis$Situations_you_use_neutral_accent_teachers,
                              DataForAnalysis$Situations_you_use_neutral_accent_boss,
                              DataForAnalysis$Situations_you_use_neutral_accent_church,
                              DataForAnalysis$Situations_you_use_neutral_accent_siblings,
                              DataForAnalysis$Situations_you_use_neutral_accent_spouse,
                              DataForAnalysis$Situations_you_use_neutral_accent_friends,
                              DataForAnalysis$Situations_you_use_neutral_accent_children
)

describe(neutralAccent)
summary(neutralAccent)
plot(neutralAccent)

# Rename columns
neutralAccent <- rename(neutralAccent,
                         c("DataForAnalysis$Situations_you_use_neutral_accent_job" = "Job",
                           "DataForAnalysis$Situations_you_use_neutral_accent_parents" = "Parents",
                           "DataForAnalysis$Situations_you_use_neutral_accent_teachers" = "Teachers",
                           "DataForAnalysis$Situations_you_use_neutral_accent_boss" = "Boss",
                           "DataForAnalysis$Situations_you_use_neutral_accent_church" = "Church",
                           "DataForAnalysis$Situations_you_use_neutral_accent_siblings" = "Siblings",
                           "DataForAnalysis$Situations_you_use_neutral_accent_spouse" = "Spouse",
                           "DataForAnalysis$Situations_you_use_neutral_accent_friends" = "Friends",
                           "DataForAnalysis$Situations_you_use_neutral_accent_children" = "Children"
                         ))


neutralNames <- c("Job", "Boss", "Teachers", "Parents", "Church", "Friends", "Children", "Spouse", "Siblings")
neutralFreqencies <- c(sum(neutralAccent$Job, na.rm = TRUE),
                       sum(neutralAccent$Boss, na.rm = TRUE),
                       sum(neutralAccent$Teachers, na.rm = TRUE),
                       sum(neutralAccent$Parents, na.rm = TRUE),
                       sum(neutralAccent$Church, na.rm = TRUE),
                       sum(neutralAccent$Friends, na.rm = TRUE),
                       sum(neutralAccent$Children, na.rm = TRUE),
                       sum(neutralAccent$Spouse, na.rm = TRUE),
                       sum(neutralAccent$Siblings, na.rm = TRUE)
                       )

# For the life of me, I cannot figure out how to get a barplot of logistic data.
# I am going to manually create a vector that can be plotted
# By replicating the relevant labels the number of times of their
# TRUE occurances. This is definitely a hack. Why is there not an easier way?
neutralAccentForPlot <- as.factor(c(rep("Job", sum(neutralAccent$Job, na.rm = TRUE)),
                      rep("Boss", sum(neutralAccent$Boss, na.rm = TRUE)),
                      rep("Teachers", sum(neutralAccent$Teachers, na.rm = TRUE)),
                      rep("Parents", sum(neutralAccent$Parents, na.rm = TRUE)),
                      rep("Church", sum(neutralAccent$Church, na.rm = TRUE)),
                      rep("Friends", sum(neutralAccent$Friends, na.rm = TRUE)),
                      rep("Children", sum(neutralAccent$Children, na.rm = TRUE)),
                      rep("Spouse", sum(neutralAccent$Spouse, na.rm = TRUE)),
                      rep("Siblings", sum(neutralAccent$Siblings, na.rm = TRUE))
                      ))

neutralAccentGGplot <- ggplot(as.data.frame(neutralAccentForPlot),
                     aes(factor(neutralAccentForPlot,
                                levels=c("Job", "Boss", "Teachers",
                                         "Parents", "Church", "Friends",
                                         "Children", "Spouse", "Siblings"))))


neutralAccentPlot <- neutralAccentGGplot +
  geom_bar() +
  xlab("Neutral Accent Situtations" ) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.5) +
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=90, hjust=0))




neutralAccentPlot

pdf("~/Google Drive/Indiana/Projects/Orientation Survey/Orientation Paper/images/neutralAccentPlot.pdf",width=5.6,height=7)
neutralAccentPlot
dev.off()

# Correlations/covariances among numeric variables in
# data frame mtcars. Use listwise (instead of pairwise) deletion of missing data.
pearsonCor <- cor(neutralAccent, use="complete.obs", method="pearson")

## Principal Components Analysis

# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(pearsonCor, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

## Exploratory factor analysis

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors (scree plot of PCA showed
# an 'elbow' at 2 PCs),
# with varimax rotation
# I can't get this to run with logical values.
fit <- factanal(neutralAccent, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load) # set up plot
text(load,labels=names(hometownSatisfaction),cex=.7) # add variable names


# Made fun of how you talk

summary(DataForAnalysis$Someone_has_made_fun_of_your_speech)

# Use different accent

summary(DataForAnalysis$Accent_shift_in_formal_vs_informal)

summary(DataForAnalysis$Accent_shift_between_strangers_and_intimates)

accentShift <- cbind.data.frame(DataForAnalysis$Accent_shift_in_formal_vs_informal,
                                DataForAnalysis$Accent_shift_between_strangers_and_intimates)

formalShift <- factor(DataForAnalysis$Accent_shift_in_formal_vs_informal)
strangersShift <- factor(DataForAnalysis$Accent_shift_between_strangers_and_intimates)
accentShift <- cbind(formalShift, strangersShift)

pearsonCor <- cor(accentShift, use="complete.obs", method="pearson")

cor.test(as.numeric(formalShift), as.numeric(strangersShift))  # pearsons by default
cor.test(as.numeric(formalShift), as.numeric(strangersShift), method="spearman")



## Cultural ties

# Religious Activity

summary(DataForAnalysis$Religious_participation)
summary(DataForAnalysis$religiousActive)

# Clubs and Social activities

summary(DataForAnalysis$Club_participation)

# Sports fans

summary(DataForAnalysis$sportsFan)

summary(DataForAnalysis$footballFan)
summary(DataForAnalysis$hockeyFan)
summary(DataForAnalysis$baseballFan)
summary(DataForAnalysis$basketballFan)
summary(DataForAnalysis$NCAAfan)

summary(DataForAnalysis$stlFan)
summary(DataForAnalysis$ohioFan)
summary(DataForAnalysis$chicagoFan)
summary(DataForAnalysis$IndyFan)
summary(DataForAnalysis$detroitFan)

# fan regions
ggmap(myMap)+
  geom_point(aes(x = DataForAnalysis$lon,
                 y = DataForAnalysis$lat,
                 colour = DataForAnalysis$chicagoFan),                  # what columns the data comes from
             data = DataForAnalysis,                            # which data frame the coordinate date are in
             alpha = 0.5,                                   # transparency
             size = 2,                                      # size of points
             position=position_jitter(w = 0.2, h = 0.2) # jitters points to reduce overlap (depends on the scale, as a percentage)
  )+
  labs(colour="Chicago Fan") +
  geom_hline(yintercept = c(southernBoundary, northernBoundary))


## Family history

summary(DataForAnalysis$Father_family_in_Indiana)

#################################################
##                                             ##
## Additional correlations and correspondences ##
##                                             ##
#################################################

## frequency table of "cardinality" and "accent usually spoken"

mytable <- table(DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Which_accent_you_usually_speak)

prop.table(mytable) # cell percentages

mytable

# 2-Way Cross Tabulation
library(gmodels)
CrossTable(DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Which_accent_you_usually_speak)

## frequency table of "how much..." and "do you like living in your hometown

mytable <- table(DataForAnalysis$Like_living_in_hometown_gradient, DataForAnalysis$Like_living_in_hometown_binary)

prop.table(mytable) # cell percentages

mytable

## frequency table of "How likely are you to move out of these regions" and Provenance

mytable <- table(DataForAnalysis$Likelihood_of_moving_outside_of_hometown, DataForAnalysis$From_which_region_in_Indiana)

prop.table(mytable) # cell percentages

mytable

mytable <- table(DataForAnalysis$Likelihood_of_moving_outside_of_Indiana, DataForAnalysis$From_which_region_in_Indiana)

prop.table(mytable) # cell percentages

mytable

mytable <- table(DataForAnalysis$Likelihood_of_moving_outside_of_USA, DataForAnalysis$From_which_region_in_Indiana)

prop.table(mytable) # cell percentages

mytable


## frequency table of personal accent rating and told have an accent

mytable <- table(DataForAnalysis$Personal_distinct_regional_accent_no1_yes7, DataForAnalysis$Personal_distinct_accent_told)

prop.table(mytable) # cell percentages

mytable

## frequency table of personal accent rating and told have an accent

mytable <- table(DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Is_your_region_good_to_learn_neutral_American_accent)

prop.table(mytable) # cell percentages

mytable

## frequency table of home region and regional accent

mytable <- table(DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Which_accent_you_usually_speak)

prop.table(mytable) # cell percentages

mytable

## frequency table of home region and made fun of

mytable <- table(DataForAnalysis$From_which_region_in_Indiana, DataForAnalysis$Someone_has_made_fun_of_your_speech)

prop.table(mytable) # cell percentages

mytable

## frequency table of different accent with strangers and different accent in formal sits

mytable <- table(DataForAnalysis$Accent_shift_between_strangers_and_intimates, DataForAnalysis$Accent_shift_in_formal_vs_informal)

prop.table(mytable) # cell percentages

mytable

## frequency table of religious participation and sports fans

mytable <- table(DataForAnalysis$Religious_participation, DataForAnalysis$sportsFan)

prop.table(mytable) # cell percentages

mytable
