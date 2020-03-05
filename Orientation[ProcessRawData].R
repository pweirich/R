###############################################################################
###############################################################################
##
##  Script for processing raw data from Indiana Culture Survey
##
##  This script takes data that I collected with the first distribution of 
##  the Indiana Culture Survey (collected mid-September--early-October)
##  and cleans it up for basic summarization of the results.
##
##  It also provides a basic summary of the results.
##
##
##  Further processing may be needed for certain specific kinds of data,
##  for example, the language beliefs/attitudes likert scales.
##
##  This script does not add any information to the data.
##  Update: Actually, I have now added a part that gets coordinate information
##          for respondent's hometowns.
##
###############################################################################
#
#  Written by: Phillip Weirich
#  Email: pweirich@indiana.edu
#  Date: 10/5/2016
#
###############################################################################
###############################################################################

## Set working directory
setwd("~/Google Drive/Indiana/Projects/Orientation Survey/Data")

## Import data
# data = read.csv( file="Indiana Language and Culture Survey 09_28_2016.csv")
data = read.csv( file="Indiana Language and Culture Survey_October 20, 2016_13.31.csv")

################################
## Exclude irrelevant columns ##
################################

# Include only relevant ID and data columns
data <- data[c(7:9,17:36,38:56,58:72,74:84)]

# Save informative column labels for later (they will get deleted in the subsetting process)
#  (This didn't end up working) columnLabels <- as.data.frame(data[1,])

# Include only people who finished the survey
data <- subset (data, data$finished =='True')

# Exclude people who didn't fill in the first five questions (the assumption is that
# people who didn't answer these questions didn't actually take the they survey, 
# even though they "finished")
data <- data[!apply(data[,4:9] == "", 1, all),]

# Exclude pilot subjects (everyone who completed the survey on or before 2016-09-06)
data$recordedDate <- as.POSIXct(data$recordedDate, format="%Y-%m-%d %H:%M:%S")
data <- subset(data, data$recordedDate >= as.POSIXct('2016-09-06 23:59:59', format="%Y-%m-%d %H:%M:%S"))

# Exclude people older than 25
data <- subset(data, data$QID19 != '25 or older')

# Include only people who attend IUB or IUSB
#data <- subset (data, data$QID21 =='IU Bloomington' | data$QID21 =='IU Southbend')


# return original column labels from Qualtrics to the top of the data
# (but not as actual column names because I'm adding simpler ones next)
#  (This didn't end up working) rbind(data.frame(columnLabels), data)

######################################
## Add descriptive names to columns ##
######################################

colnames(data) <- c("finished",
                    "recordedDate",
                    "X_recordId",
                    "Like_living_in_hometown_gradient",
                    "Satisfaction_with_hometown_family",
                    "Satisfaction_with_hometown_friends",
                    "Satisfaction_with_hometown_social_life",
                    "Satisfaction_with_hometown_economy",
                    "Satisfaction_with_hometown_job_prospects",
                    "Satisfaction_with_hometown_education",
                    "Satisfaction_with_hometown_weather",
                    "Satisfaction_with_hometown_food",
                    "Satisfaction_with_hometown_safety",
                    "Like_living_in_hometown_binary",
                    "Likelihood_of_moving_outside_of_hometown",
                    "Likelihood_of_moving_outside_of_Indiana",
                    "Likelihood_of_moving_outside_of_USA",
                    "Regions_to_visit",
                    "Regions_to_live",
                    "Has_passport",
                    "From_which_region_in_Indiana",
                    "From_which_region_in_Indiana_other",
                    "Breadth_of_region",
                    "Personal_distinct_regional_accent_no1_yes7",
                    "Personal_distinct_accent_told",
                    "Beliefs_of_speech_in_your_region_fast1_slow7",
                    "Beliefs_of_speech_in_your_region_polite1_rude7",
                    "Beliefs_of_speech_in_your_region_down_to_earth1_snobbish7",
                    "Beliefs_of_speech_in_your_region_educated1_uneducated7",
                    "Beliefs_of_speech_in_your_region_normal1_abnormal7",
                    "Beliefs_of_speech_in_your_region_smart1_dumb7",
                    "Beliefs_of_speech_in_your_region_formal1_casual7",
                    "Beliefs_of_speech_in_your_region_goodEnglish1_badEnglish7",
                    "Beliefs_of_speech_in_your_region_friendly1_unfriendly7",
                    "Is_your_region_good_to_learn_neutral_American_accent",
                    "Which_accent_you_usually_speak",
                    "Situations_you_use_neutral_accent",
                    "Someone_has_made_fun_of_your_speech",
                    "Accent_shift_between_strangers_and_intimates",
                    "Accent_shift_in_formal_vs_informal",
                    "Age",
                    "Gender",
                    "Current_university",
                    "Year_in_school",
                    "Major",
                    "Ethnicity",
                    "Ancestry",
                    "Ancestry_other",
                    "Birthplace",
                    "Place_lived_longest",
                    "Primary_residence_ever_outside_Indiana",
                    "Lived_outside_of_USA",
                    "Religious_participation",
                    "Religious_tradition",
                    "Religious_tradition_protestant_denomination",
                    "Religious_tradition_otherChristian_denomination",
                    "Religious_tradition_other",
                    "Club_participation",
                    "Professional_basketball",
                    "Professional_baseball",
                    "Professional_hockey",
                    "Professional_football",
                    "NCAA_teams",
                    "Raised_by",
                    "Mother_grew_up_state",
                    "Mother_family_in_Indiana",
                    "Father_grew_up_state",
                    "Father_family_in_Indiana"
                    )

###########################################
## Split columns with mulitple responses ##
###########################################

## Regions to visit

# create new vectors that store Boolean values for presence
# of a relevant string in relevant columns

Regions_to_visit_Indiana <- grepl("Indiana", data$Regions_to_visit) 
Regions_to_visit_state <- grepl("state", data$Regions_to_visit) 
Regions_to_visit_country <- grepl("country", data$Regions_to_visit) 

# combine new vectors into a data frame
Regions_to_visit_boolean <- data.frame(
  Regions_to_visit_Indiana,
  Regions_to_visit_state,
  Regions_to_visit_country
)

# add new data frame to the main data frame
data <- cbind(data, Regions_to_visit_boolean)


## Regions to live

# create new vectors that store Boolean values for presence
# of a relevant string in relevant columns

Regions_to_live_Indiana <- grepl("Indiana", data$Regions_to_live) 
Regions_to_live_state <- grepl("state", data$Regions_to_live) 
Regions_to_live_country <- grepl("country", data$Regions_to_live) 

# combine new vectors into a data frame
Regions_to_live_boolean <- data.frame(
  Regions_to_live_Indiana,
  Regions_to_live_state,
  Regions_to_live_country
)

# add new data frame to the main data frame
data <- cbind(data, Regions_to_live_boolean)

## Neutral accent situations

# create new vectors that store Boolean values for presence
# of a relevant string in relevant columns

Situations_you_use_neutral_accent_parents <- grepl("Parents", data$Situations_you_use_neutral_accent) 
Situations_you_use_neutral_accent_teachers <- grepl("Teachers", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_boss <- grepl("Boss", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_church <- grepl("worship", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_job <- grepl("Job", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_siblings <- grepl("Siblings", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_spouse <- grepl("Spouse", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_friends <- grepl("Friends", data$Situations_you_use_neutral_accent)
Situations_you_use_neutral_accent_children <- grepl("Children", data$Situations_you_use_neutral_accent)

# combine new vectors into a data frame
Situations_you_use_neutral_accent_boolean <- data.frame(
  Situations_you_use_neutral_accent_parents,
  Situations_you_use_neutral_accent_teachers,
  Situations_you_use_neutral_accent_boss,
  Situations_you_use_neutral_accent_church,
  Situations_you_use_neutral_accent_job,
  Situations_you_use_neutral_accent_siblings,
  Situations_you_use_neutral_accent_spouse,
  Situations_you_use_neutral_accent_friends,
  Situations_you_use_neutral_accent_children
)

# add new data frame to the main data frame
data <- cbind(data, Situations_you_use_neutral_accent_boolean)

## Sports

# Basketball

# create new vectors that store Boolean values for presence
# of a relevant string in relevant columns

Professional_basketball_chicago <- grepl("Chicago", data$Professional_basketball) 
Professional_basketball_cleveland <- grepl("Cleveland", data$Professional_basketball) 
Professional_basketball_indianapolis <- grepl("Indianapolis", data$Professional_basketball) 

# combine new vectors into a data frame
Professional_basketball_boolean <- data.frame(
  Professional_basketball_chicago,
  Professional_basketball_cleveland,
  Professional_basketball_indianapolis
)

# Baseball

Professional_baseball_chicago <- grepl("Chicago", data$Professional_baseball) 
Professional_baseball_cincinnati <- grepl("Cincinnati", data$Professional_baseball) 
Professional_baseball_cleveland <- grepl("Cleveland", data$Professional_baseball) 
Professional_baseball_detroit <- grepl("Detroit", data$Professional_baseball) 
Professional_baseball_stl <- grepl("Louis", data$Professional_baseball) 

# combine new vectors into a data frame
Professional_baseball_boolean <- data.frame(
  Professional_baseball_chicago,
  Professional_baseball_cincinnati,
  Professional_baseball_cleveland,
  Professional_baseball_detroit,
  Professional_baseball_stl
)

# Hockey

Professional_hockey_chicago <- grepl("Chicago", data$Professional_hockey) 
Professional_hockey_detroit <- grepl("Detroit", data$Professional_hockey) 
Professional_hockey_stl <- grepl("Louis", data$Professional_hockey) 

# combine new vectors into a data frame
Professional_hockey_boolean <- data.frame(
  Professional_hockey_chicago,
  Professional_hockey_detroit,
  Professional_hockey_stl
)

# Football

Professional_football_chicago <- grepl("Chicago", data$Professional_football) 
Professional_football_cincinnati <- grepl("Cincinnati", data$Professional_football) 
Professional_football_cleveland <- grepl("Cleveland", data$Professional_football)  
Professional_football_detroit <- grepl("Detroit", data$Professional_football) 
Professional_football_indianapolis <- grepl("Indianapolis", data$Professional_football) 
Professional_football_stl <- grepl("Louis", data$Professional_football)  

# combine new vectors into a data frame
Professional_football_boolean <- data.frame(
  Professional_football_chicago,
  Professional_football_cincinnati,
  Professional_football_cleveland,
  Professional_football_detroit,
  Professional_football_indianapolis,
  Professional_football_stl
)

# NCAA

NCAA_teams_butler <- grepl("Butler", data$NCAA_teams) 
NCAA_teams_depaul <- grepl("DePaul", data$NCAA_teams)
NCAA_teams_purdue <- grepl("Purdue", data$NCAA_teams) 
NCAA_teams_indiana <- grepl("Indiana", data$NCAA_teams)
NCAA_teams_northwestern <- grepl("Northwestern", data$NCAA_teams) 
NCAA_teams_notredame <- grepl("Notre", data$NCAA_teams)
NCAA_teams_kentucky <- grepl("Kentucky", data$NCAA_teams) 
NCAA_teams_louisville <- grepl("Louisville", data$NCAA_teams)

# combine new vectors into a data frame
NCAA_boolean <- data.frame(
  NCAA_teams_butler,
  NCAA_teams_depaul,
  NCAA_teams_purdue,
  NCAA_teams_indiana,
  NCAA_teams_northwestern,
  NCAA_teams_notredame,
  NCAA_teams_kentucky,
  NCAA_teams_louisville
)

# combine all sports data frames

sports_boolean <- cbind(
  Professional_basketball_boolean, 
  Professional_baseball_boolean,
  Professional_hockey_boolean,
  Professional_football_boolean,
  NCAA_boolean
  )

# add new data frame to the main data frame
data <- cbind(data, sports_boolean)


########################################################
##                                                    ##
## Convert Satisfaction responses to numerical values ##
##                                                    ##
########################################################

cols <- c(4:13, 15:17)
data[,cols] <- ifelse(data[,cols] == "Extremely dissatisfied"|data[,cols] == "Dislike a great deal"|data[,cols] == "Extremely unlikely", 1,
                   ifelse(data[,cols] == "Somewhat dissatisfied"|data[,cols] == "Dislike somewhat"|data[,cols] == "Somewhat unlikely", 2,
                          ifelse(data[,cols] == "Neither satisfied nor dissatisfied"|data[,cols] == "Neither like nor dislike"|data[,cols] == "Neither likely nor unlikely", 3,
                                 ifelse(data[,cols] == "Somewhat satisfied"|data[,cols] == "Like somewhat"|data[,cols] == "Somewhat likely", 4,
                                        ifelse(data[,cols] == "Extremely satisfied"|data[,cols] == "Like a great deal"|data[,cols] == "Extremely likely", 5,
                          99)))))

#############################################
##                                         ##
## Add coordinate information on hometowns ##
##                                         ##
#############################################

# Load required packages
# install.packages("ggmap")
library(ggmap)

# Grab coordinates from Google Maps, treat the Place_lived_longest column as a character vector
data.coords <- geocode(as.character(data$Place_lived_longest))


# Add coordinate information to main data file
data <- cbind(data, data.coords)


#######################################
## Relabel responses with long names ##
#######################################


library(plyr)
data$From_which_region_in_Indiana <- revalue(data$From_which_region_in_Indiana, c("Other (please specify)"="Other"))



###################
##               ##
## Save New File ##
##               ##
###################


# save modified data file as CSV with time and date stamp
now <- Sys.time()
file_name <- paste0("ProcessedData_", format(now, "%Y_%m_%d_%H_%M_%S"))
write.csv(data, file_name)
print(file_name)




