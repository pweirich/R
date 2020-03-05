###############################################################################
###############################################################################
##
##  Script for subsetting and categorizing data from Indiana Culture Survey
##
##  This script is for data that I collected with the first distribution of
##  the Indiana Culture Survey (collected mid-September--early-October).
##
##  The script uses data that has already been processed by the script:
##  Orientation[ProcessRawData]
##
##
##  Output: This script generates a file, DataForAnaysis, that is saved with
##          a unique time-stamped file name that can then be analyzed by other
##          scripts. No data should be analyzed that did not pass through
##          this script; therefore, any additional data cleaning or subsetting
##          should be added to this script, or else data should be subset in
##          in later scripts.
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

# Find the most recent version of the output of Orientation[ProcessRawData]
details = file.info(list.files(pattern="ProcessedData"))
details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]
mostRecentVersion = rownames(details)[1]

## Import most recent verstion of the processed data
# (output of Orientation[ProcessRawData])
ProcessedData = read.csv( file=paste0(mostRecentVersion))





# # separate out coordinate and town information for convenience
#
# town.coords <- cbind(ProcessedData$Place_lived_longest, data.coords)
#
# # Define boundaries of map
# myLocation <- c(-88.25, 38.5, -85, 42)
#
# # Download map (see ggmap cheat sheet for more info)
# myMap <- get_map(location=myLocation, source="stamen", maptype="toner", crop=FALSE)
#
# # Generate the map with Respondent locations
# ggmap(myMap)+
#   geom_point(aes(x = lon, y = lat),                         # what columns the data comes from
#              data = town.coords,                            # which data frame the coordinate date are in
#              alpha = .25,                                   # transparency
#              color="darkred",                               # color of points
#              size = 3,                                      # size of points
#              position=position_jitter(w = 0.01, h = 0.01) # jitters points to reduce overlap (depends on the scale, as a percentage)
#             )


## Provenence ##
# Designate Northen, Middle, or Southern based on 'Place lived longest'
# (Based on what criteria? Probably based on a single lattitude.
#  for convenience, just divide the state into even thirds)
#  Furthest South point in Indiana, Point Township  37.77 N
#  Furthest North point, Northern boundary 41.78 N

furthestSouth <- 37.77
furthestNorth <- 41.78

# calculate lattitude boundaries for geographic northern and southern regions
indianaThirds <- (furthestNorth - furthestSouth)/3

southernBoundary <- (furthestSouth + indianaThirds)
northernBoundary <- (furthestNorth - indianaThirds)


# # Generate the map with Respondent locations and geographic boundary lines
# ggmap(myMap)+
#   geom_point(aes(x = lon, y = lat),                         # what columns the data comes from
#              data = town.coords,                            # which data frame the coordinate date are in
#              alpha = .25,                                   # transparency
#              color="darkred",                               # color of points
#              size = 3,                                      # size of points
#              position=position_jitter(w = 0.01, h = 0.01) # jitters points to reduce overlap (depends on the scale, as a percentage)
#   )+
#   geom_hline(yintercept = c(southernBoundary, northernBoundary))

# Subset respondents by geographic boundary regions

ProcessedData$geographicProvenance[ProcessedData$lat >= northernBoundary] <- "Geographic North"
ProcessedData$geographicProvenance[ProcessedData$lat <= southernBoundary] <- "Geographic South"
ProcessedData$geographicProvenance[ProcessedData$lat > southernBoundary & ProcessedData$lat < northernBoundary] <- "Geographic Middle"




## Religion ##

# Designate 'active' participants
# (attends at least once a week; or some other minimal amount)

ProcessedData$religiousActive <- grepl(c("Weekly|Daily"), ProcessedData$Religious_participation)


## Sports ##

# basketball fans
basketballResponses <- cbind.data.frame(ProcessedData$Professional_basketball_chicago,
                         ProcessedData$Professional_basketball_cleveland,
                         ProcessedData$Professional_basketball_indianapolis)

ProcessedData$basketballFan <- rowSums(basketballResponses) > 0L

# baseball fans
baseballResponses <- cbind.data.frame(ProcessedData$Professional_baseball_stl,
                                        ProcessedData$Professional_baseball_chicago,
                                        ProcessedData$Professional_baseball_cincinnati,
                                      ProcessedData$Professional_baseball_cleveland,
                                      ProcessedData$Professional_baseball_detroit)

ProcessedData$baseballFan <- rowSums(baseballResponses) > 0L


# football fans
footballResponses <- cbind.data.frame(ProcessedData$Professional_football_stl,
                                      ProcessedData$Professional_football_chicago,
                                      ProcessedData$Professional_football_detroit,
                                      ProcessedData$Professional_football_cleveland,
                                      ProcessedData$Professional_football_cincinnati,
                                      ProcessedData$Professional_football_indianapolis)

ProcessedData$footballFan <- rowSums(footballResponses) > 0L

# hockey fans

hockeyResponses <- cbind.data.frame(ProcessedData$Professional_hockey_stl,
                                    ProcessedData$Professional_hockey_chicago,
                                    ProcessedData$Professional_hockey_detroit)

ProcessedData$hockeyFan <- rowSums(hockeyResponses) > 0L

# NCAA fans

ncaaResponses <- cbind.data.frame(ProcessedData$NCAA_teams_butler,
                                  ProcessedData$NCAA_teams_depaul,
                                  ProcessedData$NCAA_teams_purdue,
                                  ProcessedData$NCAA_teams_indiana,
                                  ProcessedData$NCAA_teams_kentucky,
                                  ProcessedData$NCAA_teams_notredame,
                                  ProcessedData$NCAA_teams_louisville,
                                  ProcessedData$NCAA_teams_northwestern)

ProcessedData$NCAAfan <- rowSums(ncaaResponses) > 0L


# Designate 'Sports Fans'
# (3 or more teams, from any pro or NCAA team)

sportsResponses <- cbind.data.frame(basketballResponses,
                                    baseballResponses,
                                    footballResponses,
                                    hockeyResponses,
                                    ncaaResponses)

ProcessedData$sportsFan <- rowSums(sportsResponses) > 2L

# Regional Sports Fans (fan of any single professional team from the given region)
# Designante Chicago fans

chicagoResponses <- cbind.data.frame(ProcessedData$Professional_basketball_chicago,
                                     ProcessedData$Professional_baseball_chicago,
                                     ProcessedData$Professional_football_chicago,
                                     ProcessedData$Professional_hockey_chicago)

ProcessedData$chicagoFan <- rowSums(chicagoResponses) > 0L

# Designante Ohio fans

ohioResponses <- cbind.data.frame(ProcessedData$Professional_basketball_cleveland,
                                  ProcessedData$Professional_baseball_cleveland,
                                  ProcessedData$Professional_baseball_cincinnati,
                                  ProcessedData$Professional_football_cleveland,
                                  ProcessedData$Professional_football_cincinnati)

ProcessedData$ohioFan <- rowSums(ohioResponses) > 0L

# Designante Detroit fans

detroitResponses <- cbind.data.frame(ProcessedData$Professional_baseball_detroit,
                                     ProcessedData$Professional_football_detroit,
                                     ProcessedData$Professional_hockey_detroit)

ProcessedData$detroitFan <- rowSums(detroitResponses) > 0L

# Designante STL fans

stlResponses <- cbind.data.frame(ProcessedData$Professional_baseball_stl,
                                 ProcessedData$Professional_football_stl,
                                 ProcessedData$Professional_hockey_stl)

ProcessedData$stlFan <- rowSums(stlResponses) > 0L

# Designate Indianapolis fans

indianapolisResponses <- cbind.data.frame(ProcessedData$Professional_football_indianapolis,
                                          ProcessedData$Professional_basketball_indianapolis)

ProcessedData$IndyFan <- rowSums(indianapolisResponses) > 0L


## Factor Analyses ##

# Factor analysis on Hometown attitudes


#Factor analysis on Regional Accent attitudes



## Spatial Analyses ##

# Use distance from University to describe Rs.

# Use distance from urban centers to predict (regional) Sports Fans

# Use distance from urban centers to predict accent label reponse

# Use distance form lines of latitude (parallels) to predict regional
# affiliation and regional accent labels


#####################################
##                                 ##
## General Demographic Information ##
##                                 ##
#####################################

# Age


# Gender


# Major


# Religion
# (active or not: 0 (not active at all), 1 (less than once a month), 2 (at least once a month))


# Provenence
# (based on hometown (place lived longest before 18))


# University


# Distribution of Provenence by University


# Ancestry
# (European or not)


# Lived outside Indiana



# Lived abroad


# Parents born in Indiana
# (0 neither parent, 1 one parent, 2 both parents)


# Grandparents born in Indiana
# (0 no grandparents, 1 at least one from one side, 2 at least one from both sides)


############################
##                        ##
## Save Data for Analysis ##
##                        ##
############################

# save modified data file as CSV with time and date stamp
now <- Sys.time()
file_name <- paste0("DataForAnalysis_", format(now, "%Y_%m_%d_%H_%M_%S"))
write.csv(ProcessedData, file_name)
print(file_name)

################
##            ##
## Constructs ##
##            ##
################

## Regional Pride

# Like living in where I do
# (-1 no, 0 neut, 1 yes)


# Satisfied
# (This one will be based on the factorial analysis.
#  I'll need to think through this one more.)


# Use binary response to predict the 'like living' and 'satisfied' responses


## Homebody (desire to leave)
# (This one will require some exploration.
#  I should probably treat 'visit' and 'live' responses separately
#  and then decide if it makes sense to treat them on the same scale.)



## Cardinality
# (N, S, MW)



## Breadth of region
# (locally oriented, state oriented, region oriented, nationally oriented)
# (Use these responses to predict regional accent label, attitude, and possession responses)


## Local embeddedness

# Sports Fans

# Religious participation

# Club activities

# Aggregate of local embeddedness
# (Regional sports fan +1,
#  monthly religious participation +1,
#  > 50 percentile club activities +1)


## Accentedness

# Personal accentedness
# (-1 no, 0 neut, +1 yes)


# Linguistic Security
# (predict 'have accent' response from 'told accent' response)

# Accent change

# Made fun of accent


# Regional accentedness
# (This one will require some exploration, starting with the factor analysis.)

# Regional accent label
# predict accent label based on parallels

# predict accent labels based on breadth of region

# predict accent labels based on regional pride
