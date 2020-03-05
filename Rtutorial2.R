###################################################
#######                                    ########
#######        More fun with R!!!          ########
#######                                    ########
###################################################
#######                                    ########
####### Compiled by: Phillip Weirich       ########
####### Date: Spring 2016                  ########
####### For: L541 Intro. to Phonetics      ########
####### Contact: pweirich@indiana.edu      ########
#######                                    ########
###################################################

# In in this installment we'll review some things
# from the first tutorial and build on those basic
# pieces.

###### Getting some help ###### 
# To get help with a function, just type:

?plot     # gives info about the "plot" function
?boxplot  # gives infor about the "boxplot" function

# You can type ?+anyFunction to get help with that function

# Also try:

?par

# This gives you a list of a lot of the graphical parameters
# you can adjust.  Explore these options by including them in
# your plot code and changing their values.  You'll also find
# some example code at the bottom of the help document.

###### Review of vectors, variables, and functions ######



###### Introduction to dataframes and subsetting ######
# There are several kinds of objects available in R,
# including vectors, factors, and dataframes.

# Vectors can be either numerical or logical (i.e. TRUE/FALSE).
# Vectors have a mode (numberical or logical) and a length.

numericalVector = c(0,45, 37, 18)
logicalVector = c(T,F, F, F, T, T, F, T)

# Look at these variables in Values window. See how they are different?

# Factors are similar to vectors in that they have a mode and length,
# but they also have levels.  Factors can be numbers or characters.

aFactor = factor(1:3)
anotherFactor = factor(1:3, levels=1:5)

alphaFactor = factor(1:3, labels=c("A", "B", "C"))
show(alphaFactor)

excludedFactor = factor(1:5, exclude=4)
show(excludedFactor)

colorFactor = factor(1:3, labels=c("blue", "green", "red"))
colorFactor[1]

# A dataframe is just a collection of vectors and factors
# Load and attach the data file from the first tutorial
# Make sure you have set the correct Working Directory
# Session > Set Working Directory > Choose Directory...
# All you have to do is select the directory (aka: folder)
# You load the data with the following command in the script.

theData = read.csv( file="T439ForAnalysis.csv") 
attach(theData) # This lets us reference columns in the data by their lablels

## Now, let's select some data!

theData[2]
theData$GENDER

# What is different about the two commands?  Are they showing the same data?

class(theData)
class(theData[2])
class(theData$GENDER)

sapply(theData, class)

# This is a lot of data; let's make it smaller.

smallerData = theData[1:50,]  # first 50 observations 
# (why is there a comma? Hint: delete it and see what happens.)

# Using what you learned about the comma, 
# make the data set even smaller (call it "evenSmallerData")





## Let's learn something about the data
summary(smallerData)

# What did you learn?
# Now, learn something specific

levels(GENDER)
levels(smallerData[2])        # Do these three commands do the same thing?
levels(smallerData$GENDER)    # Why not?

summary(I1)                   # What is the mean?
mean(smallerData$I1)          # Why is the mean different?
length(I1)                    # Does this give you a hint?
length(smallerData$I1)        # So make sure you are querrying the right data : )

## Let's subset even further

# based on variable values
femaleData = smallerData[ which(smallerData$GENDER=='F'), ]
maleData = smallerData[ which(smallerData$GENDER=='M'), ]




###### Some practical uses of subsetting for data analysis ######

## Now that you can subset data like a pro (oh, there are lots of other 
# ways to subset data, by the way) try to plot male and female vowel spaces
# on the same plot.  For extra fun, try to figure out how to plot the mean of
# all the vowels for males and all the vowels for females.  I don't mean the 
# means of each vowel, I mean ALL of the vowels, so it would be one point somewhere
# in the middle of the vowel space.

plot(femaleData$I2, femaleData$I1, 
     main="North American Vowels",   # The title of the plot
     xlab="F2 (Hz)",     # Label for x-axis
     ylab="F1 (Hz)",     # Label for x-axis
     xlim=c(3000, 600),  # Range of the x-axis. Also, inverts the axis values
     ylim=c(1000, 200),  # Range of the y-axis. Values also inverted
     col="blue",         # color of the points
     pch=1               # shape of the points (1=cirlces, 2=triangles, 3=pluses, etc. )
)

points(AE2, AE1, pch=2, col="red")
points(OH2, OH1, pch=4, col="black")
points(UWC2, UWC1, pch=3, col="green")


plot(maleData$I2, maleData$I1, 
     main="North American Vowels",   # The title of the plot
     xlab="F2 (Hz)",     # Label for x-axis
     ylab="F1 (Hz)",     # Label for x-axis
     xlim=c(3000, 600),  # Range of the x-axis. Also, inverts the axis values
     ylim=c(1000, 200),  # Range of the y-axis. Values also inverted
     col="blue",         # color of the points
     pch=1               # shape of the points (1=cirlces, 2=triangles, 3=pluses, etc. )
)

points(AE2, AE1, pch=2, col="red")
points(OH2, OH1, pch=4, col="black")
points(UWC2, UWC1, pch=3, col="green")
