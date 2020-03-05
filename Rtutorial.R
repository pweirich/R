###################################################
#######                                    ########
####### Exploring R for the first time : ) ########
#######                                    ########
###################################################
#######                                    ########
####### Compiled by: Phillip Weirich       ########
####### Date: Spring 2016                  ########
####### For: L541 Intro. to Phonetics      ########
####### Contact: pweirich@indiana.edu      ########
#######                                    ########
###################################################

# Welcome to R!  In this tutorial you will get to explore
# the basics of the R programing language.  We'll learn that
# R can be used as a calculator, that it can store information 
# in variables, how to load data files, and how to plot
# xy charts and make boxplots.  


###### R can calculate stuff #######

# Let's see how R can be used as a calculator. Below is 
# a simple math problem.  As you can see, the numbers are
# just sitting there.  How do you tell R what to do?
# You'll need to tell R to run the script.  If you are 
# using RStudio, highlight the equation and click "Run"
# at the top of this window.

1+1

# You'll notice that the following text appeared in the Console
# window, probably right below this window:

# > 1+1
# [1] 2

# The first line `echos' the command
# The second line is the output of the command
# You can ignore the [1] for now, and you will find the 
# output of the equation next to it.

# Run the following commands and see what happens.
# Also, feel free to try your own commands

13 + 7

10 - 3

12*12

48/8

5^2

# At this point, I want to mention that the pound symbol --> #
# can be used to include comments in your code.
# When R comes to a pound symbol, it stops reading that line.
# In the scripts we'll look at later, you'll see how comments
# can be very helpful in organizing scripts and telling the user
# what the code does.

###### Variables #######

# Now run the next three lines and see what happens

a = 2
b = 5
a + b 

# When you ran the code, you told R to store the numbers
# as variables.  Variables are very important in R.  Let's 
# see more about what variables can store.

a <- 7
b <- 3
a + b 

# The three lines above show you two things.  First, you 
# use the 'less than' sign plus a dash instead of the
# equals sign.  Second, you can reassign the values 
# associated with a variable.

# Variables don't just have to be single numbers, though.

a = c(1, 2, 3, 4, 5)

# What we've done here is create a 'vector' called a
# Look in the top right window of RStudio and you'll
# See a listing of Values for variables.  You'll notice
# that a has the value 'num [1:5] 1 2 3 4 5'
# This means that a is a series of numbers occupying
# slots 1-5 in a vector and that the contents of
# those slots are 1 2 3 4 and 5.

# What can we do with a vector?

b = 2
a^b

# Running the above two lines sets b to 2 and 
# then asks R to raise each value in a to the 2nd power

# You can also easily create a variable that contains 
# the output of a formula.

c = a^b
show(c)

# You'll notice that just declaring the value of c
# doesn't cause R to output the results of the formula
# in the Console window.  You can see the contents of c
# in the Values window, but in order to see the contents of 
# c in the console, you have to ask are to show you c

# You can also store text in a variable

myText = "Hello World!"
show(myText)


###### Computations #######


randomData = rnorm(n=200, mean=5, sd=10)

mean(randomData)
median(randomData)
sd(randomData)

###### Functions #######

tempConvert = function( f=72 ) {
  c = (f-32)*(5/9)
  return(c)
}


tempConvert()
tempConvert(98.6)
tempConvert(c(32, 72, 98.6, 212))

##############################
###### The Main Event! #######
######     Plotting    #######
##############################

######   Vowel Plots   #######

# Before we get too excited, let's do something basic
# Let's create two vectors

F1 = rnorm(n=200, mean=660, sd=100)
F2 = rnorm(n=200, mean=1720, sd=200)

# You can make histograms to check for normality
hist(F1)
hist(F2)

# You can plot F1 against F2
plot(F2,F1)

# There are a few things that are missing from this plot
# Let's learn how to add them.

plot(F2, F1, 
     main="MainTitle",   # The title of the plot
     xlab="F2 (Hz)",     # Label for x-axis
     ylab="F1 (Hz)",     # Label for x-axis
     xlim=c(3000, 600),  # Range of the x-axis. Also, inverts the axis values
     ylim=c(1000, 200),  # Range of the y-axis. Values also inverted
     col="blue",         # color of the points
     pch=1               # shape of the points (1=cirlces, 2=triangles, 3=pluses, etc. )
)
# We can also add more points to the same plot
moreF1 = rnorm(n=200, mean=400, sd=75)
moreF2 = rnorm(n=200, mean=2520, sd=200)
anotherF1 = rnorm(n=200, mean=400, sd=75)
anotherF2 = rnorm(n=200, mean=1200, sd=200)

points(moreF2, moreF1, pch=2, col="red")
points(anotherF2, anotherF1, pch=3, col="green")

# Finally, you can add a legend, too. 
# Go here to find the UTF-8 codes for symbols: 
#http://www.phon.ucl.ac.uk/home/wells/ipa-unicode.htm#numbers

legend("top", horiz=TRUE, c("i", "\u00E6", "u"), pch=c(1, 2, 3), 
       col=c("red", "blue", "green"), x.intersp=0.8)

# it turns out that this legend covers some of the data,
# so we can move it. Also, the legend doesn't show the 
# correct symbols Try this version instead.

legend("bottomright", horiz=FALSE, c("i", "\u00E6", "u"), pch=c(2, 1, 3), 
       col=c("red", "blue", "green"), x.intersp=0.8)

# Below is the code presented in a way that makes is easier to play around with

F1 = rnorm(n=200, mean=660, sd=100)
F2 = rnorm(n=200, mean=1720, sd=200)
moreF1 = rnorm(n=200, mean=400, sd=75)
moreF2 = rnorm(n=200, mean=2520, sd=200)
anotherF1 = rnorm(n=200, mean=400, sd=75)
anotherF2 = rnorm(n=200, mean=1200, sd=200)

plot(F2, F1, 
     main="MainTitle",   # The title of the plot
     xlab="F2 (Hz)",     # Label for x-axis
     ylab="F1 (Hz)",     # Label for x-axis
     xlim=c(3000, 600),  # Range of the x-axis. Also, inverts the axis values
     ylim=c(1000, 200),  # Range of the y-axis. Values also inverted
     col="blue",         # color of the points
     pch=1               # shape of the points (1=cirlces, 2=triangles, 3=pluses, etc. )
)

points(moreF2, moreF1, pch=2, col="red")
points(anotherF2, anotherF1, pch=3, col="green")

legend("bottomright", horiz=FALSE, c("i", "\u00E6", "u"), pch=c(2, 1, 3), 
       col=c("red", "blue", "green"), x.intersp=0.8)



######   Loading your own data   #######

# As fun as generating fake data is, you probably want to plot your own vowels.
# First, you'll need to load your data.  
# To do this, click Session > Set Working Directory > Choose Directory...
# Then go to the directory that has your data and click Open
# I've provided some data for you to use on the lab webpage

theData = read.csv( file="T439ForAnalysis.csv") # This loads the data matrix. It has to be a .csv file
attach(theData) # This lets us reference columns in the data by their lablels

# There are a lot of measurements in this data set. Below is the list of measurements.
# We'll only plot a few, but you are welcome to include more.
# firstFormants  = data.frame (I1, E1, AE1, O1, OH1, UH1, U1, IYC1, EYC1, UWC1, OWC1)
# secondFormants = data.frame (I2, E2, AE2, O2, OH2, UH2, U2, IYC2, EYC2, UWC2, OWC2)

plot(theData$I2, theData$I1, 
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


legend("bottomright", horiz=FALSE, c("i", "\u00E6", "\u0251", "u"), pch=c(1, 2, 4, 3), 
       col=c("blue", "red", "black", "green"), x.intersp=0.8)

# The axes for this data are too big.  Adjust them so you get a better looking plot.


# Here is a different example just showing the means
# UWC is missing some values, so first we have to remove the blank data, represented as NA

UWC1 <- UWC1[!is.na(UWC1)]
UWC2 <- UWC2[!is.na(UWC2)]

plot(mean(I2), mean(I1), 
     main="North American Vowels, Mean Values",   # The title of the plot
     xlab="F2 (Hz)",     # Label for x-axis
     ylab="F1 (Hz)",     # Label for x-axis
     xlim=c(3000, 600),  # Range of the x-axis. Also, inverts the axis values
     ylim=c(1000, 200),  # Range of the y-axis. Values also inverted
     col="blue",         # color of the points
     pch="i"               # shape of the points (1=cirlces, 2=triangles, 3=pluses, etc. )
)

points(mean(AE2), mean(AE1), pch="\u00E6", col="red")
points(mean(OH2), mean(OH1), pch="\u0251", col="black")
points(mean(UWC2), mean(UWC1), pch="u", col="green")


######   Box Plots   #######

# Boxplot of VOT by Consonant

# First, let's generate some (kind of) plausible data based on Lisker and Abramson (1965)
b = rnorm(n=200, mean=-101, sd=10)

p = rnorm(n=200, mean=58, sd=10)

d = rnorm(n=200, mean=-102, sd=10)

t = rnorm(n=200, mean=70, sd=10)

g = rnorm(n=200, mean=-88, sd=10)

k = rnorm(n=200, mean=80, sd=10)

# Then we need to put these values into a data frame

consonants = c("b", "p", "d", "t", "g", "k")
VOTdata = data.frame(cbind(b,p,d,t,g,k))

par(mar =  c(3, 4, 4, 2))               # Adjusts the bottom, left, top, and right margins, respectively
boxplot(VOTdata,                        # This is the data file. You would have loaded your own data instead of generating with R.
        main="Some Funky VOT Data",     # Plot title
        xlab="Consonant",               # x-axis label
        ylab="VOT (ms)",                # y-axis label
        col=c("blue", "green")          # Color of the boxes. I 
        )

legend("left", horiz=FALSE, c("voiced", "voiceless"), pch=15, 
       col=c("blue", "green"), x.intersp=0.8)

# This plot looks okay, but the legend is rather distracting.  Here's how you
# can place the legend outside of the plot.
par(mar =  c(3, 4, 4, 2),              # Sets the margins of the plot
    oma = c(2, 1, 1, 1)                # Sets the outside margins
    ) 
 

boxplot(VOTdata,                        # This is the data file. You would have loaded your own data instead of generating with R.
        main="Some Funky VOT Data",     # Plot title
        xlab="Consonant",               # x-axis label
        ylab="VOT (ms)",                # y-axis label
        col=c("blue", "green")          # Color of the boxes. I 
)

par(fig = c(0, 1, 0, 1),                # This part creates a new, "invisible" plot that
    oma = c(0, 0, 0, 0),                # We can then add the legend to
    mar = c(0, 0, 0, 0), 
    new = TRUE)                         # This keeps our original plot 
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

legend("bottomleft", horiz=FALSE, c("voiced", "voiceless"), pch=15, 
       col=c("blue", "green"), x.intersp=0.8)


##############################
######    The End !    #######   <-- Well, not really.  There is a lot of other stuff you can do.
######                 #######       As your projects progress, we can talk about how to create
##############################       appropriate figures.

