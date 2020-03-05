############################################
############################################
#
# This is a silly script where I was
# practicing creating plots from scratch
# so I could better understand the functions
# and problems when customizing plots.
#
# Author: Phillip Weirich
# Date: 5 March 2018
#
############################################
############################################

event=c("cookies",
        "surprise cookies",
        "crying",
        "irate \nyelling",
        "laughing",
        "school \nmascot",
        "backhanded \ncompliment")
excitement=c(8,3,5,9,5,6,2)
confusion=c(2,6,6,9.5,4,9,8)

df <- data.frame(confusion,excitement)
rownames(df) <- paste0(event)

df = rbind(df, "receive email \nfrom my advisor" = c(1, 2))  # adding another observation
df = rbind(df, "accidentally spend \nthree hours on YouTube" = c(4, 1))
df = rbind(df, "ask someone \nto be quiet \non silent floor" = c(7, 7))
df = rbind(df, "see the book I need \nin someone’s \ncarrel" = c(8, 9))

plot( df$excitement~df$confusion, xlim = c(0, 10), xlab = 'Confusion', ylim = c(0,10),
     ylab = 'Excitement', main = 'How much I want things to \n happen in the library',
     data = df,pch=20)
abline(0,1,lty=3)
rect(2.5, 2.5, 7.5, 7.5,lty=5)
polygon(x=c(2.5,2.5,7.5), y=c(2.5,7.5,7.5), col="gray",border = NA)
polygon(x=c(0,2.5,7.5,7.5,10,10),y=c(0,2.5,2.5,7.5,10,0),col="gray",border=NA)

points(df$excitement~df$confusion, pch=20)
with(df, text(df$excitement~df$confusion, labels = row.names(df), pos = 3))

legend("topleft", inset=.02,
       c("Things I want","Things I don't want"),
       fill=c("white","gray"), horiz=0, cex=0.8)
