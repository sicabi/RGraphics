################################################################################
################################################################################
###                      SIMPLE HORIZONTAL PERCENTAGE                        ###
###                                BAR CHART                                 ###
################################################################################
################################################################################
# This script plots a simple horizontal bar chart in an external file. 
# It slightly modifies the one created by Thomas Ralhlf in his book 
# "Data Visualization with R" of 2017. 
# Date of last modification: January 14th, 2019
################################################################################
###                      STEP 0. CLEAN THE ENVIRONMENT                       ###
################################################################################
# To avoid any conflicts with other packages, let's aasure ourselves the 
# R environment is clean from objetcs and from certain packages.
# 0.1 Clean environment---------------------------------------------------------
rm(list = ls())
# 0.2 Detach any possible conflicting packages----------------------------------
if ("memisc" %in% (.packages())) {
    detach("package:memisc", unload = TRUE) 
}
if ("foreign" %in% (.packages())) {
    detach("package:foreign", unload = TRUE) 
}
if ("Hmisc" %in% (.packages())) {
    detach("package:Hmisc", unload = TRUE) 
}
if ("haven" %in% (.packages())) {
    detach("package:haven", unload = TRUE) 
}
################################################################################
###             STEP 1. IMPORT FORMATED DATA FROM FILE                       ###
################################################################################
# 1.1 Using the gdata package to import data from an xlsx file------------------
directory <- "./Scripts/myData/"
fileName <- "ipsos.xlsx"
xlsxFile <- paste(directory, fileName, sep = "")
data <-
    gdata::read.xls(xlsxFile, encoding = "latin3")
data <- data[order(data[ , 2]), ] # Order data according to values.
# 1.2 View how the data is arranged.--------------------------------------------
print(data)
# 1.3 Assing the plot labels and numbers to variables.--------------------------
categories <- as.character(data[ , "Country"])
values <- data[ , "Percent"]
################################################################################
###                         STEP 2. PLOT CHART                               ###
################################################################################
# 2.1 Define the destination file and the plot area in centimeters.------------- 
inch <- 2.54
cairo_pdf(filename = paste("PDF_files/", fileName, "_barchart_h.pdf", sep = ""), 
          width = 27.94/inch, height = 21.59/inch, bg = "grey98")
# 2.2 Define the size of inner and outer margins in centimeters.----------------
par(omi = c(2/inch, 2/inch, 2/inch, 1.5/inch), 
    mai = c(2/inch, 4/inch, 2/inch, 1.5/inch), family = "Lato Light", las = 1,
    mgp = c(3, 3, 0))
n_sections <- length(categories)
xlength <- 100
ylength <- n_sections
barwidth <- 0.85
valuespace <- 0.15/barwidth
# 2.3 Plot values without labels.-----------------------------------------------
color1 <- "grey"
bp <- barplot(height = values, width = barwidth, space = valuespace,
              names.arg = FALSE, horiz = TRUE, border = NA, 
              xlim = c(0, xlength), ylim = c(0, ylength), col = color1, 
              axes = FALSE, family = "Lato")
# 2.6.1 See what categories should be highlighted.------------------------------
print(data)
# 2.4 Define the values where we've used the special labels, then -----------
# mark its percentage values and define a different color than the first barplot
category1 <- "Great Britain"
category2 <- "Italy"
values2 <- vector(mode = "numeric", length = n_sections)
index1 <- which(data["Country"] == category1)
index2 <- which(data["Country"] == category2)
values2[index1] <- values[index1]
values2[index2] <- values[index2]
color2 <- rgb(255, 0, 210, maxColorValue = 255)
# 2.5 Plot the second barplot over the first one to higlight--------------------
# selected values.
bp <- barplot(height = values2, width = barwidth, space = valuespace,
              names.arg = FALSE, horiz = TRUE, border = NA, 
              xlim = c(0, xlength), ylim = c(0, ylength), col = color2, 
              axes = FALSE, family = "Lato", add = TRUE)
# 2.6 Mark the labels of each bar. If the values of the labels------------------ 
# match the character strings defined, use a special font.
for (i in 1:length(categories)) {
    if (categories[i] %in% c(category1, category2)) {
        myFont <- "Lato Black"
    } else {
        myFont <- "Lato Light"
    }
    text(labels = categories[i], 
         x = -5, y = bp[i], xpd = TRUE, adj = 1, cex = 0.85, family = myFont)
    text(labels = values[i], 
         x = -1, y = bp[i], xpd = TRUE, adj = 1, cex = 0.85, family = myFont)
}
# 2.7 To recognize difefferent measures of the values, draw --------------------
# a background with rectangles of two light but different colors.
color1 <-  rgb(red = 191, green = 239, blue = 255, alpha =  80, 
               maxColorValue = 255)
color2 <-  rgb(red = 191, green = 239, blue = 255, alpha = 110, 
               maxColorValue = 255)
rect(xleft = 0, ybottom =  -0.1, xright = 20, ytop = ylength*1.02, col = color1, 
     border = NA)
rect(xleft = 20, ybottom = -0.1, xright = 40, ytop = ylength*1.02, col = color2, 
     border = NA)
rect(xleft = 40, ybottom = -0.1, xright = 60, ytop = ylength*1.02, col = color1, 
     border = NA)
rect(xleft = 60, ybottom = -0.1, xright = 80, ytop = ylength*1.02, col = color2, 
     border = NA)
rect(xleft = 80, ybottom = -0.1, xright = 100, ytop = ylength*1.02, 
     col = color1, border = NA)
# 2.8 Draw a line to divide the chart where the mean value is found-------------
arrows(x0 = mean(values), y0 = -0.1, x1 = mean(values), y1 = ylength*1.02, 
       lwd = 1.5, length = 0, xpd = TRUE, col = "skyblue3")
arrows(x0 = mean(values), y0 = -0.1, x1 = mean(values), y1 = -0.3, lwd = 2, 
       length = 0, xpd = TRUE)
arrows(x0 = mean(values), y0 =  ylength*1.02, x1 = mean(values), 
       y1 =  ylength*1.03, lwd = 2, length = 0, xpd = TRUE)
text(labels = paste("Average:", round(mean(values), 1), sep = " "),
     x = mean(values) - 1, y =  ylength*1.03, adj = 1, xpd = TRUE, cex = 0.65, 
     font = 3)
# 2.9 Titling and further labeling----------------------------------------------
chartTitle <- "'I definetely believe in god or a supreme being'"
chartSubtitle <- "was said in 2010 in:"
scaleTag <- "All values in percent"
sourceTag <- "Source: www.ipsos-na.com, Design: Stefan Fichtel"
sourceTag2 <- "Design modified by: Sicabí Cruz"
mtext(text = chartTitle, side = 3, line = 0, adj = 0, cex = 1.2, 
      family = "Lato Black", outer = TRUE, las = 1)
mtext(text = chartSubtitle, side = 3, line = -1, adj = 0, cex = 0.9, 
      family = "Lato Light", outer = TRUE, las = 1)
text(labels = scaleTag, 100, 20.25, adj = 1, xpd = TRUE, cex = 0.65, font = 3)
mtext(text = sourceTag, side = 1, line = -1, adj = 1.0, cex = 0.75, 
      outer = TRUE, font = 3, las = 1)
mtext(text = sourceTag2, side = 1, line = 0, adj = 1.0, cex = 0.75, 
      outer = TRUE, font = 3, las = 1)
dev.off()