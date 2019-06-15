################################################################################
################################################################################
###                      SIMPLE VERTICAL PERCENTAGE                          ###
###                               BAR CHART                                  ###
################################################################################
################################################################################
# This script plots a simple vertical bar chart in an external file. It modifies 
# the one created by Thomas Ralhlf in his book "Data Visualization with R" of
# 2017. Date of last modification: January 13th, 2019
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
cairo_pdf(filename = paste("PDF_files/", fileName, "_barchart_v.pdf", sep = ""), 
          width = 27.94/inch, height = 21.59/inch, bg = "grey98")
# 2.2 Define the size of inner and outer margins in centimeters.----------------
par(omi = c(2/inch, 2/inch, 2/inch, 2/inch), 
    mai = c(2/inch, 2/inch, 2/inch, 2/inch),
    family = "Lato Light", las = 2, mgp = c(3, 1, 0))
# 2.3 Plot values without labels.-----------------------------------------------
n_sections <- length(categories)
xlength <- length(categories) + 0.15
ylength <- 100
barwidth <- 0.85
valuespace <- 0.15/barwidth
bp <- barplot(height = values, width = barwidth, space = valuespace, 
             names.arg = FALSE, horiz = FALSE, border = NA, 
             xlim = c(0, xlength), ylim =  c(0, ylength), axes = FALSE)
# 2.6.1 See what categories should be highlighted.------------------------------
print(data)
# 2.4 Define the values where we've used the special labels, then -----------
# mark its percentage values and define a different color than the first barplot
category1 <- "Great\nBritain"
category2 <- "Italy"
values2 <- vector(mode = "numeric", length = n_sections)
index1 <- which(data["Country"] == category1)
index2 <- which(data["Country"] == category2)
values2[index1] <- values[index1]
values2[index2] <- values[index2]
color2 <- rgb(255, 0, 210, maxColorValue = 255)
# 2.5 Plot the second barplot over the first one to higlight -------------------
# selected values.
bp <- barplot(height = values2, width = barwidth, space = valuespace, 
              names.arg = FALSE, horiz = FALSE, border = NA, 
              xlim = c(0, xlength), ylim = c(0, ylength), col = color2, 
              cex.names = 0.85, axes = FALSE, add = TRUE)
# 2.2. Format string labels.----------------------------------------------------
# We break lines each 2 words in order to adapt the categories' labels to the
# plot and we also capitalize each label.
words <- 1
for (i in 1:n_sections) {
    char_string <- substring(categories[i], 1:nchar(categories[i]), 
                             1:nchar(categories[i]))
    index <- which(char_string == " ")
    if (!length(index) == 0) {
        breaks <- length(index) %/% words
        for (lines in 1:breaks) {
            position <- lines*words
            substr(categories[i], index[position], index[position]) <- "\n"
        }
    }
}
# 2.6 Mark the labels of each bar. If the values of the labels ----------------- 
# match the character strings defined, use a special font.
for (i in 1:length(categories)) {
    if (categories[i] %in% c(category1, category2)) {
        myFont <- "Lato Black"
    } else {
        myFont <- "Lato Light"
    }
    text(labels =  values[i],  x = bp[i], y = -2, cex = 0.75, family = myFont, 
         xpd = TRUE, adj = 0.5)
    text(labels = categories[i], x = bp[i], y = -9, srt = 0, cex = 0.65, 
         family = myFont, xpd = TRUE, adj = 0.5)
}
# 2.7 To recognize difefferent measures of the values, draw --------------------
# a background with rectangles of two light but different colors.
color1 <-  rgb(red = 191, green = 239, blue = 255, alpha =  80, 
               maxColorValue = 255)
color2 <-  rgb(red = 191, green = 239, blue = 255, alpha = 110, 
               maxColorValue = 255)
rect(xleft = 0, ybottom = 0, xright = xlength, ytop = 20, col = color1, 
    border = NA)
rect(xleft = 0, ybottom = 20, xright = xlength, ytop = 40, col = color2, 
     border = NA)
rect(xleft = 0, ybottom = 40, xright = xlength, ytop = 60, col = color1, 
     border = NA)
rect(xleft = 0, ybottom = 60, xright = xlength, ytop = 80, col = color2, 
     border = NA)
rect(xleft = 0, ybottom = 80, xright = xlength, ytop = 100, col = color1, 
     border = NA)
# 2.8 Draw a line to divide the chart where the mean value is found-------------
arrows(x0 = 0, x1 = xlength, y0 = mean(values), y1 = mean(values), 
       lwd = 1.5, length = 0, col = "skyblue3")
arrows(x0 = 0, x1 = -0.2, y0 = mean(values), y1 = mean(values), lwd = 2, 
       length = 0)
arrows(x0 = xlength, y0 = mean(values), x1 = xlength + 0.2, y1 = mean(values), 
       lwd = 2, length = 0)
text(labels = paste("Average:", round(mean(values), 1), sep = " "), 
     x = xlength + (valuespace*6), y = mean(values), adj = 0.5, xpd = TRUE, 
     cex = 0.65, font = 3)
# 2.9 Titling and further labeling.--------------------------------------------
chartTitle <- "'I definetely believe in god or a supreme being'"
chartSubtitle <- "was said in 2010 in:"
scaleTag <- "All values in percent (%)"
sourceTag <- "Source: www.ipsos-na.com, Design: Stefan Fichtel"
sourceTag2 <- "Design modified by: Sicabí Cruz"
mtext(text = c(seq(0, 100, 20)), side = 2, line = 0, at = c(seq(0, 100, 20)))
mtext(text = chartTitle, 
      +side = 3, line = 0, adj = 0, cex = 1.2, family = "Lato Black", 
      outer = TRUE, las = 1)
mtext(text = chartSubtitle, 
      side = 3, line = -1, adj = 0, cex = 0.9, family = "Lato Light", 
      outer = TRUE, las = 1)
text(labels =  scaleTag, 
     x = 15, y = 101, xpd = TRUE, cex = 0.75, font = 3)
mtext(text = sourceTag, 
      side = 1, line = 1, adj = 1.0, cex = 0.65, outer = TRUE, font = 3, 
      las = 1)
mtext(text = sourceTag2,
      side = 1, line = 2, adj = 1.0, cex = 0.65, outer = TRUE, font = 3, 
      las = 1)
dev.off()