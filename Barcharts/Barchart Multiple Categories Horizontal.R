################################################################################
################################################################################
###                      SIMPLE HORIZONTAL MULTIPLE-RESPONSE                 ###
###                                BAR CHART                                 ###
################################################################################
################################################################################
# This script plots a simple vertical bar chart for multiple categories or
# responses in an external file. Specifically, it works with survey data and 
# it allows the user to compare a group of questions and their answers.
# It modifies the code created by Thomas Ralhlf in his book 
# "Data Visualization with R" of 2017, in order to make it easier and clearer 
# to work with survey data from any source.
# Date of last modification: January 18th, 2019
################################################################################
###                      STEP 0. CLEAN THE ENVIRONMENT                       ###
################################################################################
# To avoid any conflicts with other packages, let's aasure ourselves the--------
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
###                      STEP 1. IMPORT DATA FROM FILE                       ###
################################################################################
# 1.1 Load the memisc package. This package is the fastest and most.------------  
# space-efficient for working with SPSS data. It also allows you to keep the 
# labels from imported data in order to use it in the chart labelling later on.
library("memisc")
# 1.2 Unzip the downloaded survey file.-----------------------------------------
# We assume it was manually downloaded to the directory "./SAV_files/..." 
# which you should create in your project directory or will set as your working 
# directory.
zipFile <- "./SAV_files/EVS/datasets/EVS_80-08.zip"
exDir <- "./SAV_files/EVS/datasets/"
unzip(zipfile = zipFile, exdir = exDir, list = TRUE)
# 1.3 Look up for the file we want to extract-----------------------------------
datasetName <- "ZA4800_v4-0-0.sav"
unzip(zipfile = zipFile, exdir = exDir, list = FALSE, files = datasetName) 
# 1.4 Read the data into R and find the relevant variables for our analysis-----
# Specifically, we would like to know what is measuring each variable from our
# imported data set in order to select the relevan variables.
data <- spss.system.file(paste(exDir, datasetName, sep = ""))
description(data)
year <- labels(Table(data[["year"]])[1])
# 1.5 Subset relevant variables for analysis.-----------------------------------
# Once we have already reviewed the variables and what they are 
# measuring, we can subset the relevant survey questions into our data. 
# These might include, for example, the year, country, 
# thematic questions as well as socio demographic 
# questions such as a person's sex or age (v240).
data <- subset(data, select = c(country, v159, v160, v161, v162, v163, v164, 
                                v165))
# 1.6 Retrieve the country names.-----------------------------------------------
# To know which ones were surveyed in this wave/dataset.
countries <- names((table(data$country)))
countries # Display the countries available 
# 1.7 Select a country and subset the data using the selected country.----------
# Once we have subsetted the data using the country we choose, we drop it from
# our dataset to avoid To keep space in the hard disk, we delete the unziped 
# file.
selectedCountry <- "Germany"
data <- subset(data, country == selectedCountry)
data$country <- NULL 
file.remove(paste(exDir, datasetName, sep = ""))
selectedCountry <- "Alemania"
################################################################################
###                      STEP 2. FORMAT DATA FOR PLOT                        ###
################################################################################
# 2.1 Define the number of categories according to questions subsetted.---------
barLabels <- unlist(annotation(data), use.names = TRUE)
varLabels <- names(barLabels)
varLabels <- gsub(pattern = ".description", replacement = "", x = varLabels)
n_sections <- ncol(data)
# 2.2. Format string labels.----------------------------------------------------
# If the number of characters from a given label section is too long, 
# (bigger than 40 characters, including white spaces), we break lines before 
# the end of the sentence. The break position is defined after the first white 
# space found after the 60th character. Also, capitalize first letter in case 
# it is not properly made.
for (i in 1:n_sections) {
    if (nchar(barLabels[i]) > 40) {
        char_string <- substring(barLabels[i], 1:nchar(barLabels[i]), 
                                 1:nchar(barLabels[i]))
        index <- which(char_string == " ")
        position <- index > 40
        position <- length(position) - 
            ifelse(is.na(table(position)["TRUE"]), 0, table(position)["TRUE"])
        substr(barLabels[i],index[position], index[position]) <- "\n"
    }
}
barLabels <- Hmisc::capitalize(barLabels[length(barLabels):1])
# 2.3 Calculate the values for each category.-----------------------------------
# First, we create an empty data frame with a variable for each kind of response
# we are interested in to show in the chart. 
# 2.3.1 Look for the relevant categories ---------------------------------------
Table(data[[1]], include.missings = TRUE)
# 2.3.2 Aggregate the values for each category we've chosen.--------------------
fData <- data.frame(variables = varLabels, 
                    agree = vector(length = length(varLabels), 
                                   mode = "numeric"),
                    `disagree` = vector(length = length(varLabels), 
                                        mode = "numeric"),
                    `don't know-no answer` = vector(length = length(varLabels), 
                                                    mode = "numeric"))
for (i in 1:n_sections) {
    x <- Table(data[[i]])["agree"] + 
        Table(data[[i]])["agree strongly"]
    y <- Table(data[[i]])["disagree"] + 
        Table(data[[i]])["disagree strongly"]
    z <- Table(data[[i]], include.missings = TRUE)["*don't know"] + 
        Table(data[[i]], include.missings = TRUE)["*no answer"]
    fData[i, 2] <- x
    fData[i, 3] <- y
    fData[i, 4] <- z
}
fData$total <- rowSums(fData[,2:4])
totalSurveys <- fData$total[1]
fData <- cbind(variables = fData$variables, (fData[,2:4]/fData$total)*100)
################################################################################
###                         STEP 3. PLOT CHART                               ###
################################################################################
# 3.1 Prepare plot file and area.-----------------------------------------------
inch <- 2.54
fileName <- paste("PDF_files/",selectedCountry,"_", year,"_barchart_h.pdf", 
                  sep = "")
cairo_pdf(filename = fileName, width = 27.94/inch, height = 21.59/inch, 
          bg = "grey98")
par(omi = c(2/inch, 2/inch, 2/inch, 1.5/inch), 
    mai = c(2/inch, 10/inch, 2/inch, 1.5/inch), family = "Lato Light", las = 1,
    mgp = c(3, 3, 0))
ylength <- n_sections
xlength <- 100
barwidth <- 0.85
valuespace <- 0.15/barwidth
# 3.2 Draw the barplot using formated data--------------------------------------
values <- fData[nrow(fData):1, "agree"]
#categories <- varLabels[length(varLabels):1]
categories <- barLabels
color1 <- "grey"
bp <- barplot(height = values, width = barwidth, space = valuespace,
              names.arg = FALSE, horiz = TRUE, border = NA, 
              xlim = c(0, xlength), ylim = c(0, ylength), col = color1, 
              axes = FALSE, family = "Lato")
color2 <- rgb(255, 0, 210, maxColorValue = 255)
values2 <- vector(mode = "numeric", length = n_sections)
values2[which.max(values)] <- max(values)
bp <- barplot(height = values2, width = barwidth, space = valuespace, 
              names.arg = FALSE, horiz = TRUE, border = NA, 
              xlim = c(0, xlength), ylim = c(0, ylength), col = color2, 
              axes = FALSE, add = TRUE)
for (i in 1:length(barLabels)) {
    if (i == which.max(values)) {
        myFont <- "Lato Bold"
    } else {
        myFont <- "Lato Light"
    }
    text(-3, bp[i], categories[i], xpd = TRUE, adj = 1, family = myFont, 
         cex = 0.8)
    text(10, bp[i], format(round(values[i], 1), 
                           nsmall = 1), 
         family = myFont, cex = 1.25, col = ifelse(i == which.max(values),
                                                   "white", "black"))
}
# 3.3 To recognize difefferent levels of the values, draw --------------------
# a background with rectangles of two light but different colors.
color1 <-  rgb(red = 191, green = 239, blue = 255, alpha =  80, 
               maxColorValue = 255)
color2 <-  rgb(red = 191, green = 239, blue = 255, alpha = 110, 
               maxColorValue = 255)
rect(xright = 0, ybottom =  -0.1, xleft = 20, ytop = ylength*1.04, col = color1, 
     border = NA)
rect(xright = 20, ybottom = -0.1, xleft = 40, ytop = ylength*1.04, col = color2, 
     border = NA)
rect(xright = 40, ybottom = -0.1, xleft = 60, ytop = ylength*1.04, col = color1, 
     border = NA)
rect(xright = 60, ybottom = -0.1, xleft = 80, ytop = ylength*1.04, col = color2, 
     border = NA)
rect(xright = 80, ybottom = -0.1, xleft = 100, ytop = ylength*1.04, 
     col = color1, border = NA)
# 3.4 Draw a line to cut the majority-------------------------------------------
arrows(x0 = 50, y0 = -0.1, x1 = 50, y1 = ylength*1.04, lwd = 1.5, length = 0, 
       xpd = TRUE, col = "skyblue3")
arrows(x0 = 50, y0 = ylength*(1.04), x1 = 50, y1 = ylength*(1.04) + 0.1, 
       lwd = 1.5, length = 0, xpd = TRUE, col = "black")
arrows(x0 = 50, y0 = -0.11, x1 = 50, y1 = -0.2, lwd = 1.5, length = 0, 
       xpd = TRUE, col = "black")
text(labels =  "Mayoría", x = 48, y = ylength*1.05, adj = 1, xpd = TRUE, 
     cex = 0.65, font = 3)
text(labels = "50%", x = 52, y = ylength*1.05, adj = 0, xpd = TRUE, 
     cex = 0.65, font = 3)
# 2.9 Titling and further labeling----------------------------------------------
chartTitle <- paste("'Qué tan importante es en tu vida', ", 
                    selectedCountry, " (", year, ")", sep = "")
chartSubtitle <- "Importante o algo importante:"
scaleTag <- "Valores en porcentaje (%)"
sourceTag <- paste(format(totalSurveys, big.mark = ","), 
                   " encuestas. Fuente: Estudio de Valores Europeo, ", year,
                   ".", sep = "")
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #Español
date <- Sys.Date()
date <- format(date, "%d de %B de %Y")
sourceTag2 <- paste("Base de datos: ", datasetName, 
                    ". Consultada en www.gesis.org el ", date, ".", sep = "")
mtext(text = chartTitle, side = 3, line = 0, adj = 0, cex = 1.5, 
      family = "Lato Black", outer = TRUE)
mtext(text = chartSubtitle, side =  3, line = -2, adj = 0, cex = 1.25, 
      outer = TRUE)
text(labels = scaleTag, 100, 8.9, adj = 1, xpd = TRUE, cex = 0.65, 
     font = 3)
mtext(c(0, 20, 40, 60, 80, 100), at = c(0, 20, 40, 60, 80, 100), 1, line = 0, 
      cex = 0.75)
mtext(text = sourceTag, side = 1, line = 0, adj = 1, cex = 0.85, outer = TRUE, 
      font = 3)
mtext(text =  sourceTag2, side = 1, line = 1, adj = 1, cex = 0.85, outer = TRUE, 
      font = 3 )
dev.off()