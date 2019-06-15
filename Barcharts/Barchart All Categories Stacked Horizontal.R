################################################################################
################################################################################
###                     HORIZONTAL MULTIPLE-RESPONSE                         ###
###                                STACKED BAR CHART                         ###
################################################################################
################################################################################
# This script plots a horizontal bar chart for multiple categories or
# responses in an external file. Specifically, it works with survey data and 
# it allows the user to compare a group of questions and their answers. 
# It allows the reader to view all responses to a question.
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
                 `Muy de acuerdo` = vector(length = length(varLabels), 
                                mode = "numeric"),
                 `De acuerdo` = vector(length = length(varLabels), 
                                     mode = "numeric"),
                 `No sabe o no contestó` = vector(length = length(varLabels), 
                                                          mode = "numeric"),
                 `En desacuerdo` = vector(length = length(varLabels), 
                                                 mode = "numeric"),
                 `Muy en desacuerdo` = vector(length = length(varLabels), 
                                     mode = "numeric"))


for (i in 1:n_sections) {
 v <- Table(data[[i]])["agree strongly"]
 w <- Table(data[[i]])["agree"]
 x <- Table(data[[i]], include.missings = TRUE)["*don't know"] + 
     Table(data[[i]], include.missings = TRUE)["*no answer"]
 y <- Table(data[[i]])["disagree"]
 z <- Table(data[[i]])["disagree strongly"]
 fData[i, 2] <- v
 fData[i, 3] <- w
 fData[i, 4] <- x
 fData[i, 5] <- y
 fData[i, 6] <- z
}
totalSurveys <- rowSums(fData[,2:ncol(fData)])[1]
fData <- cbind(variables = fData$variables, 
            (fData[,2:ncol(fData)]/totalSurveys)*100)
fData <- fData[nrow(fData):1, ]
responses <- colnames(fData[,2:ncol(fData)])
responses <- gsub(pattern = "\\.", replacement = " ", responses)
responses <- Hmisc::capitalize(responses)
print(responses)
responses[3] <- "No sabe/No contestó"
################################################################################
###                         STEP 3. PLOT CHART                               ###
################################################################################
# 3.1 Prepare plot file and area.-----------------------------------------------
inch <- 2.54
fileName <- paste("PDF_files/",selectedCountry,"_", year,"_barchart_stacked.pdf", 
               sep = "")
cairo_pdf(filename = fileName, width = 27.94/inch, height = 21.59/inch, 
       bg = "grey98")
par(omi = c(2/inch, 2/inch, 2/inch, 1.5/inch), 
 mai = c(2/inch, 9/inch, 3/inch, 1.5/inch), family = "Lato Light", las = 1,
 mgp = c(3, 3, 0))
# 3.2 Define measures and colors of the barplot---------------------------------
ylength <- n_sections
xlength <- 100
barwidth <- 0.85
valuespace <- 0.15/barwidth
color1 <- rgb(86, 194, 130, maxColorValue = 255)
color2 <- rgb(159, 234, 189, maxColorValue = 255)
color3 <- rgb(224, 224, 207, maxColorValue = 255)
color4 <- rgb(245, 175, 192, maxColorValue = 255)
color5 <- rgb(233, 129, 154, maxColorValue = 255)
colors <- c(color1, color2, color3, color4, color5)
# 3.3 Draw barplot--------------------------------------------------------------
bp <- barplot(t(as.matrix(fData[, 2:ncol(fData)])), names.arg = barLabels, 
           cex.names = 1, horiz = TRUE, border = NA, xlim = c(0, xlength), 
           ylim = c(0, ylength), col = colors, axes = FALSE, family = "Lato", 
           width = barwidth, space = valuespace)
# 3.4 Set value labels for each bar---------------------------------------------
xpos <- apply(t(as.matrix(fData[ , 2:ncol(fData)])), 2, cumsum)
xpos <- xpos - t(as.matrix(fData[ , 2:ncol(fData)])) / 2
xpos <- t(xpos)
text(xpos, bp, 
     format(round(as.matrix(fData[, 2:ncol(fData)]), 1), nsmall = 1), 
     cex = 0.75, col = "gray27") 
# 3.5 Draw responses labels-----------------------------------------------------
px <-  c(1, 22, 38, 62, 81)
py <- rep(max(bp)*1.1, length(responses))
points(px, py, pch = 15, cex = 2, col = colors, xpd = TRUE)
tx <- px + 3
ty <- rep(max(bp)*1.1, length(responses))
text(tx, ty, responses, adj = 0, xpd = TRUE, family = "Lato", font = 3, 
  cex = 0.65)
# 2.6 Titling and further labeling----------------------------------------------
chartTitle <- paste("Opiniones con respecto a los roles de género en ", 
                 selectedCountry, ", " , year, sep = "")
chartSubtitle <- "Todas las respuestas"
scaleTag <- "Valores en porcentaje (%)"
sampleTag <- paste("n = ", format(totalSurveys, big.mark = ","), 
                " encuestas", sep = "")
sourceTag <- paste("Fuente: Estudio de Valores Europeo, ", year, 
                ". Base de datos: ", datasetName, sep = "")
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #Español
date <- Sys.Date()
date <- format(date, "%d de %B de %Y")
dateTag <- paste("Consultada en: www.gesis.org el ", date, ".", sep = "")
mtext(text = chartTitle, side = 3, line = 0, adj = 0, cex = 1.75, 
   family = "Lato Black", outer = TRUE)
mtext(text = chartSubtitle, side =  3, line = -2, adj = 0, cex = 1.5, 
   outer = TRUE, font = 3)
mtext(c(0, 20, 40, 60, 80, 100), at = c(0, 20, 40, 60, 80, 100), side = 1, 
   line = -1, cex = 0.9)
mtext(text = scaleTag, line = 0, side = 1, cex = 0.9, adj = 1, 
    font = 3)
mtext(text = sampleTag, side = 1, line = 3, adj = 1, cex = 0.9, font = 3)
mtext(text =  sourceTag, side = 1, line = 4, adj = 1, cex = 0.9, font = 3)
mtext(text =  dateTag, side = 1, line = 5, adj = 1, cex = 0.9, font = 3)
dev.off() 