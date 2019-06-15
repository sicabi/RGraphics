######## BAR CHART FOR MULTIPLE RESPONSE QUESTIONS: SYMBOLS--- ################
#### ---FOR INDIVIDUALS (ORDERED SYMBOLS) 
#### THIS SCRIPT CREATES A BAR CHART USING SYMBOLS INSTEAD OF BARS. THE SYMBOLS
#### WERE PREDIFINED ASSIGNING A EACH OF THEM TO A CHARACTER USING A DRAWER 
#### SOFTWARE. THIS SCRIPT MODIFIES THE ONE PROVIDED BY THOMAS RALHLF IN HIS 
#### BOOK DATA VISUALIZATION WITH R (2017).
#### LAST MODIFIED: NOVEMBER 15TH, 2018.
#### AUTHOR: JOSÉ SICABÍ CRUZ SALINAS.
######## STEP 0. CLEAN ENVIRONMENT ############################################
# 0.1 Clean environment 
rm(list = ls())
# 0.2 Detach any possible conflicting packages.
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
######## STEP 1. IMPORT SURVEY DATA FROM FILE  ################################
# 1.1 Load the memisc package. This package is the fastest and most  
# space-efficient to work with SPSS data. It also allows you to keep the labels 
# from imported data in order to use it in the chart labelling later on.
library("memisc")
# 1.2 Unzip the downloaded survey file. We assume it was manually downloaded to 
# the directory "SAV_files" which will be created in your project directory or 
# working directory.
unzip(zipfile = "SAV_files/EVS/datasets/EVS_80-08.zip", 
      exdir = "SAV_files/", list = TRUE)
unzip(zipfile = "SAV_files/EVS/datasets/EVS_80-08.zip", 
      exdir = "SAV_files/", list = FALSE, files = "ZA3811_v3-0-0.sav") 
# 1.3 Read the SPSS data into R.
mydata <- spss.system.file("SAV_files/ZA3811_v3-0-0.sav")
# 1.4 Subset the relevant survey questions into our data. Theses include the 
# country (v2, v2a), the thematic questions (v45-v54) and the socio demographic
# question about the person's sex (v240).
mydata <- subset(mydata,
                   select = c(country, v45, v48, v50, v51, v52, v53, v54, 
                              v240))
# 1.4.1 Retrieve the country names to know which ones were surveyed in this 
# wave/dataset.
countries1 <- names((table(mydata$country)))
countries1 # Display the countries available 
# 1.5 Subset the data for a selected country.
mydata <- subset(mydata, country == "Great Britain")
# 1.5.1 Extract country name for further titling.
country <- names(table(mydata$v2)) 
# 1.5.2 # Delete country variable from de dataset.
mydata$v2 <- NULL 
# 1.6 Delete the SPSS file to keep the disk cleaned.
file.remove("SAV_files/WV6_Data_spss_v_2016_01_01.sav")
# 1.7 Show data warnings.
warnings()
######## STEP 2. TRANSFORM SURVEY DATA TO CHART TYPE ##########################
# 2.1 Define the total number of chart sections according to the relevant  
# survey questions. We have to take away one section from the total number  
# since the last one, v240(sex), is not properly a section 
# or a thematic question, but an element to present crossed results by sex.
n_sections <- ncol(mydata) - 1 
# 2.2.1 Declare the list where we are going to save the 'bars'.
chart_sec <- list() 
# 2.2.2 Run the loop to create cross-tabs for the relevant survey 
# questions by sex.
for (i in 1:n_sections) { 
    chart_sec[[i]] <- table(mydata[[i]], mydata[[ncol(mydata)]])
}
# 2.2.3 Extract the section labels from the dataset.
section_labels <- vector(mode = "character")
for (i in 1:n_sections) {
    section_labels[[i]] <-  as.character(annotation(mydata[[i]]))
    
}
# 2.2.3.1 If the number of characters from a given label section is too long, 
# (bigger than 60 characters, including white spaces), we break lines before 
# the end of the sentence. The break position is defined after the first white 
# space found after the 60th character.
for (i in 1:n_sections) {
    if (nchar(section_labels[i]) > 60) {
        char_string <- substring(section_labels[i], 1:nchar(section_labels[i]), 
                            1:nchar(section_labels[i]))
        index <- which(char_string == " ")
        position <- index > 60
        position <- length(position) - 
            ifelse(is.na(table(position)["TRUE"]), 0, table(position)["TRUE"])
        substr(section_labels[i],index[position], index[position]) <- "\n"
    }
}
# 2.3 With the chart sections tables, create the bars for each section 
# according to the sex variable and adding up the people who answered agree 
# or strongly agree. Here the memisc packake helps us to identify the answer 
# directly by its label in the dataset.
total_persons <- vector(mode = "numeric", length = n_sections*2)
bars <- vector(mode = "numeric", length = n_sections*2)
j <- 1
for (i in 1:n_sections) {
    a <- which(rownames(chart_sec[[i]]) == "Agree")
    b <- which(rownames(chart_sec[[i]]) == "Agree strongly")
    c <- which(colnames(chart_sec[[i]]) == "Female")
    d <- which(colnames(chart_sec[[i]]) == "Male")
    bars[[j]] <- ifelse(length(a) == 0, 
                           0, 
                           chart_sec[[i]][a, 
                                               d]) + ifelse(length(b) == 0, 
                                                          0, 
                                                          chart_sec[[i]][b, d])
    bars[[j + 1]] <- ifelse(length(a) == 0, 
                               0, 
                               chart_sec[[i]][a, 
                                              c]) + ifelse(length(b) == 0, 
                                                              0, 
                                                              chart_sec[[i]][b, 
                                                                             c])
    total_persons[[j]] <- bars[[j]] + bars[[j + 1]]
    total_persons[[j + 1]] <- bars[[j]] + bars[[j + 1]]
    j <- j + 2
}
bars_names <- rep(c("men", "women"), n_sections)
names(bars) <- bars_names
bars_section <- rep(section_labels, each = 2)
dataset <- data.frame("Opinions" = bars_section, 
                      "Sex" = bars_names, 
                      "Persons" = bars,
                      "Total" = total_persons,
                      stringsAsFactors = FALSE)
dataset <- dataset[order(dataset[ , 4]), ]
# 2.4 Define the values of other elements of the chart. As le length of the x
# axis, the labels of the tick marks, the number of persons represented by a
# single symbol and the number of persons that took the survey.
max <- max(bars)
divisors <- numbers::divisors(round(max, 1))
index <- length(divisors) - length(divisors)/2 - 1
divisor <- divisors[index]
persons <- round(max / divisor)
marks <- vector(mode = "character")
lim_sup <- divisor + 1
for (i in 1:lim_sup) {
    marks[[i]] <- paste((i)*persons, sep = "")
}
bars_marks <- vector(mode = "integer")
for (i in 1:lim_sup) {
    bars_marks[[i]] <- (i)*persons
}
x <- length(marks)
n_surveys <- nrow(mydata)
######## STEP 3. DEFINE PLOTTING FUNCTIONS ####################################
# 3.1 Defines a function that takes the number of females and prints sign...
mySymbolsWomen <- function(n_f, y, ...) {
    if (n_f == 0) {
        par(family = "Symbol Signs", cex = 2.5)
        text(1, y, "F", col = "navajowhite")
        par(family = "Lato Light", cex = 1)
    } else {
        par(family = "Symbol Signs", cex = 2.5)
        for (i in 1:n_f) {
            text(i, y, "F", col = col_f)
        }
        par(family = "Lato Light", cex = 1)
    }
}
mySymbolsMen <- function(n_m, y, labelling, ...) {
    if (n_m == 0) {
        par(family = "Symbol Signs", cex = 2.5)
        text(1, y, "M", col = "lightsteelblue3")
        par(family = "Lato Light", cex = 1)
        text(0, y + 0.5, labelling, xpd = T, adj = 1)
    } else {
        par(family = "Symbol Signs", cex = 2.5)
        for (i in 1:n_m) {
            text(i, y, "M", col = col_m)
        }
        par(family = "Lato Light", cex = 1)
        text(0, y + 0.5, labelling, xpd = T, adj = 1)
    }
}
######## STEP 4. PLOT CHART ##############################################
pdf_file <- "PDF_files/barras y simbolos ordenados.pdf"
cairo_pdf(filename = pdf_file, bg = "grey98", width = 13, height = 10.5)
par(omi = c(0.65, 0.65, 0.75, 0.75), mai = c(1.1, 5.85, 1.55, 0), 
    family = "Lato Light",
    las = 1)
# Prepare chart
col_f <- rgb(255, 97, 0, 190, maxColorValue = 255)
col_m <- rgb(68, 90, 111, 190, maxColorValue = 255)
# Create chart
plot(1:n_sections, type = "n" , axes = F, xlab = "", ylab = "", xlim = c(0, x), 
     ylim = c(1, n_sections*2)) 
par(family = "Lato Light", cex = 0.85)
axis(1, at = c(1:lim_sup), labels = marks, col = par("bg"), 
     col.ticks = "grey81",
     lwd.ticks = 0.5, tck = -0.025)
axis(2, at = c(1:n_sections), col = par("bg"), 
     col.ticks = "grey81",
     lwd.ticks = 0.5, tck = -0.025)
abline(v = c(1:lim_sup), lty = "dotted")
# Ordered according to the number of persons
j <- 1
for (i in 1:(n_sections)) {
    mySymbolsMen(round(dataset[j, 3] / persons), j, dataset[j, 1])
    mySymbolsWomen(round(dataset[j + 1, 3] / persons), j + 1)
    j <- j + 2
}
rect(xleft = 0.2, ybottom = 0, xright = lim_sup + 0.2, ytop = 2.5,
    col = rgb(191, 239, 255, 120, maxColorValue = 255),
    border = NA
)
# Alphabetic order
# j <- 1
# for (i in 1:(n_sections)) {
#    mySymbolsMen(round(bars[j] / persons), j, bars_section[j])
#    mySymbolsWomen(round(bars[j + 1] / persons), j + 1)
#    j <- j + 2
# }
# Titling
par(family = "Lato Light")
mtext(paste("Opinions about gender roles in ", country, ", 2016", sep = ""),
      3, line = -3, adj = 0,
      cex = 1.8, family = "Lato Black", outer = T)
mtext("Persons that aswered 'Agree' or 'Strongly agree' by sex", 
      3, line = -6, adj = 0, cex = 1.8, outer = T, font = 3)
mtext(paste(format(n_surveys, big.mark = ","),
            "surveys. Each symbol represents", persons, "people.", 
            sep = " "), 1, line = -3, adj = 0, cex = 1.5, outer = TRUE, 
            font = 3)
mtext("Women ", 1, line = -0.5, adj = 0.02, cex = 1.5, outer = TRUE, font = 3)
mtext("Men ", 1, line = -0.5, adj = 0.12, cex = 1.5, outer = TRUE, font = 3)
par(family = "Symbol Signs")
mtext("F", 1, line = -0.5, adj = 0, cex = 2.5, outer = TRUE, font = 3, 
      col = col_f)
mtext("M", 1, line = -0.5, adj = 0.1, cex = 2.5, outer = TRUE, font = 3, 
      col = col_m)
mtext( paste("Source: World Values Survey, Wave 6, 2010-2014. \n Country: ", 
             country, ".\n Dataset: WV6_Data_v2016.", sep = ""), 1, 
       line = -0.5, adj = 1, cex = 1.25, outer = T, font = 3, 
       family = "Lato Light")
dev.off()
countries1