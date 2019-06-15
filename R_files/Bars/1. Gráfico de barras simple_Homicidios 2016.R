################################################################################
################################################################################
###                      GRÁFICA SIMPLE DE BARRAS HORIZONTAL                 ###
###                                PORCENTUAL                                ###
################################################################################
################################################################################
# This script plots a simple horizontal bar chart in an external file. 
# It slightly modifies the code created by Thomas Ralhlf in his book 
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
###                      STEP 1. IMPORT DATA FROM FILE                       ###
################################################################################
# 1.1 Using the gdata package to import data from an xlsx file------------------
filedir <- "CSV_files/Homicidios_MX_16.csv"
data <-
    read.csv(filedir, encoding = "latin1")
data <- data[order(data[ , 2]), ] # Order data according to values
categories <- as.character(data[ , 1])
values <- data[ , 2]
################################################################################
###                         STEP 2. PLOT CHART                               ###
################################################################################
# 2.1 Define the destination file and the plot area in centimeters.------------- 
inch <- 2.54
cairo_pdf(
    filename = "PDF_files/1. barchart_horizontal_homicidios.pdf", width = 27.94/inch, 
    height = 21.59/inch, bg = "grey98")
# 2.2 Define the size of inner and outer margins in centimeters.----------------
par(omi = c(2/inch, 2/inch, 2/inch, 1.5/inch), 
    mai = c(2/inch, 6/inch, 2/inch, 1.5/inch), family = "Lato Light", las = 1,
    mgp = c(3, 3, 0))
# 2.3 Plot values without labels.-----------------------------------------------
xlength <- 100
myColor1 <- "grey"
myColor2 <- rgb(255, 0, 210, maxColorValue = 255)
x <- barplot(height = values, names.arg = FALSE, horiz = TRUE, border = NA,
             xlim = c(0, xlength), col = myColor1, cex.names = 0.85, axes = F)
# 2.4 Define the values where we've used the special labels and then -----------
# mark its percentage values and define a different color than the first barplot
myvalue <-
    c(0, 0, 0, 7.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 86)
# 2.5 Plot the second barplot over the first one to higlight--------------------
# selected values.
x2 <- barplot(height = myvalue, names.arg = FALSE, horiz = TRUE, border = NA, 
              xlim = c(0, 100), col = myColor2, cex.names = 0.85, axes = F,
              add = TRUE)
# 2.6 Mark the labels of each bar. If the values of the labels------------------ 
# match the character strings defined, use a special font.
for (i in 1:length(categories)) {
    if (categories[i] %in% c("Tlaxcala", "Colima")) {
        myFont <- "Lato Black"
    } else {
        myFont <- "Lato Light"
    }
    text(labels = categories[i], 
         x = -6, y = x[i], xpd = TRUE, adj = 1, cex = 0.85, family = myFont)
    text(labels = format(values[i], nsmall = 1),
         x = -1, y =  x[i], xpd = TRUE, adj = 1, cex = 0.85, family = myFont)
}
# 2.7 To recognize difefferent measures of the values, draw --------------------
# a background with rectangles of two light but different colors.
color1 <-  rgb(red = 191, green = 239, blue = 255, alpha =  80, 
               maxColorValue = 255)
color2 <-  rgb(red = 191, green = 239, blue = 255, alpha = 110, 
               maxColorValue = 255)
rect(xleft = 0, ybottom = -0.5, xright = 20, ytop = 39, col = color1, 
     border = NA)
rect(xleft = 20, ybottom = -0.5, xright = 40, ytop = 39, col = color2, 
     border = NA)
rect(xleft = 40, ybottom = -0.5, xright = 60, ytop = 39, col = color1, 
     border = NA)
rect(xleft = 60, ybottom = -0.5, xright = 80, ytop = 39, col = color2, 
     border = NA)
rect(xleft = 80, ybottom = -0.5, xright = 100, ytop = 39, col = color1, 
     border = NA)
mtext(c(0, 20, 40, 60, 80, 100), at = c(0, 20, 40, 60, 80, 100), side = 1, 
      line = 0, cex = 0.80)
# 2.8 Draw a line to divide the chart where the mean value is found-------------.
arrows(x0 = mean(values), y0 = -0.5, x1 = mean(values), y1 = 39, lwd = 1.5, 
       length = 0, xpd = TRUE, col = "skyblue3")
arrows(x0 = mean(values), y0 = -0.55, x1 = mean(values), y1 = -0.80, lwd = 3, 
       length = 0, xpd = TRUE)
arrows(x0 = mean(values), y0 = 39, x1 = mean(values), y1 = 39.25, lwd = 3, 
       length = 0, xpd = TRUE)
text(labels = paste("Promedio:", round(mean(values), 1), sep = " "),
     x = mean(values) - 1, y = 39.3, adj = 1, xpd = TRUE, cex = 0.65, font = 3)
# 2.9 Titling and further labeling----------------------------------------------
chartTitle <-  "México: tasa de homicidios por entidad federativa en 2016"
chartSubtitle <- "Defunciones por homicidio según el año de registro y la entidad federativa de ocurrencia:"
scaleTag <- "Tasa de homicidios por cada 100,000 habitantes"
sourceTag <- "Fuente: INEGI. Estadísticas de mortalidad e INEGI. Encuesta Intercensal, 2015."
sourceTag2 <- "Diseño modificado por: Sicabí Cruz"
mtext(text = chartTitle, side = 3, line = 0, adj = 0, cex = 1.2, 
      family = "Lato Black", outer = TRUE, las = 1)
mtext(text = chartSubtitle, side = 3, line = -1, adj = 0, cex = 0.9, 
      family = "Lato Light", outer = TRUE, las = 1)
text(labels = scaleTag, 100, 39.3, adj = 1, xpd = TRUE, cex = 0.65, font = 3)
mtext(text = sourceTag, side = 1, line = -1, adj = 1.0, cex = 0.65, 
      outer = TRUE, font = 3, las = 1)
mtext(text = sourceTag2, side = 1, line = 0, adj = 1.0, cex = 0.65, 
      outer = TRUE, font = 3, las = 1)
dev.off()