# 2.4 Mark the labels of each bar. If the values of the labels match the 
# the character strings defined, use a special font. If not, use a normal one.
for (i in 1:length(ipsos$Country)) {
    if (ipsos$Country[i] %in% c("Germany", "Brazil")) {
        myFont <- "Lato Black"
    } else {
        myFont <- "Lato Light"
    }
    text(-8, x[i], ipsos$Country[i], xpd = TRUE, adj = 1, cex = 0.85, 
         family = myFont)
    text(-3.5, x[i], ipsos$Percent[i], xpd = TRUE, adj = 1, cex = 0.85,
         family = myFont)
}
# 2.5 To recognize difefferent measures of the bars, paint a background with
# rectangles of two light but different colors.
rect(0, -0.5, 20, 28, col = rgb(191, 239, 255, 80, maxColorValue = 255),
     border = NA)
rect(20, -0.5, 40, 28, col = rgb(191, 239, 255, 110, maxColorValue = 255), 
     border = NA)
rect(40, -0.5, 60, 28, col = rgb(191, 239, 255, 80, maxColorValue = 255),
     border = NA)
rect(60, -0.5, 80, 28, col = rgb(191, 239, 255, 110, maxColorValue = 255),
     border = NA)
rect(80, -0.5, 100, 28, col = rgb(191, 239, 255, 80, maxColorValue = 255),
     border = NA)
# 2.6 Define the bars where we used the special labels and mark its percentage
# values and define a different color than the first barplot.
myvalue <- c(0, 0, 0, 0, 27, 0, 0, 0, 0, 0, 0, 0, 0, 84, 0, 0)
myColor2 <- rgb(255, 0, 210, maxColorValue = 255)
# 2.7 Plot the second plot over the first one.
x2 <- barplot(myvalue, names.arg = FALSE, horiz = FALSE, border = NA, 
              xlim = c(0, 100), col = myColor2, cex.names = 0.85, axes = F,
              add = TRUE)
# 2.8 Draw a line to divide the chart in two equal parts.
arrows(45, -0.5, 45, 20.5, lwd = 1.5, length = 0, xpd = TRUE,
       col = "skyblue3")
# 2.9 Draw two lines to mark the beginning and the end of the dividing line.
arrows(45, -0.5, 45, -0.75, lwd = 3, length = 0, xpd = TRUE)
arrows(45, 20.5, 45, 20.75, lwd = 3, length = 0, xpd = TRUE)
# 2.10 Titling and further labeling.
text(41, 20.5, "Average", adj = 1, xpd = TRUE, cex = 0.65, font = 3)
text(44, 20.5, "45", adj = 1, xpd = TRUE, cex = 0.65, family = "Lato", font = 4)
text(100, 20.5, "All values in percent", adj = 1, xpd = TRUE, cex = 0.65, 
     font = 3)
mtext(c(0, 20, 40, 60, 80, 100), at = c(0, 20, 40, 60, 80, 100), 1, line = 0,
      cex = 0.80)
mtext("'I Definetely Believe in God or a Supreme Being'", 3, line = 1.3,
      adj = 0, cex = 1.2, family = "Lato Black", outer = TRUE)
mtext("was said in 2010 in:", 3, line = -0.4, adj = 0, cex = 0.9, outer = TRUE)
mtext("Source: www.ipsos-na.com, Design: Stefan Fichtel, extract", 1, line = 1,
      adj = 1.0, cex = 0.65, outer = TRUE, font = 3)
dev.off()



x<-rnorm(12)
plot(x,axes=FALSE)
box()
months<-c("January","February","March","April","May","June",
          "July","August","September","October","November","December")
staxlab(1,1:12,months)
plot(x,axes=FALSE)
box()
staxlab(1,1:12,months,srt=45)
ylabels<-round(seq(min(x),max(x),length.out=10),3)
staxlab(2,ylabels,ylabels,srt=45)

barplot(ipsos$Percent, border = NA, ylim = c(0, 100), col = "grey", axes = F, 
        las = 2, names.arg = countries)
