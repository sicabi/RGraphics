pdf_file<-"PDF_files/maps_germany_choropleth_counties.pdf"
cairo_pdf(bg="grey98", pdf_file,width=8,height=9)

par(mai=c(0,0,0,0),omi=c(1,0.25,1,0.25),family="Lato Light",las=1)
library(maptools)
library(RColorBrewer)
library(gdata)

# Import data and prepare chart

#x<-rgdal::readOGR(dsn = path.expand("./Scripts/Geometrie_Wahlkreise_16DBT_Shape_VG1000/Geometrie_Wahlkreise_16DBT_VG1000.shp/"))
x <- shapefile("ZIP_files/Geometrie_Wahlkreise_16DBT_Shape_VG1000/Geometrie_Wahlkreise_16DBT_VG1000.shp")
y <- read.csv(file="ZIP_files/scriptsanddata/myData/struktbtwkr2005.csv",head = F, sep = ";", dec = ".")
myDistrict_structure_data <- subset(y,V2 > 0)
n<-length(x)
position<-vector()
for (i in 1:n){
    position[i]<-match(x$WKR_NR[i], myDistrict_structure_data$V2)
}
myColour_no<-cut(myDistrict_structure_data$V34[position],c(4,9,12.5,16.5,20,24,28))
levels(myColour_no)<-c("4 to 9 %","9 to 12.5 %","12.5 to 16.5 %","16.5 to 20 %","20 to 24 %","24 to 28 %")
myColours<-brewer.pal(6,"Oranges")

# Create chart

plot(x,col=myColours[myColour_no],border=grey(.8),lwd=.5)
legend("topleft", levels(myColour_no), cex = 0.95, border = F , bty = "n", 
       fill = myColours, bg = "transparent", inset = 0.05, y.intersp = 0.75)

# Titling

mtext("Unemployed at electoral disctrict level 2005",3,line=1,adj=0,family="Lato Black",outer=T,cex=2)
mtext("Source (map geometry and data): www.bundeswahlleiter.de",1,line=2.6,adj=0,cex=0.95,font=3,outer=T)
dev.off()



