library("maptools")
library("raster")
library("RColorBrewer")
library("gdata")
library("foreign")
library("rgeos")
x <- shapefile(x = "INEGI Mapas/09_ciudaddemexico/conjunto de datos/09mun.shp")
y <- read.csv(file = "CSV_files/hablantes_cdmx1.csv", header = TRUE, 
         na.strings = FALSE)
y$CVE_MUN <- substr(y$Municipio, 1, 3)
y <- y[order(y$CVE_MUN), ]
y$Municipio <- trimws(gsub('[[:digit:]]+', '', y$Municipio), "both")
pdf_file<-"PDF_files/cdmx_choropleth.pdf"
inch <- 2.54
cairo_pdf(bg="grey98", pdf_file,width=8,height=9)
n <- length(x)
position<-vector()
for (i in 1:n){
    position[i]<-match(x$CVE_MUN[i], y$CVE_MUN)
}
colors <- cut(y$Hablantes[position],c(0, 0.5, 1, 1.5, 2, 5, 10))
levels(colors) <- c("0 a 0.5%", "0.51 a 1%", "1.1 a 1.5%", "1.51 a 2%", "2.1 a 5%", "5.1 a 10%")
palette <- brewer.pal(6,"Blues")
plot(x,col=palette[colors],border=grey(.8),lwd=.5)
text(getSpPPolygonsLabptSlots(x),labels=y$Municipio,cex=0.50,col=ifelse(y$Hablantes > 2, "white", "black"),family="Lato Bold")
legend("bottomleft",levels(colors),cex=0.95,border=F,bty="n",fill=palette, title = "Porcentaje de\nhablantes", )
mtext("Porcentaje de hablantes de una lengua indígena\n en la Ciudad de México por delegación, 2015", 
      side = 3,line=-1,adj=0,family="Lato Black",outer=F,cex=1.5)
mtext("Fuente: INEGI, Encuesta Intercensal 2015 e INEGI, Marco Geoestadístico Nacional, 2015 ", 
      side = 1,line=2.6,adj=0,cex=0.95,font=3,outer=F)
dev.off()

