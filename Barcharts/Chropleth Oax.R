library("maptools")
library("raster")
library("RColorBrewer")
x <- shapefile(x = "INEGI Mapas/20_oaxaca/conjunto de datos/20mun.shp")
load("./RData_files/hablantesOax.RData")
y <- hablantesOax
x$CVE_MUN <- as.numeric(as.character(x$CVE_MUN))
y$CVE_MUN <- as.numeric(as.character(y$CVE_MUN))
file <- "PDF_files/oax_choropleth.jpg"
jpeg(bg = "grey98", file, width = 8, height = 9, units = "in", quality = 100,
    res = 300)
n <- length(x)
position <- vector()
for (i in 1:n) {
    position[i] <- match(x$CVE_MUN[i], y$CVE_MUN)
}
colors <- cut(y$hablantesrel[position], c(0, 0.2, 0.4, 0.6, 0.8, 1))
levels(colors) <-
    c("De 0 a 20%", ">20 a 40%", ">40 a 60%", ">60 a 80%", ">80 a 100")
palette <- brewer.pal(6, "Oranges")
plot(x, col = palette[colors], border = grey(.8), lwd = .5)
text(getSpPPolygonsLabptSlots(x), labels = as.character(y$CVE_MUN), cex = 0.35,
    col = ifelse(y$hablantesrel > 0.8, "white", "black"), family = "Lato Bold")
legend("bottomleft", levels(colors), cex = 0.95, border = FALSE, bty = "n", 
    fill = palette, title = "Porcentaje de\nhablantes")
mtext("Porcentaje de hablantes de una lengua indígena por municipio\nen el estado de Oaxaca, 2015",
    side = 3, line = -1, adj = 0, family = "Lato Black", outer = F, cex = 1.5)
mtext("Fuente: INEGI, Encuesta Intercensal 2015 e INEGI, Marco Geoestadístico Nacional, 2015 ",
    side = 1, line = 2.6, adj = 0, cex = 0.95, font = 3, outer = FALSE)
dev.off()