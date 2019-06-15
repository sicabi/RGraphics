######## 1. IMPORTACIÓN DE LOS DATOS ##### #######
# Datos para gráficos sobre México memisc
library("memisc")
unzip(zipfile = "SAV_archivos/F00005809-WV6_Data_spss_v_2016_01_01.zip", 
      exdir = "SAV_archivos/")
misdatos <- spss.system.file("SAV_archivos/WV6_Data_spss_v_2016_01_01.sav")
misdatos <- subset(misdatos,
                   select = c(v2, v2a, v45, v47, v48, v50, v51, v52, v53, v54, 
                              v240))
paises <- names((table(misdatos$v2)))
misdatos <- subset(misdatos, v2 == "Mexico")
country <- names(table(misdatos$v2))
misdatos$v2 <- NULL
misdatos$v2a <- NULL
file.remove("SAV_archivos/WV6_Data_spss_v_2016_01_01.sav")
######## 2. TRANSFORMACIÓN DE DE LOS DATOS #######
library("numbers")
n_variables <- ncol(misdatos) - 1
variables <- list()
for (i in 1:n_variables) {
    variables[[i]] <- table(misdatos[[i]], misdatos[[ncol(misdatos)]])
}
### Etiquetas de las variables
etiquetas <- vector(mode = "character")
for (i in 1:n_variables) {
    etiquetas[[i]] <-  as.character(annotation(misdatos[[i]]))
    
}
for (i in 1:n_variables) {
    if (nchar(etiquetas[i]) > 60) {
        cadena <- substring(etiquetas[i], 1:nchar(etiquetas[i]), 
                            1:nchar(etiquetas[i]))
        indices <- which(cadena == " ")
        posicion <- indices > 60
        posicion <- length(posicion) - 
            ifelse(is.na(table(posicion)["TRUE"]), 0, table(posicion)["TRUE"])
        substr(etiquetas[i],indices[posicion], indices[posicion]) <- "\n"
    }
}
valores <- vector(mode = "numeric", length = n_variables*2)
j <- 1
for (i in 1:n_variables) {
    a <- which(rownames(variables[[i]]) == "Agree")
    b <- which(rownames(variables[[i]]) == "Agree strongly")
    c <- which(colnames(variables[[i]]) == "Female")
    d <- which(colnames(variables[[i]]) == "Male")
    valores[[j]] <- ifelse(length(a) == 0, 
                           0, 
                           variables[[i]][a, d]) + ifelse(length(b) == 0, 
                                                 0, 
                                                 variables[[i]][b, d])
    valores[[j + 1]] <- ifelse(length(a) == 0, 
                               0, 
                               variables[[i]][a, c]) + ifelse(length(b) == 0, 
                                                     0, 
                                                     variables[[i]][b, c])
    j <- j + 2
}
maximo <- max(valores)
divisores <- divisors(round(maximo, -1))
indice <- length(divisores) - length(divisores)/2 - 1
divisor <- divisores[indice]
personas <- round(maximo / divisor)
marcas <- vector(mode = "character")
lim_sup <- divisor + 1
for (i in 1:lim_sup) {
    marcas[[i]] <- paste((i)*personas, sep = "")
}
valores_marcas <- vector(mode = "integer")
for (i in 1:lim_sup) {
    valores_marcas[[i]] <- (i)*personas
}
x <- length(marcas)
n_encuestados <- nrow(misdatos)
######## 3. FUNCIONES DE GRAFICACIÓN ##### ########
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
######## 4. PLOT CHART ##################### #######
pdf_file <- "PDF_archivos/barras y simbolos ordenados.pdf"
cairo_pdf(bg = "grey98", pdf_file, width = 13, height = 10.5)
par(omi = c(0.65, 0.65, 0.75, 0.75), mai = c(1.1, 5.85, 1.55, 0), 
    family = "Lato Light",
    las = 1)
# Prepare chart
col_f <- rgb(255, 97, 0, 190, maxColorValue = 255)
col_m <- rgb(68, 90, 111, 190, maxColorValue = 255)
# Create chart
plot(1:n_variables, type = "n" , axes = F, xlab = "", ylab = "", xlim = c(0, x), 
     ylim = c(1, n_variables*2)) 
par(family = "Lato Light", cex = 0.85)
axis(1, at = c(1:lim_sup), labels = marcas, col = par("bg"), 
     col.ticks = "grey81",
     lwd.ticks = 0.5, tck = -0.025)
abline(v = c(1:lim_sup), lty = "dotted")
j <- 1
for (i in 1:(n_variables)) {
    mySymbolsMen(round(valores[j] / personas), j, etiquetas[i])
    mySymbolsWomen(round(valores[j + 1] / personas), j + 1)
    j <- j + 2
}
# Titling
par(family = "Lato Light")
mtext(paste("Opinions about gender roles in ", country, ", 2016", sep = ""),
      3, line = -3, adj = 0,
    cex = 1.8, family = "Lato Black", outer = T)
mtext("Persons that aswered 'Agree' or 'Strongly agree' by sex", 
      3, line = -6, adj = 0, cex = 1.8, outer = T, font = 3)
mtext(paste(format(n_encuestados, big.mark = ","),
            "polls. Each symbol stands for", personas, "people.", 
            sep = " "), 1, line = -3, adj = 0, cex = 1.5, outer = T, font = 3)
mtext("Women ", 1, line = -0.5, adj = 0.02, cex = 1.5, outer = T, font = 3)
mtext("Men ", 1, line = -0.5, adj = 0.12, cex = 1.5, outer = T, font = 3)
par(family = "Symbol Signs")
mtext("F", 1, line = -0.5, adj = 0, cex = 2.5, outer = T, font = 3, 
      col = col_f)
mtext("M", 1, line = -0.5, adj = 0.1, cex = 2.5, outer = T, font = 3, 
      col = col_m)
mtext( paste("Source: World Values Survey, Wave 6, 2010-2014. \n Country: ", 
             country, ".\n Dataset: WV6_Data_v2016.", sep = ""), 1, 
       line = -0.5, adj = 1, cex = 1.25, outer = T, font = 3, 
       family = "Lato Light")
dev.off()

