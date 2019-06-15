# Definir las etiquetas de las categorías y almacenarlas
# en una variable 
myC_v159 <-
    "Una madre trabajadora puede establecer un ambiente 
tan cálido y seguro como una madre que no trabaja"
myC_v160 <-
    "Un niño de menos de tres años sufriría si 
su madre trabajara"
myC_v161 <-
    "Tener un trabajo está bien, pero lo que la mayoría de
las mujeres realmente quiere es un hogar y sus hijos"
myC_v162 <-
    "Ser un ama de casa es tan satisfactorio 
como tener un trabajo"
myC_v163 <- "Tener un trabajo es la mejor manera para una
mujer de volverse independiente"
myC_v164 <- "Tanto el esposo como la esposa deberían contribuir
al ingreso familiar"
myC_v165 <-
    "En general, los padres están igual de preparados para cuidar
a los hijos como las mujeres"
myC_v166 <-
    "Los hombres deberían aceptar la misma responsabilidad que las
mujeres en la casa y con los hijos"
mynames <-
    c(myC_v165,
      myC_v164,
      myC_v163,
      myC_v162,
      myC_v161,
      myC_v160,
      myC_v159)
myvalues <-
    c(
        "No sabe/No contestó",
        "Muy de acuerdo",
        "De acuerdo",
        "En desacuerdo",
        "Muy en desacuerdo"
    )
# Crear archivo PDF en donde se almacenará el gráfico
library("Cairo")
pdf_file <- "PDF_files/all_response_categories.pdf"
cairo_pdf(pdf_file, width = 13,
    height = 10.5,
    bg = "grey98"
)
# Medidas del lienzo
par(
    omi = c(1.0, 1.0, 1.0, 0.25),
    lheight = 1.15,
    family = "Lato Light",
    las = 1
)
#Distribución
layout(matrix(
    data = c(1, 2, 3, 4, 5),
    nrow = 1,
    ncol = 5
), widths = c(2.5, 1, 1, 1, 1))
#Colores
myC1 <- rgb(0, 208, 226, maxColorValue = 255)
myC2 <- rgb(109, 221, 225, maxColorValue = 255)
myC3 <- rgb(255, 138, 238, maxColorValue = 255)
myC4 <- rgb(255, 0, 210, maxColorValue = 255)
mycolors <- c("grey", myC1, myC2, myC3, myC4)
#Datos
z <- NULL
y <- 100 * table(as.matrix(v165), exclude = FALSE) / length(v165)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(v164), exclude = FALSE) / length(v164)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(v163), exclude = FALSE) / length(v163)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(v162), exclude = FALSE) / length(v162)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(v161), exclude = FALSE) / length(v161)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(v160), exclude = FALSE) / length(v160)
z <- rbind(z, y[1:5])
y <- c(100 * table(as.matrix(v159), exclude = FALSE) / length(v159))
z <- rbind(z, y[1:5])
w <- NULL
w <- cbind(w, z[,5])
w <- cbind(w, z[,1:4])
colnames(w) <- c("1","2","3","4","5")

# Crear gráfica
for (i in 1:5) {
    if (i == 1) {
        par(mai = c(0.25, 2.75, 0.25, 0.15))
        bp1 <- # Primera barra
            barplot(
                w[, i],
                horiz = T,
                cex.names = 1.1,
                names.arg = mynames,
                xlim = c(0, 50),
                col = mycolors[i],
                border = NA,
                axes = F
            )
        text( # Etiquetas de las barras
            w[,i] + 2.5,
            bp1,
            format(round(w[, i], 1), nsmall = 1),
            family = "Lato Light",
            cex = 1.25,
            col = "black"
        )
    } else {
        par(mai = c(0.25, 0.1, 0.25, 0.15))
        bp2 <- # Resto de barras
            barplot(
                w[, i],
                horiz = T,
                axisnames = F,
                xlim = c(0, 50),
                col = mycolors[i],
                border = NA,
                axes = F
            )
        text( # Etiquetas de las barras
            w[,i] + 2.5,
            bp2,
            format(round(w[, i], 1), nsmall = 1),
            family = "Lato Light",
            cex = 1.25,
            col = "black"
        )
        }
    rect(
        0,
        0,
        10,
        8.5,
        col = rgb(191, 239, 255, 80, maxColorValue = 255),
        border = NA
    )
    rect(
        10,
        0,
        20,
        8.5,
        col = rgb(191, 239, 255, 120, maxColorValue = 255),
        border = NA
    )
    rect(
        20,
        0,
        30,
        8.5,
        col = rgb(191, 239, 255, 80, maxColorValue = 255),
        border = NA
    )
    rect(
        30,
        0,
        40,
        8.5,
        col = rgb(191, 239, 255, 120, maxColorValue = 255),
        border = NA
    )
    rect(
        40,
        0,
        50,
        8.5,
        col = rgb(191, 239, 255, 80, maxColorValue = 255),
        border = NA
    )
    
    mtext(
        myvalues[i],
        3,
        adj = 0,
        line = 0,
        cex = 0.95,
        font = 3
    )
    mtext(
        c(10, 20, 30, 40, 50) ,
        at = c(10, 20, 30, 40, 50) ,
        1,
        line = 1,
        cex = 0.85
    )
    mtext(
        0,
        at = 0,
        1,
        line = 1,
        cex = 0.90,
        family = "Lato Bold"
    )
    arrows(
        0,
        -0.1,
        0,
        8.6,
        lwd = 2.5,
        length = 0,
        xpd = T,
        col = "skyblue3"
    )
}
# Leyendas
mtext(
    "Opiniones con respecto a los roles de género en Alemania (2008)",
    3,
    line = 3.5,
    adj = 1,
    cex = 1.8,
    family = "Lato Black",
    outer = T
)
mtext(
    "N = 2,075",
    1,
    line = 3,
    adj = 0.25,
    cex = 1.1,
    family = "Lato",
    font = 4,
    outer = T
)
mtext(
    "Todos los valores en porcentajes",
    1,
    line = 5,
    adj = 0.29,
    cex = 1.1,
    font = 3,
    outer = T
)
mtext(
    "Fuente: Estudio de Valores Europeo, 2008 (Alemania).
    Base de datos: ZA4800, consultada en www.gesis.org el 27 de mayo de 2018
    Diseño: Stefan Fichtel",
    1,
    line = 5.5,
    adj = 1.0,
    cex = 0.95,
    outer = T
)
#Cerrar gráfica
dev.off()

