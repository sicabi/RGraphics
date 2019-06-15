# Stacked around zero
attach(mydata)
myC_v159 <-
    "Una madre trabajadora puede establecer un ambiente 
tan cálido y seguro como una madre que no trabaja"
myC_v160 <-
    "Un niño de menos de tres años sufriría si su madre trabajara"
myC_v161 <-
    "Tener un trabajo está bien, pero lo que  la mayoría de 
las mujeres realmente quiere es un hogar y sus hijos"
myC_v162 <- "Ser un ama de casa es tan satisfactorio como tener un trabajo"
myC_v163 <- "Tener un trabajo es la mejor manera para una
mujer de volverse independiente"
myC_v164 <- "Tanto el esposo como la esposa deberían contribuir 
al ingreso familiar"
myC_v165 <- "En general, los padres están igual de preparados para cuidar 
a los hijos como las mujeres"
myC_v166 <- "Los hombres deberían aceptar la misma responsabilidad que las 
mujeres en la casa y con los hijos"
mynames <-
    c(myC_v165,
      myC_v164,
      myC_v163,
      myC_v162,
      myC_v161,
      myC_v160,
      myC_v159)



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

myresponses <-
    c("no sabe/no contestó",
      "muy de acuerdo",
      "de acuerdo",
      "en desacuerdo",
      "muy en desacuerdo")

cairo_pdf(
    "PDF_files/Stacked_around_zero.pdf",
    width = 13,
    height = 10.5,
    bg = "grey98"
)
par(
    omi = c(0.25, 0.75, 1, 0.75),
    mai = c(1.8, 3.75, 0.25, 0),
    lheight = 1.15,
    family = "Lato Light",
    las = 1
)

color1 <- rgb(0, 208, 226, maxColorValue = 255)
color2 <- rgb(109, 221, 225, maxColorValue = 255)
color3 <- rgb(255, 138, 238, maxColorValue = 255)
color4 <- rgb(255, 0, 210, maxColorValue = 255)
mycolors <- c("grey", color1, color2, color3, color4)

mydata0 <- cbind(z[, 5], z[, 1], z[, 2], z[, 3], z[, 4])
mydata1 <- t(mydata0)

bp <- barplot(
    -rep(100, 7),
    names.arg = mynames,
    cex.names = 0.9,
    horiz = T,
    border = par("bg"),
    xlim = c(-100, 70),
    col = mycolors[1],
    axes = F
)
bp <- barplot(
    -(100 - mydata1[1, ]),
    names.arg = mynames,
    cex.names = 0.9,
    horiz = T,
    border = par("bg"),
    xlim = c(-100, 70),
    col = par("bg"),
    axes = F,
    add = T
)

bp <- barplot(
    -mydata1[3:2, ],
    names.arg = mynames,
    cex.names = 0.9,
    horiz = T,
    border = NA,
    xlim = c(-100, 70),
    col = mycolors[3:2],
    axes = F,
    add = T
)
bp <- barplot(
    mydata1[4:5, ],
    names.arg = mynames,
    cex.names = 0.9,
    horiz = T,
    border = NA,
    xlim = c(-100, 70),
    col = mycolors[4:5],
    axes = F,
    add = T)

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
px <- c(-120, -77, -40, -10, 25)
tx <- c(-115, -72, -35, -5, 30)
y <- rep(-1, 5) 
points(
    px,
    y,
    pch = 15,
    cex = 4,
    col = mycolors,
    xpd = T
)

text(tx,
     y,
     myresponses,
     adj = 0,
     xpd = T,
     font = 3)
mtext(
    c(80, 60, 40, 20, 0, 20, 40, 60) ,
    at = c(-80, -60, -40, -20, 0, 20, 40, 60),
    1,
    line = 0,
    cex = 0.95
)

mtext(
    "N = 2,075",
    1,
    line = 6,
    adj = 0,
    cex = 1.15,
    family = "Lato",
    font = 3
)
mtext(
    "Opiniones con respecto a los roles de género en Alemania (2008)",
    3,
    line = 2.2,
    adj = 0,
    cex = 1.8,
    outer = TRUE,
    family = "Lato Black"
)

mtext(
    "Valores en porcentaje (%)",
    3,
    line = 1,
    adj = 1,
    cex = 0.95,
    font = 3
)
mtext("Fuente: Estudio de Valores Europeo, 2008.
      Base de datos: ZA4800, consultada en www.gesis.org el 14 de marzo de 2018",
      1,
      line = 6,
      adj = 1,
      cex = 0.95,
      font = 3
)
#Etiquetqs para las barras
xpos <- -(100 - mydata1[1, ])
dist <- (xpos + 100)/2
xpos <- xpos - dist
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(mydata1[1, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "gray27")

xpos <- apply(-mydata1[3:2,], 2, cumsum)
xpos[1, ] <- xpos[1, ] + mydata1[3, ] / 2
xpos[2, ] <- xpos[2, ] + mydata1[2, ] / 2
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(mydata1[3:2, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "gray27")

xpos <- apply(mydata1[4:5,], 2, cumsum)
xpos[1, ] <- xpos[1, ] - mydata1[4, ] / 2
xpos[2, ] <- xpos[2, ] - mydata1[5, ] / 2
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(mydata1[4:5, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "gray27")

dev.off()
        