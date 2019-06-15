# Apiladas alrededor del cero
# Adjuntar datos
#attach(misdatos)
# Definir etiquetas del gráfico
v45 <- "Cuando hay poco empleo, los hombres deberían 
tener más derecho a trabajar que las mujeres*"
v47 <- "Si una mujer gana más dinero que su esposo, 
es casi seguro que eso les causará problemas*"
v48 <- "Tener un trabajo es la mejor manera para
que una mujer pueda llegar a ser independiente*"
v50 <- "Cuando una mujer trabaja por un
salario sus hijos sufren"
v51 <- "En cojunto, los hombres toman mejores 
decisiones políticas que las mujeres"
v52 <- "Tener educación universitaria es más importante 
para un niño que para una niña"
v53 <- "En conjunto, los hombres toman mejores decisiones 
de negocios que las mujeres"
v54 <- "Ser ama de casa es tan satisfactorio como
trabajar por un salario"
etiquetas <- c(v45, v47, v48, v50, v51, v52, v53, v54)
respuestas <-
    c("ni de acuerdo ni en desacuerdo/no sabe/no contestó",
      "de acuerdo",
      "en desacuerdo")
# Seleccionar variables que se usarán en la gráfica
z <- NULL
y <- 100 * table(as.matrix(V45), exclude = FALSE) / length(V45)
z <- rbind(z, y[1:4])
y <- 100 * table(as.matrix(V47), exclude = FALSE) / length(V47)
z <- rbind(z, y[1:4])
y <- 100 * table(as.matrix(V48), exclude = FALSE) / length(V48)
z <- rbind(z, y[1:4])
q <- matrix(nrow = 3, ncol = 3, byrow = FALSE, data = c(z[,1],z[,3],z[,2] + z[,4]))
z <- NULL
y <- 100 * table(as.matrix(V50), exclude = FALSE) / length(V50)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(V51), exclude = FALSE) / length(V51)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(V52), exclude = FALSE) / length(V52)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(V53), exclude = FALSE) / length(V53)
z <- rbind(z, y[1:5])
y <- 100 * table(as.matrix(V54), exclude = FALSE) / length(V54)
z <- rbind(z, y[1:5])
r <- matrix(nrow = 5, ncol = 3, byrow = FALSE, data = c(z[,1] + z[ ,2], z[,3] + z[ ,4], z[ ,5]))
s <- rbind(q, r)
# Construir gráfico
cairo_pdf( # Definir archivo y formato de exportación
    "PDF_files/Apilada alrededor del cero.pdf",
    width = 13,
    height = 10.5,
    bg = "grey98"
)
par( # Definir tamaño y márgenes del gráfico
    omi = c(0.25, 0.75, 1, 0.75),
    mai = c(1.8, 3.75, 0.25, 0),
    lheight = 1.15,
    family = "Lato Light",
    las = 1
)
color1 <- rgb(109, 221, 225, maxColorValue = 255)
color2 <- rgb(255, 153, 156, maxColorValue = 255)
miscolores <- c("grey", color1, color2)
misdatos0 <- NULL
misdatos0 <- cbind(s[, 3], s[, 1], s[, 2])
misdatos1 <- t(misdatos0)
bp <- barplot(
    -rep(100, 8), # Número de etiquetas
    names.arg = etiquetas,
    cex.names = 0.9,
    horiz = T,
    border = par("bg"),
    xlim = c(-100, 70),
    col = miscolores[1],
    axes = F
)
bp <- barplot(
    -(100 - misdatos1[1, ]),
    names.arg = etiquetas,
    cex.names = 0.9,
    horiz = T,
    border = par("bg"),
    xlim = c(-100, 70),
    col = par("bg"),
    axes = F,
    add = T
)
bp <- barplot(
    -misdatos1[2, ],
    names.arg = etiquetas,
    cex.names = 0.9,
    horiz = T,
    border = NA,
    xlim = c(-100, 70),
    col = miscolores[2],
    axes = F,
    add = T
)
bp <- barplot(
    misdatos1[3, ],
    names.arg = etiquetas,
    cex.names = 0.9,
    horiz = T,
    border = NA,
    xlim = c(-100, 70),
    col = miscolores[3],
    axes = F,
    add = T)
# Leyendas del gráfico
arrows(
    0,
    -0.04,
    0,
    9.75,
    lwd = 2.5,
    length = 0,
    xpd = T,
    col = "skyblue3"
) 
mtext(
    c(80, 60, 40, 20, 0, 20, 40, 60, 80) ,
    at = c(-80, -60, -40, -20, 0, 20, 40, 60, 80),
    1,
    line = 0,
    cex = 0.95
)
mtext(
    "N = 2,000",
    1,
    line = 2,
    adj = 0,
    cex = 1.15,
    family = "Lato",
    font = 3
)
mtext(
    "*De acuerdo ni en desacuerdo se agregregó con NS/NC",
    1,
    line = 3,
    adj = 0,
    cex = 1,
    family = "Lato",
    font = 3
)
mtext(
    "Opiniones con respecto a los roles de género en México (2012)",
    3,
    line = 2.2,
    adj = 1,
    cex = 1.8,
    outer = TRUE,
    family = "Lato Black"
)
mtext(
    "Valores en porcentajes (%), decimales redondeados a la unidad",
    1,
    line = 1,
    adj = 0,
    cex = 0.95,
    font = 3
)
mtext("Fuente: Encuesta Mundial de Valores, Sexto levantamiento. 
    Base de datos: WV6_Data_v2016, consultada en http://www.worldvaluessurvey.org/ el 14 de marzo de 2018",
      1,
      line = 6,
      adj = 1,
      cex = 0.95,
      font = 3
)
#Etiquetqs para las barras
xpos <- -(100 - misdatos1[1, ])
dist <- (xpos + 100)/2
xpos <- xpos - dist
xpos <- t(xpos)
text(xpos + 3,
     bp,
     format(round(t(misdatos1[1, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "black")

xpos <- misdatos1[3, ]/2
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(misdatos1[3, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "black")

xpos <- -misdatos1[2, ]/2
xpos <- t(xpos)
text(xpos + 3,
     bp,
     format(round(t(misdatos1[2, ]), 1), nsmall = 1),
     cex = 0.75,
     col = "black")
# Añadir leyendas de los colores
px <- c(-95, 0, 40)
py <- rep(10.2,3)
tx <- c(-90, 5, 45)
ty <- rep(10.2, 3)
points(
    px,
    py,
    pch = 15,
    cex = 4,
    col = miscolores,
    xpd = T
)
text(
    tx,
    ty,
    respuestas,
    adj = 0,
    xpd = TRUE,
    family = "Lato Light",
    font = 3,
    cex = 1
)
# Cerrar archivo
dev.off()