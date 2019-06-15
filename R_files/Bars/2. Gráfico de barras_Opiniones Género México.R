# Gráfico de barras que agrupa dos categorías
attach(misdatos)

v45 <- "Cuando hay poco empleo, los hombres deberían tener más\n derecho a trabajar que las mujeres"
v47 <- "Si una mujer gana más dinero que su esposo, es casi seguro \nque eso les causará problemas"
v48 <- "Tener un trabajo es la mejor manera\n para que una mujer pueda llegar a ser independiente"
v50 <- "Cuando una mujer trabaja por un salario sus hijos sufren"
v51 <- "En cojunto, los hombres toman mejores decisiones políticas\n que las mujeres"
v52 <- "Tener educación universitaria es más\nimportante para un niño que para una niña"
v53 <- "En conjunto, los hombres toman mejores\ndecisiones de negocios que las mujeres"
v54 <- "Ser ama de casa es tan satisfactorio como trabajar por un salario"

etiquetas <- c(v45, v47, v48, v50, v51, v52, v53, v54)

cairo_pdf(
    "PDF_archivos/genero méxico.pdf",
    width = 9,
    height = 6.5,
    bg = "grey98"
)

par(
    omi = c(0, 0.75, 1.25, 0.75),
    mai = c(0.9, 3.85, 0.55, 0),
    lheight = 1.15,
    family = "Lato Light",
    las = 1
)


z <- NULL
y <- as.matrix(prop.table(table(V45))*100)
z <- c(z, y[1])
y <- as.matrix(prop.table(table(V47))*100)
z <- c(z, y[1])
y <- as.matrix(prop.table(table(V48))*100)
z <- c(z, y[1])
y <- as.matrix(prop.table(table(V50))*100)
z <- c(z, y[1] + y[2])
y <- as.matrix(prop.table(table(V51))*100)
z <- c(z, y[1] + y[2])
y <- as.matrix(prop.table(table(V52))*100)
z <- c(z, y[1] + y[2])
y <- as.matrix(prop.table(table(V53))*100)
z <- c(z, y[1] + y[2])
y <- as.matrix(prop.table(table(V54))*100)
z <- c(z, y[1] + y[2])

bp <-
    barplot(
        z,
        names.arg = F,
        horiz = T,
        border = NA,
        xlim = c(0, 100),
        col = "grey",
        axes = FALSE,
        family = "Lato"
    )

mycolor <- rgb(255, 0, 210, maxColorValue = 255)
rect(
    0,
    -0.1,
    20,
    9.9,
    col = rgb(191, 239, 255, 80, maxColorValue = 255),
    border = NA
)
rect(
    20,
    -0.1,
    40,
    9.9,
    col = rgb(191, 239, 255, 80, maxColorValue = 255),
    border = NA
)
rect(
    40,
    -0.1,
    60,
    9.9,
    col = rgb(191, 239, 255, 80, maxColorValue = 255),
    border = NA
)
rect(
    60,
    -0.1,
    80,
    9.9,
    col = rgb(191, 239, 255, 80, maxColorValue = 255),
    border = NA
)

rect(
    80,
    -0.1,
    100,
    9.9,
    col = rgb(191, 239, 255, 80, maxColorValue = 255),
    border = NA
)
z2 <- c(0, 0, 61.45, 0, 0, 0, 0, 0, 0)

bp <-
    barplot(
        z2,
        names.arg = F,
        horiz = T,
        border = NA,
        xlim = c(0, 100),
        col = mycolor,
        axes = FALSE,
        add = TRUE
    )

for (i in 1:length(etiquetas)) {
    if (i == 3) {
        myFont <- "Lato Bold"
    } else {
        myFont <- "Lato Light"
    }
    text(
        -3,
        bp[i],
        etiquetas[i],
        xpd = TRUE,
        adj = 1,
        family = myFont,
        cex = 0.75
    )
    text(
        10,
        bp[i],
        format(round(z[i], 1), nsmall = 1),
        family = myFont,
        cex = 1.25,
        col = ifelse(i == 3, "white", "black")
    )
}

arrows(50,
       -0.1,
       50,
       9.9,
       lwd = 1.5,
       length = 0,
       xpd = TRUE,
       col = "skyblue3")
arrows(50,
       9.9,
       50,
       10.1,
       lwd = 1.5,
       length = 0,
       xpd = TRUE,
       col = "black")
arrows(50,
       -0.1,
       50,
       -0.3,
       lwd = 1.5,
       length = 0,
       xpd = TRUE,
       col = "black")

text(
    44,
    20.5,
    "45",
    adj = 1,
    xpd = TRUE,
    cex = 0.65,
    family = "Lato",
    font = 4
)
text(
    48,
    10.1,
    "Mayoría",
    adj = 1,
    xpd = TRUE,
    cex = 0.9,
    font = 3
)
text(
    52,
    10.1,
    "50%",
    adj = 0,
    xpd = TRUE,
    cex = 0.9,
    family = "Lato Bold",
    font = 3
)
text(
    100,
    10.1,
    "Valores en porcentaje(%)",
    adj = 1,
    xpd = TRUE,
    cex = 0.65,
    font = 3
)
mtext(
    c(0, 20, 40, 60, 80, 100),
    at = c(0, 20, 40, 60, 80, 100),
    1,
    line = 0,
    cex = 0.75
)

mtext(
    "Opiniones con respecto a los roles de género en México (2012)",
    3,
    line = 2.2,
    adj = 0,
    cex = 1.5,
    family = "Lato Black",
    outer = TRUE
)

mtext(
    "De acuerdo o algo de acuerdo",
    3,
    line = 0,
    adj = 0,
    cex = 1.5,
    outer = TRUE
)
mtext(
    "Fuente: Encuesta Mundial de Valores, Sexto levantamiento. 
    Base de datos: WV6_Data_v2016, consultada en http://www.worldvaluessurvey.org/ el 14 de marzo de 2018",
    1,
    line = -1.5,
    adj = 1,
    cex = 0.95,
    outer = TRUE,
    font = 3
)

dev.off() 