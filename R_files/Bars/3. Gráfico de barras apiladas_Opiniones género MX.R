attach(misdatos)

v45 <- "Cuando hay poco empleo, los hombres deberían tener más derecho 
        a trabajar que las mujeres*"
v47 <- "Si una mujer gana más dinero que su esposo, es casi seguro que eso les 
        causará problemas*"
v48 <- "Tener un trabajo es la mejor manera para que una mujer pueda llegar 
        a ser independiente*"
v50 <- "Cuando una mujer trabaja por un salario sus hijos sufren"
v51 <- "En cojunto, los hombres toman mejores decisiones políticas que las 
        mujeres"
v52 <- "Tener educación universitaria es más importante para un niño que para 
        una niña"
v53 <- "En conjunto, los hombres toman mejores decisiones de negocios que las 
        mujeres"
v54 <- "Ser ama de casa es tan satisfactorio como trabajar por un salario"

etiquetas <- c(v45, v47, v48, v50, v51, v52, v53, v54)

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

respuestas <-
    c("de acuerdo",
      "en desacuerdo",
      "ni de acuerdo ni en desacuerdo* / no sabe / no conestó")

cairo_pdf(
    "PDF_archivos/Apilada_género_méxico.pdf",
    width = 13,
    height = 10.5,
    bg = "grey98"
)
par(
    omi = c(0, 0.75, 1.25, 0.75),
    mai = c(1.6, 3.75, 0.5, 0),
    lheight = 1.15,
    family = "Lato Light",
    las = 1
)


color1 <- rgb(109, 221, 225, maxColorValue = 255)
color2 <- rgb(255, 138, 238, maxColorValue = 255)
mycolors <- c(color1, color2, "grey")

mydata0 <- cbind(s[, 1], s[, 2], s[, 3])
mydata1 <- t(mydata0)

bp <-
    barplot(
        mydata1,
        names.arg = etiquetas,
        cex.names = 0.75,
        horiz = T,
        border = NA,
        xlim = c(0, 100),
        col = mycolors,
        axes = FALSE,
        family = "Lato"
    )

xpos <- apply(mydata1,  2, cumsum)
xpos <- xpos - mydata1 / 2
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(mydata1), 0), nsmall = 0),
     cex = 0.75,
     col = "gray27") 

mtext(
    c(0, 20, 40, 60, 80, 100),
    at = c(0, 20, 40, 60, 80, 100),
    1,
    line = 0,
    cex = 0.90
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
    "Opiniones con respecto a los roles de género en México (2012)",
    3,
    line = 2.2,
    adj = 0,
    cex = 1.8,
    outer = TRUE,
    family = "Lato Black"
)




mtext(
    "Valores en porcentajes (%), decimales redondeados a la unidad",
    1,
    line = 2,
    adj = 1,
    cex = 0.95,
    font = 3
)

mtext(
    "* Si contestó ni de acuerdo ni en desacuerdo se agregregó con NS/NC",
    1,
    line = 3,
    adj = 1,
    cex = 0.95,
    font = 3
)

mtext(    "Fuente: Encuesta Mundial de Valores, Sexto levantamiento. 
    Base de datos: WV6_Data_v2016, consultada en http://www.worldvaluessurvey.org/ el 14 de marzo de 2018",
      1,
      line = 5.5,
      adj = 1,
      cex = 0.95,
      font = 3
)

px <-
    c(3, 25, 45)
py <- rep(10,3)
tx <- c(6, 28, 48)
ty <- rep(10, 3)
points(
    px,
    py,
    pch = 15,
    cex = 4,
    col = mycolors,
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

dev.off() 