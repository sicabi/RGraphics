#Etiquetas de las barras
xpos <- apply(mydata1, 2, cumsum)
xpos <- xpos - mydata1 / 2
xpos <- t(xpos)
text(xpos,
     bp,
     format(round(t(mydata1), 1), nsmall = 1),
     cex = 0.75,
     col = "gray27") 


px <-
    c(3, 23, 38, 56, 78)
py <- rep(9, 5)
tx <- c(20, 35.5, 53.5, 75.5, 98.5)
ty <- rep(9, 5)
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
    myresponses,
    adj = 1,
    xpd = TRUE,
    family = "Lato Light",
    font = 3,
    cex = 1
)
mtext(
    c(0, 20, 40, 60, 80, 100),
    at = c(0, 20, 40, 60, 80, 100),
    1,
    line = 0,
    cex = 0.90
)
mtext(
    "N = 2,075",
    1,
    line = 2,
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
    "Valores en porcentajes (%)",
    1,
    line = 2,
    adj = 1,
    cex = 0.95,
    font = 3
)

mtext("Fuente: Estudio de Valores Europeo, 2008.
      Base de datos: ZA4800, consultada en www.gesis.org el 14 de marzo de 2018",
      1,
      line = 4.5,
      adj = 1,
      cex = 0.95,
      font = 3
)