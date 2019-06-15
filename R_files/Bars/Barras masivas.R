library("Hmisc")
WV6_Data_v2016 <- spss.get("SAV_archivos/WV6_Data_spss_v_2016_01_01.sav", 
                           use.value.labels = FALSE)
mexico <- WV6_Data_v2016[WV6_Data_v2016$V2 == 484, ]
misdatos <- subset(mexico, 
                   select = c(V45, V47, V48, V50, V51, V52, V53, V54, V240))
attach(misdatos)
detach(name = "package:Hmisc", unload = TRUE)
remove(mexico, WV6_Data_v2016)


pdf_file <- "PDF_archivos/barras y simbolos masivos.pdf"
cairo_pdf(bg = "grey98",
          pdf_file,
          width = 13,
          height = 10.5)

par(
    omi = c(0.65, 0.65, 0.85, 0.85),
    mai = c(1.1, 5.85, 1.55, 0),
    family = "Lato Light",
    las = 1
)  

# Prepare chart
col_f <- rgb(255, 97, 0, 190, maxColorValue = 255)
col_m <- rgb(68, 90, 111, 190, maxColorValue = 255)
v45 <- "Cuando hay poco empleo, los hombres deberían tener más\n derecho a trabajar que las mujeres"
v47 <- "Si una mujer gana más dinero que su esposo, es casi seguro \nque eso les causará problemas"
v48 <- "Tener un trabajo es la mejor manera\n para que una mujer pueda llegar a ser independiente"
v50 <- "Cuando una mujer trabaja por un salario sus hijos sufren"
v51 <- "En cojunto, los hombres toman mejores decisiones políticas\n que las mujeres"
v52 <- "Tener educación universitaria es más\nimportante para un niño que para una niña"
v53 <- "En conjunto, los hombres toman mejores\ndecisiones de negocios que las mujeres"
v54 <- "Ser ama de casa es tan satisfactorio como trabajar por un salario"

etiquetas <- c(v45, v47, v48, v50, v51, v52, v53, v54)

# Create chart
plot(
    1:5 ,
    type = "n" ,
    axes = F,
    xlab = "",
    ylab = "",
    xlim = c(0, 20),
    ylim = c(1, 6)
)
mySymbols <- function(n_f, n_m, y, labelling, ...) {
    par(family = "Symbol Signs")
    for (i in 1:n_f)
    {
        text(runif(1, 0, (n_f + n_m) / 10),
             runif(1, y, y + 1),
             "F",
             cex = 3.25,
             col = col_f)
    }
    for (i in 1:n_m)
    {
        text(runif(1, 0, (n_f + n_m) / 10),
             runif(1, y, y + 1),
             "M",
             cex = 3.25,
             col = col_m)
    }
    par(family = "Lato Light")
    text(-3,
         y + 0.5,
         labelling,
         xpd = T,
         cex = 1.45,
         adj = 1)
}

tabla_v54 <- table(V54, V240)
tabla_v53 <- table(V53, V240)
tabla_v52 <- table(V52, V240)

mySymbols(round((tabla_v54[1, 2] + tabla_v54[2, 2]) / 10), 
          round((tabla_v54[1, 1] + tabla_v54[2, 1]) / 10), 
          1, 
          v54)
mySymbols(round((tabla_v53[1, 2] + tabla_v53[2, 2]) / 10), 
          round((tabla_v53[1, 1] + tabla_v53[2, 1]) / 10), 
          3, 
          v53)
mySymbols(round((tabla_v52[1, 2] + tabla_v52[2, 2]) / 10), 
          round((tabla_v52[1, 1] + tabla_v52[2, 1]) / 10), 
          5, 
          v52)

axis(
    1,
    at = c(0, 5, 10, 15, 20),
    labels = c("0", "500", "1,000", "1,500", "2,000"),
    col = par("bg"),
    col.ticks = "grey81",
    lwd.ticks = 0.5,
    tck = -0.025
)

# Other elements

abline(v = c(0, 5, 10, 15, 20), lty = "dotted")

# Titling

mtext(
    "A menudo se dice que los roles de género están cambiando",
    3,
    line = -0.5,
    adj = 0,
    cex = 1.8,
    family = "Lato Black",
    outer = T
)
mtext(
    "Muy de acuerdo / de acuerdo",
    3,
    line = -3,
    adj = 0,
    cex = 1.8,
    outer = T,
    font = 3
)
mtext(
    "Fuente: WVS 2016 México, WV6_Data_v2016",
    1,
    line = 0,
    adj = 1,
    cex = 1.5,
    outer = T,
    font = 3
)
mtext(
    "2,000 encuestados. Cada símbolo representa 10 personas ",
    1,
    line = -2,
    adj = 0,
    cex = 1.5,
    outer = T,
    font = 3
)

par(family = "Lato Light")
mtext(
    "Mujeres",
    1,
    line = 1,
    adj = 0.02,
    cex = 1.5,
    outer = T,
    font = 3
)
mtext(
    "Hombres",
    1,
    line = 1,
    adj = 0.12,
    cex = 1.5,
    outer = T,
    font = 3
)
par(family = "Symbol Signs")
mtext(
    "F",
    1,
    line = 1,
    adj = 0,
    cex = 2.5,
    outer = T,
    font = 3,
    col = col_f
)
mtext(
    "M",
    1,
    line = 1,
    adj = 0.1,
    cex = 2.5,
    outer = T,
    font = 3,
    col = col_m
)
dev.off()
