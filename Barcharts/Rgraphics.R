library(Cairo)
library(Hmisc)
# Crea un gráfico de barras con una línea de tendencia ajustada al centro. Debe
# incluirse el gráfico de barras dentro de la función de lineas para que la lí-
# nea quede al centro.
par(bg = "lightyellow")
bar <- c(1, 4, 3, 4)
line <- bar / 2
bp <- barplot(bar)
lines(line, col = "red")
lines(bp, line, col = "blue")

# Dividir la pantalla en dos columnas
par(mfcol = c(1, 2))
plot(1:10)
plot(1:10, axes = FALSE)
axis(1)
axis(2)
box(lty = "solid")

# Ver los símbolos disponibles en R
plot(
    1:25,
    rep(1, 25),
    cex = 2,
    pch = 1:25,
    axes = FALSE,
    xlab = "",
    ylab = "",
    ylim = c(0, 1.5)
)
text(1:25, rep(1.15, 25), 1:25, srt = 0)

# Hacer un scatterplot con histogramas laterales.

nf <-
    layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)
x <- pmin(3, pmax(-3, stats::rnorm(50)))
y <- pmin(3, pmax(-3, stats::rnorm(50)))
xhist <- hist(x, breaks = seq(-3, 3, 0.5), plot = FALSE)
yhist <- hist(y, breaks = seq(-3, 3, 0.5), plot = FALSE)
top <-  max(c(xhist$counts, yhist$counts))
par(mai = c(1, 1, 0.2, 0.2))
plot(
    x,
    y,
    xlim = c(-3, 3),
    ylim = c(-3, 3),
    xlab = "",
    ylab = ""
)
par(mai = c(0, 1, 0.2, 0.2))
barplot(xhist$counts,
        axes = FALSE,
        ylim = c(0, top),
        space = 0)
par(mai = c(1, 0, 0.2, 0.2))
barplot(
    yhist$counts,
    axes = FALSE,
    xlim = c(0, top),
    space = 0,
    horiz = TRUE
)

# 5 scatterplots distribuidos

nl <- layout(
    matrix(
        data = c(1, 2, 3, 4, 5),
        nrow = 1,
        ncol = 5
    ),
    widths = c(2, 1, 1, 1, 1),
    heights = c(1, 1)
)
par(mai = c(0.5, 1, 0.5, 0),
    omi = c(0.25, 0.25, 0.25, 0.25))
x <- pmin(3, pmax(-3, rnorm(50)))
y <- pmin(3, pmax(-3, rnorm(50)))

plot(
    x,
    y,
    xlim = c(-3, 3),
    ylim = c(-3, 3),
    axes = F,
    col = 1,
    xlab = "",
    ylab = "y-axis-\nlabel"
)
axis(1)
axis(2)
box(lty = "solid", col = "darkgrey")
par(mai = c(0.5, 0, 0.5, 0))
for (i in 2:5) {
    x <- pmin(3, pmax(-3, rnorm(50)))
    y <- pmin(3, pmax(-3, rnorm(50)))
    plot(
        x,
        y,
        xlim = c(-3, 3),
        ylim = c(-3, 3),
        axes = F,
        col = i,
        xlab = ""
    )
    if (i %% 2 == 0) {
        axis(3)
    } else {
        axis(1)
    }
    box(lty = "solid", col = "darkgrey")
}

#Use a font in R
par(family = "Courier")
plot(1:10, main = "Hello World", cex.main = 3)


#Export unicode symbols to R

cairo_pdf(filename = "unicode_symbols_r_xlsx.png",
          width = 9,
          height = 3)
par(
    family = "Symbola",
    mfcol = c(1, 2),
    mai = c(0.25, 0, 0.25, 0),
    omi = c(0.25, 0, 0.25, 0),
    bg = "aliceblue"
)
files <- "Unicodeblock_different_symbols.xlsx"
files <- c(files, "Unicode_different_pictographic_symbols.xlsx")
for (i in 1:2) {
    data <- read.xls(paste("data/", files[i], sep = ""))
    print(data)
    attach(data)
    plot(
        No,
        = 1:-5,
        type = "n",
        axes = FALSE,
        xlab = "",
        ylab = ""
    )
    text(1, -No, i - 1, cex = 1.5, xpd = T)
    text(1.5, -No, i - 1, Characters, cex = 1.5, xpd = T)
    text(2, -No, Name, adj = 0, xpd = T)
}
dev.off()


#Imprimir unicode
library(RMySQL)
con <- dbConnect(
    MySQL(),
    user = "xxx",
    password = "xxx",
    dbname = "xxx",
    host = "xxx"
)
sqlset <- "SET NAMES utf8mb4" do <- dbGetQuery (con, sqlset)
sqldata <  –“select   from unicodetest" data < –dbGetQuery(con, sqldata) cairo_pdf(
filename = “unicode_symbols_r_mysql.png",
width = 9,
&
    height =
    3
)
par(family =  “Symbola",
    mai = c(0.25, 0, 0.25, 0),
    omi = c(0.25, 0, 0.25, 0) &
    ,
    bg = “aliceblue")
print(data)
attach(data)
plot(
    1:1,
    type = "n",
    axes = F,
    xlim = c(0, 4),
    ylim = c(–8, 0),
    xlab =  "",
    &
        ylab = ""
)
text(1.5,  –No, character, cex = 1.5, xpd = T) text(2,  –No, name, adj =
                                                        0, xpd = T) dev.off()
#Instalación de paquetes gráficos


