library("likert")
library("haven")
# Import data and prepare chart

#load("RData_files/pisaitems.rda")
pisaitems <- read_sav(file = "SAV_files/PISA/PISA 2009.sav")
pisaitems <- as.data.frame(pisaitems)
items28 = pisaitems[ , substr(names(pisaitems), 1, 5) == "ST24Q"]

pdf_file <- "PDF_files/barcharts_multiple_all_grouped.pdf"
cairo_pdf(bg = "grey98",
          pdf_file,
          width = 12,
          height = 19)

par(
    omi = c(1.0, 0.5, 1.75, 0.5),
    mai = c(0.1, 1.45, 0.35, 0.8),
    family = "Lato Light",
    las = 1
)


source("Scripts/scriptsanddata/scripts/inc_names_item28.r")

for (i in 1:ncol(items28))
{
    items28[, i] = factor(items28[, i], levels = 1:4, ordered = T)
}
source("Scripts/scriptsanddata/scripts/functions/lickert.r")
library(reshape)
lik = likert(items28, grouping = pisaitems$CNT)
x <- print(lik)
y <- cbind(x[, 1], x[, 3], x[, 4], x[, 5], x[, 6])
colours <- c("palevioletred4", "lightpink", "cornsilk1", "cornsilk4")
k <- length(y[, 1]) / length(unique(y[, 1]))
par(mfcol = c(k + 1, 1), las = 1)

for (i in 1:k)
{
    z <- y[c(i, i + k, i + 2 * k), ]
    prozcan_l <- format(round(z[1, 2] + z[1, 3], 0), nsmall = 0)
    prozmex_l <- format(round(z[2, 2] + z[2, 3], 0), nsmall = 0)
    prozusa_l <- format(round(z[3, 2] + z[3, 3], 0), nsmall = 0)
    prozcan_r <- format(round(z[1, 4] + z[1, 5], 0), nsmall = 0)
    prozmex_r <- format(round(z[2, 4] + z[2, 5], 0), nsmall = 0)
    prozusa_r <- format(round(z[3, 4] + z[3, 5], 0), nsmall = 0)
    b1 <- paste("Canada", "-", prozcan_l, "%", sep = " ")
    b2 <- paste("Mexico", "-", prozmex_l, "%", sep = " ")
    b3 <- paste("USA", "-", prozusa_l, "%", sep = " ")
    
    # Create chart
    
    barplot(
        t(z[, 2:5]),
        names.arg = c(b1, b2, b3),
        cex.names = 2,
        col = colours,
        horiz = T,
        axes = F
    )
    text(105.5,
         1.0 - 0.25,
         paste(prozcan_r, "%", sep = " "),
         xpd = T,
         cex = 2)
    text(105.5,
         2.2 - 0.25,
         paste(prozmex_r, "%", sep = " "),
         xpd = T,
         cex = 2)
    text(105.5,
         3.4 - 0.25,
         paste(prozusa_r, "%", sep = " "),
         xpd = T,
         cex = 2)
    text(
        0,
        4.3,
        names(items28)[i],
        cex = 2.1,
        xpd = T,
        adj = 0
    )
}

# Other elements

par(mai = c(1.1, 1.225, 0, 0.45))
plot(
    1:2,
    typ = "n",
    axes = F,
    xlim = c(0, 100),
    xlab = "",
    ylab = ""
)
axis(1, at = c(0, 25, 50, 75, 100), cex.axis = 2)
legend(
    -10,
    -0.5,
    pt.cex = 4,
    cex = 2.5,
    pch = 15,
    col = colours,
    ncol = 4,
    c("Strongly disagree", "Disagree", "Agree", "Strongly agree"),
    bty = "n",
    xpd = T
)

# Titling

mtext(
    "Reading attitude",
    3,
    line = 5.5,
    adj = 0,
    cex = 3.8,
    family = "Lato Black",
    outer = T
)
mtext(
    "How much do you disagree or agree with these statements about reading?",
    3,
    line = 2.2,
    adj = 0,
    cex = 2.0,
    outer = T
)
mtext(
    "Source: PISA 2009 Assessment Framework - Key Competencies in Reading, Mathematics, and Science",
    1,
    line = 1,
    adj = 1.0,
    cex = 1.25,
    outer = T
)
mtext(
    "© OECD 2009, Data: bryer.org",
    1,
    line = 3.5,
    adj = 1.0,
    cex = 1.25,
    outer = T
)
dev.off()

