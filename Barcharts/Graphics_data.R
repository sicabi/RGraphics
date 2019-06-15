########### 1. DESANCLAR PAQUETES DE R Y BORRAR OBJETOS DEL AMBIENTE ####
rm(list = ls())
detach("package:memisc", unload = TRUE)
detach("package:foreign", unload = TRUE)
detach("package:Hmisc", unload = TRUE)
detach("package:haven", unload = TRUE)
########### 2. DESCOMPRIMIR ARCHIVO SAV (SPSS)
unzip("SAV_archivos/ZA4800_v4-0-0.sav.zip", setTimes = TRUE, 
      exdir = "SAV_archivos/")
unzip("SAV_files/WVS/F00005809-WV6_Data_spss_v_2016_01_01.zip",
      setTimes = TRUE,
      exdir = "SAV_archivos/")
######### 3. IMPORTING DATA WITH FOREIGN ########
# Name: Foreign, referring to external.  
# Author: R Core Team, Roger Bivand, Free Software Foundations et al.
# Last Update: 2018
# Recomende for: General application.
library("foreign")
system.time(
    ZA4800 <- read.spss(
        "SAV_archivos/ZA4800_v4-0-0.sav",
        use.value.labels = FALSE,
        to.data.frame = TRUE,
        use.missings = TRUE
    )
) 
# Elapsed time: 4.403 segundos.
# Data frame size: 243.3 MB
# Warnings: 4
germany <- ZA4800[ZA4800$country == 276, ]
mydata <- subset(germany, 
                 select = c(v159, v160, v161, v162, v163, v164, v165, v302))
attach(mydata)
detach("package:foreign", unload = TRUE)
remove(germany, ZA4800)
######### IMPORTING DATA WITH HMISC ########
# Name: Harrel Miscelaneus  
# Author: Frank E. Harrell
# Last Update: 2018
# Recomende for: working with Biostatistics and medicine
library("Hmisc")
unzip(zipfile = "SAV_files/ZA4800_v4-0-0.sav.zip", 
      exdir = "SAV_files/") 
system.time(ZA4800 <- spss.get("SAV_files/EVS/Datasets/EVS_80-08/ZA4800_v4-0-0.sav", 
                   use.value.labels = FALSE))
# Elapsed time: 6.971 secs
# Data frame size: 127.3 MB
# Warnings: 4
germany <- ZA4800[ZA4800$country == 276, ]
mydata <- subset(germany, 
                 select = c(v159, v160, v161, v162, v163, v164, v165, v302))
attach(mydata)
detach("package:Hmisc", unload = TRUE)
remove(germany, ZA4800)
file.remove("SAV_files/ZA4800_v4-0-0.sav")
######### IMPORTING DATA WITH MEMISC ########
# Name: Management of Survey Data and Presentation of Analysis Results.
# Author:  Martin Elff (with contributions from Christopher N. Lawrence, 
# Dave Atkins.
# Jason W. Morgan, Achim Zeileis)
# Last Update: 2018
# Recommended for: The Manifiesto Project, GESIS surveys
library("memisc")
system.time(ZA4800 <- spss.system.file("SAV_files/EVS/Datasets/ZA4800_v4-0-0.sav"))
# Elapsed time: 0.38 secs
# Data frame size: 7.9 MB
# Warnings: 14
mydata <- subset(ZA4800, 
                 select = c(country, 
                            v159, v160, v161, v162, v163, v164, v165, v302))
mydata <- as.data.frame(subset(mydata, country == "Germany"))
attach(mydata)
detach("package:memisc", unload = TRUE)
remove(germany, ZA4800)
########## IMPORTING DATA WITH HAVEN ########
#Name: Haven stands for refuge. Maybe it stands as a 'haven' for
# safe-importing data. 
# Author: Hadley Wickham. Date: 2018
# Last update: 2018
library(haven) 
system.time(ZA4800 <- read_sav("SAV_archivos/ZA4800_v4-0-0.sav"))
# Elapsed time: 8.248 secs
# Data frame size: 244.2 MB
# Warnings: 0
germany <- ZA4800[ZA4800$country == 276, ]
mydata <- subset(germany, 
                 select = c(v159, v160, v161, v162, v163, v164, v165, v302))
attach(mydata)
detach("package:haven", unload = TRUE)
remove(germany, ZA4800)
# Datos para gráficos sobre México Hmisc
library("Hmisc")
unzip(zipfile = "SAV_files/WVS/F00005809-WV6_Data_spss_v_2016_01_01.zip", 
      exdir = "SAV_files/") 
WV6_Data_v2016 <- spss.get("SAV_archivos/WV6_Data_spss_v_2016_01_01.sav", 
                           use.value.labels = FALSE)
mexico <- WV6_Data_v2016[WV6_Data_v2016$V2 == 484, ]
misdatos <- subset(mexico, 
                   select = c(V45, V47, V48, V50, V51, V52, V53, V54, V240))
attach(misdatos)
detach(name = "package:Hmisc", unload = TRUE)
remove(mexico, WV6_Data_v2016)
file.remove("SAV_archivos/WV6_Data_spss_v_2016_01_01.sav")
# Datos para gráficos sobre México memisc
library("memisc")
system.time(misdatos <- 
                spss.system.file("SAV_files/ZA3811_v3-0-0.sav"))
# Elapsed time: 0.38 secs
# Data frame size: 7.9 MB
# Warnings: 14
misdatos <- subset(misdatos, 
                 select = c(v2, v45, v47, v48, v50, v51, v52, v53, v54, v240))
misdatos <- subset(misdatos, v2 == "Mexico")
misdatos$v2 <- NULL
attach(misdatos)
detach(misdatos)