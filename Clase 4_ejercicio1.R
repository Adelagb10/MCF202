#Adela Garcia
#09/08/2019
#Clase 4

#Ejercicio 1

ebanos <- read.csv("C:/MCF202-2019/Datos/ebanos.csv", header = T)

plot(ebanos$diametro , ebanos$altura, pch=17,
     col = "lightblue",
     xlab = "Diametro (cm)",
     ylab = "Altura (cm)")

## A mayor diámetro, mayor será la altura

## Desglose de datos de la variable dependiente 
library("pastecs")
stat.desc(ebanos$altura,basic=FALSE, norm=TRUE)

## HO Las variables diámetro y altura no tendrán coorrelación 
## H1 Las variables diámetro y altura tendrán coorrelación

shapiro.test(ebanos$diametro)
shapiro.test(ebanos$altura)

## Prueba para comprobar si losdatos pueden normalizarse
shapiro.test(log(ebanos$diametro))
## Valor de p(1.689e-05) menor al alfa (0.05)

## Prueba de coorrelación 
cor.test(ebanos$diametro, ebanos$altura)
## Mediante la formula de coorrelación el valor de p (2.2e-16) es menor al 
## valor alfa (0.05), por lo tanto la coorrelación es significativa 
## entre ambas varibles (diametro y altura), es decir se acepta la hipótesis 
## alternativa.






