#Adela Garcia
#09/08/2019
#Clase 4


# Coorrelación ------------------------------------------------------------

library(repmis)

erupciones <- read.csv("C:/MCF202-2019/Datos/erupciones.csv", header = T)


plot(erupciones$waiting , erupciones$eruptions, pch=19,
     col = "lightgreen",
     xlab = "Tiempo de espera (min)",
     ylab = "Duracion (min)")

## A mayor tiempo de espera, mayor sera la erupción
## se observa que no hay un origen muy claro de las erupciones 

library("pastecs")
stat.desc(erupciones$eruptions,basic=FALSE, norm=TRUE)

shapiro.test(erupciones$eruptions)
shapiro.test(erupciones$waiting)

cor.test(erupciones$eruptions, erupciones$waiting)

## Ho no es significativa
## H1 es significativa la coorelacion
## Los datos muestran una coorrelacion significativa entre las variables.

# Regresión lineal --------------------------------------------------------

## Hipotesis = La variable tiempo de espera ayuda a predecir la duracion del geiser 
## Ho no es significativa la prediccion
## H1 es significativa la prediccion 

## lm "lineal model"
lm.erup <- lm(erupciones$eruptions ~ erupciones$waiting)

plot(erupciones$waiting , erupciones$eruptions, pch=19,
     col = "lightgreen",
     xlab = "Tiempo de espera (min)",
     ylab = "Duracion (min)")
abline(lm.erup, col = "red")

## PARA LA FORMULA LM
## X = waitin, variable independiente
## Y = eruptions, variable dependiente

## PARA LA GRÁFICA 
## Y = eruptions, variable dependiente
## X = waitin, variable independiente


## Coeficientes de regresión
text(52, 4.5, "Y = -1.87 + 0.07 * x")
text(52, 4, "r^2 = 0.81")

## El tiempo de erupcion depende del tiempo de espera 

## Valores de la intercepcion de alfa y beta 
lm.erup
summary(lm.erup)

## coeficiente alfa = -1.87

length(erupciones$eruptions)
sqrt(0.90)
(0.90)^2

y.60 <- -1.87 + 0.07*60
y.60


# Datos regresión ---------------------------------------------------------

espera <- erupciones$waiting
duracion <- erupciones$eruptions
res <- resid(lm.erup)
pre <- fitted(lm.erup)
res.2 <- res^2

cuadro <- round(data.frame(espera, duracion, pre, res,
                     res.2), 4)
SSE <- sum(cuadro$res.2)
SSE

vari <- SSE/(length(erupciones$waiting)-2)
vari

var(duracion)

# Prueba de hipotesis de la regresión -------------------------------------

an.erup <- anova(lm.erup)
an.erup
## Se acepta la hipotesis alternativa, es decir, el modelo de regresion aplicado para 
## los datos de erupciones del volcan son diferentes a 0, por lo tanto es significativo.




