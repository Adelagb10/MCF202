#Adela Garcia
#09/08/2019
#Clase 6

#Instalas librería gapminder

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")

head(edad)
str(edad)

#Identificar columna SP como factor
edad$SP <- factor(edad$SP)
str(edad)


# Separar factor ----------------------------------------------------------

ariz <- subset(edad, SP == "arizonica")
ariz.lm <- (ariz$EDAD ~ ariz$DAP)
summary(ariz.lm)

dura <- subset(edad, SP == "durangensis")



# Regresión dos factores --------------------------------------------------

cov.edad <- lm(edad$EDAD ~ edad$DAP + edad$SP)
summary(cov.edad)

#Gráfica de datos
plot(edad$DAP[edad$SP == "arizonica"], edad$EDAD[edad$SP =="arizonica"],
     col = "red",
     pch = "A",
     xlim=c(0,50),
     ylim=c(0, 130))
abline(cov.edad$coefficients[1], cov.edad$coefficients[2], 
       col = "red")
text(30, 20, "Ya = -7.65 + 1.98 * x")


points(edad$DAP[edad$SP == "durangensis"], edad$EDAD[edad$SP =="durangensis"],
       col = "blue",
       pch = "D")
abline(cov.edad$coefficients[1] + cov.edad$coefficients[3], cov.edad$coefficients[2],
       col = "blue", lty = "dashed")
text(19, 100, "Yd = 11.41 + 1.98* x")

## Ho no existe diferencia entre la lineas de regresión
## H1 existe diferencia entre lineas de regresión

## Se acepta la hipotesis alternativa ya que el valor de p (2.2e-16) es menor al valor de p (0.05)
## por lo tanto si existen diferencias significativas







