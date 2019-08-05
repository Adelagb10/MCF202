# Adela Garcia
# 05/08/2019
# Clase 0

# Pasos básicos -----------------------------------------------------------

2+2
a <- 2
a + a
a + 5


# Importar datos ----------------------------------------------------------

diametro <- c(12, 8.6, 9.2, 7.7, 12.9, 11.7, 9.7, 14.2,
              11.8, 14.3, 12.5)
diametro

#Medidas de tendencia central
mean(diametro)
median(diametro)
fivenum(diametro)

#Medidas de dispersión
sd(diametro)
var(diametro)


# Gráficas ----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col="lightblue", main="Diametro",
        xlab="D (cm)")

# Importado excel ---------------------------------------------------------

DB_alturas <- read.csv("C:/MCF202-2019/Datos/alturas.csv", header = T) 
head(DB_alturas)
boxplot(DB_alturas$crecimiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento, 
col="lightgreen",
xlab = "Tratamientos",
ylab = "Crecimiento (cm)",
main = "Efectos del fertilizante")

mean(DB_alturas$crecimiento)

# Restricciones -----------------------------------------------------------
sum(DB_alturas$crecimiento < mean(DB_alturas$crecimiento))
TratA <- DB_alturas[!(DB_alturas$tratamiento == "TA"),]

mean(TratA$crecimiento)



# Sumbmuetra --------------------------------------------------------------

T.mean <- subset(DB_alturas,crecimiento >= mean(DB_alturas$crecimiento))
boxplot(T.mean$crecimiento ~ T.mean$tratamiento)
