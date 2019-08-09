# Adela Garcia
# 09/08/2019
# Clase 5

## Ho no existen diferencias entre medias de tratamientos 
## H1 al menos una media tiene diferencia entre los tratamientos 

arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

#Toneladas por hectarea
y.ton <- c(arena, arcilla, limo)
suelo <- gl(3, 10, 30, labels=c("arena", "arcilla", "limo"))

#productividad
prod <- data.frame(suelo, y.ton)
head(prod)

tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)

#normalidad de varianzas 
shapiro.test(prod$y.ton)

#pruebas para homogenidad de  varianzas, usar una u otra 
bartlett.test(prod$y.ton, prod$suelo)
fligner.test(prod$y.ton, prod$suelo)

#inspeccion visual de los datos 
boxplot(prod$y.ton ~ prod$suelo,
        xlab = "Tipo de suelo",
        ylab = "Ton/ha",
        col = "lightblue")

#fuente de variacion = suelo

#analisi de varianza (siglas en ingles)
aov.suelo <- aov(prod$y.ton ~ prod$suelo)
aov.suelo
summary(aov.suelo)

par(mfrow=c(2,2))
plot(aov(prod$y.ton ~ prod$suelo))
par(mfrow=c(1,1))

# Diferencias entre tratamientos mediante prueba de Tukey
TukeyHSD(aov.suelo, conf.level = 0.95)

#revision del modelo
plot(TukeyHSD(aov.suelo))
summary(aov.suelo)
summary.lm(aov.suelo)
## si existe una diferencia significativa entre tratamientos 


