rm(list = ls(all.names = TRUE))
gc()
#--------------------------------------*----------------------------------------
#------------------------------- Ejercicio 3 -----------------------------------
#------------------------ Modelos Lineales Generalizados -----------------------
#------------------------------ datos binarios  --------------------------------
#--------------------------------------*----------------------------------------
library(tidyverse)

#-------------- Cargamos datos y un breve analisis descriptivo -----------------

datos <- read.csv("Preg3B.csv", header = TRUE)
summary(datos) # estadisticas de las variables
str(datos) # Vistazo del dataframe

#Convertimos a tipo factor la variable Insecticide y Deposit
datos$Insecticide <- factor(datos$Insecticide) # 3 niveles (A,B,C) 6 obs de cada una
datos$Deposit <- factor(datos$Deposit) # 6 niveles (2, 2.64, 3.48, 4.59, 6.06, 8) 3 obs de cada una
datos$propkilled <- with(datos, Killed/Number)
summary(datos)

#Vamos a trabajar con datos agrupados que obtenemos a partir de la BD que nos dan
#La manera en la que se va a trabajar con las etiquetas de las variables
#categoricas que propone nuestro modelo y la base de datos que nos dan es:
#Y = El insecto muere o no (1-Yes; 0 No)
#Z = Dosis que recibe el insecto (2.00,2.64,3.48,4.59,6.06,8.00)
#X = Tipo de insecticida que recibe el mosquito 

#Datos agrupados forma 1
n <- c(3, 50-3, 5, 49-5, 19, 47-19, 19, 38-19, 24, 29-24, 35, 50-35,
       2, 50-2, 14, 49-14, 20, 50-20, 27, 50-27, 41, 50-41, 40, 50-40,
       28, 50-28, 37, 50-37, 46, 50-46, 48, 50-48, 48, 50-48, 50, 50-50)

Y <- c("Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No",
       "Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No",
       "Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No")

Z <- c("2.00","2.00", "2.64","2.64", "3.48","3.48","4.59","4.59","6.06","6.06","8.00","8.00",
       "2.00","2.00", "2.64","2.64", "3.48","3.48","4.59","4.59","6.06","6.06","8.00","8.00",
       "2.00","2.00", "2.64","2.64", "3.48","3.48","4.59","4.59","6.06","6.06","8.00","8.00")

X <- c("A","A","A","A","A","A","A","A","A","A","A","A",
       "B","B","B","B","B","B","B","B","B","B","B","B",
       "C","C","C","C","C","C","C","C","C","C","C","C")

datosAg1 <- data.frame(cbind(n,Y,Z,X))
datosAg1[sapply(datosAg1, is.character)] <- lapply(datosAg1[sapply(datosAg1, is.character)], 
                                                   as.factor)
datosAg1$n <- as.numeric(as.character(datosAg1$n))
summary(datosAg1)
head(datosAg1)

#Datos desagrupados (más común en la práctica) 
Datos <- as.data.frame(lapply(datosAg1, rep, datosAg1$n))
Datos$ID <- seq.int(nrow(Datos))
Datos <- Datos[ , c(5,1,2,3,4)]
head(Datos)
summary(Datos)

Datos$lnD <- sapply(as.numeric(as.character(Datos$Z)), log)
power <- function(x){x^2}
Datos$lnD2 <- sapply(as.numeric(as.character(Datos$lnD)), power)
head(Datos)

Datos$lnD <- as.factor(Datos$lnD)
Datos$lnD2 <- as.factor(Datos$lnD2)
summary(Datos)

## Datos agrupados forma 2
nY1 <- c(3,5,19,19,24,35,
         2,14,20,27,41,40,
         28,37,46,48,48,50)
nY0 <- c(50-3,49-5,47-19,38-19,29-24,50-35,
         50-2,49-14,50-20,50-27,50-41,50-40,
         50-28,50-37,50-46,50-48,50-48,50-50)
Z1 <- c("2.00", "2.64", "3.48", "4.59","6.06","8.00",
        "2.00", "2.64", "3.48", "4.59","6.06","8.00",
        "2.00", "2.64", "3.48", "4.59","6.06","8.00")
X1 <- c("A","A","A","A","A","A",
        "B","B","B","B","B","B",
        "C", "C", "C", "C", "C", "C")
datosAg2 <- as.data.frame(cbind(nY1, nY0, Z1,X1 ))
data1 <- matrix(append(nY1, nY0),ncol=2)
head(data1)
head(datosAg2)
#-------------- Busqueda y seleccion del modelo -----------------
#Ajustamos los modelos completos con covariables X=Insecticida y lnD=ln(Deposit)
fitlogit1 <- glm( I(Y == "Yes") ~ X * lnD,
                  family = binomial(link="logit"),
                  data = Datos)
summary(fitlogit1) 

fit1 <- glm(I(Y=="Yes") ~ X + Z,
            family = binomial(link="logit"),
            weights = n,
            data = datosAg1) # AIC: 781.04
summary(fit1) #minuto 31:00, 26 sept

fit2 <- glm(Y ~ X + Z,
            family = binomial(link="logit"),
            data = Datos) # AIC: 781.04
summary(fit2) #minuto 32:30, 26 sept

fit3 <- glm(data1 ~ X1 + Z1,
            family = binomial(link="logit")) # AIC: 88.127
summary(fit3)  #minuto 37:00, 26 sept


DatosObs <- Datos %>% 
  group_by(X,Z) %>%
  summarise(N = n(), Oyes = sum(Y=="Yes"), Ono = sum(Y=="No"))

Datosnew <- as.data.frame(DatosObs[,1:2])

DatosObs$EspYes <- DatosObs$N * predict(fit2, newdata = Datosnew,  type = "response")
DatosObs$EspNo <- DatosObs$N * (1 - predict(fit2, newdata = Datosnew,  type = "response"))

X2 <- sum((DatosObs$EspYes - DatosObs$Oyes)^2 / DatosObs$EspYes) + sum((DatosObs$EspNo-DatosObs$Ono)^2 / DatosObs$EspNo)
G <- -2 * sum(DatosObs$Oyes * log(DatosObs$EspYes / DatosObs$Oyes)) - 2 * sum(DatosObs$Ono * log(DatosObs$EspNo / DatosObs$Ono))

# Ho: Los datos parecen provenir del modelo ajustado 
# vs 
# Ha: los datos no provienen del modelo ajustado
# P-value
pchisq(X2, df = 1, lower.tail = FALSE) # 0.2382319 --- 0.001910288
pchisq(G, df = 1, lower.tail = FALSE) #  0.2395008--- NaN

pchisq(fit3$deviance, df=fit3$df.residual, lower.tail=FALSE) #   --- 0.4466908


library(multcomp)
K <- matrix(c(0,1,0,0,0,0,0,0,
              0,0,1,0,0,0,0,0,
              0,0,0,1,0,0,0,0,
              0,0,0,0,1,0,0,0,
              0,0,0,0,0,1,0,0,
              0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,1),
            ncol = 8,
            nrow = 7,
            byrow = TRUE)
m <- c(0, 0, 0, 0, 0, 0, 0)
summary(glht(fit1, linfct = K, rhs = m),
        test = Chisqtest() ) # Rechazamos p=0.034, significa que 
# al menos uno de los betas es distinto de cero.

# Son equivalentes sin importar qué modelo se usó
summary(glht(fit2, linfct = K, rhs = m), test = Chisqtest() )
summary(glht(fit3, linfct = K, rhs = m), test = Chisqtest() )

coef(fit1)
coef(fit1)
coef(fit3)
vcov(fit1)
vcov(fit2)
vcov(fit3)


#_--------------------------------------
# AQUI COMIENZA PARTE 2 DE GLM BINARIOS

# -------------- minuto 21:20, 27 sept -----------------------------
fitlogit <- glm(Y ~ X * lnD, # modelo completo con todas las interacciones
                family = binomial(link = "logit"),
                data = Datos) # Reg Logistica
summary(fitlogit) # AIC: 791.11

fitprob <- glm(Y ~ X * lnD,
               family = binomial(link = "probit"),
               data = Datos)
summary(fitprob) # betas diferentes al modelo anterior (logit)  # AIC: 791.11

# fitlog <- glm(Y ~ X * Z,
#               family = binomial(link = "log"),
#               data = Datos)
# summary(fitlog) # NO SIRVE

fitcll <- glm(Y ~ X * lnD,
              family = binomial(link = "cloglog"),
              data = Datos)
summary(fitcll) # AIC: 791.11

### REDUCIDO I(Y == "Yes") ~ I(X == "C") * lnD + I(X == "C") + lnD2
fitlogitc <- glm(Y ~ X * lnD + X * lnD2,
                 family = binomial(link = "logit"),
                 data = Datos) # modelo aun mas reducido
summary(fitlogitc) # AIC: 786.27(interacciones) , 781.25 (no interacciones)

#Con este modelo podríamos ya responder las preguntas
#sobre el efecto de la variable X

# -------------- minuto 32:50, 27 sept -----------------------------
#Notar que con la prueba de bondad de ajuste no se rechaza H0
#es decir, el modelo es plausible para los datos
anova(fitlogit, fitlogitc, test = "Chisq") # pvalor = 0.3057


library(statmod)
fitlogitqr <- qresid(fitlogit) #Notar que estos son diferentes a los del
#panel de arriba, incluye cierta aleatorización
#para datos binarios
# -------------- minuto 37:23, 27 sept -----------------------------
#X11()
qqnorm(fitlogitqr, las = 1)
qqline(fitlogitqr) # notamos que si hay normalidad
nortest::lillie.test(fitlogitqr) # No rechaza, es buena noticia
shapiro.test(fitlogitqr) # No rechaza, es buena noticia
library(DHARMa)  #Los residuales simulados también son útiles en este caso
set.seed(123)
fitlogitres <- simulateResiduals(fittedModel = fitlogit)
#X11()
#minuto 38:45
plot(fitlogitres)

summary(fitlogitc)


# Muchas veces se usa el cociente de momios para interpretar
# Para variables binarias se compara contra el nivel de referencia
# Este corresponde al exp(parámetro) #minuto 12:00 - 28 sept

exp(fitlogitc$coeff[2])
exp(confint(fitlogitc))


# Quizás usar intervalos de confianza simultáneos
library(multcomp)
K <- matrix(c(1,0,0,0,0,0,0,0,0, #minuto 20:00 - 28sept
              1,1,0,0,0,0,0,0,0,
              1,0,1,0,0,0,0,0,0,
              1,0,0,1,0,0,0,0,0,
              1,0,0,0,1,0,0,0,0,
              1,0,0,0,0,1,0,0,0,
              1,0,0,0,0,0,1,0,0,
              1,0,0,0,0,0,0,1,0,
              1,0,0,0,0,0,0,0,1),
            ncol = 9,
            nrow = 9,
            byrow = TRUE)
ICeta <- confint(glht(fitlogitc, linfct = K),
                 level = .95)
ICeta

# Aquí es creciente la función liga
Fg_1 <- family(fitlogitc)$linkinv
ICmuLI <- Fg_1(ICeta$confint[1:4, 2]) #min 24:30 28 sept
ICmuLS <- Fg_1(ICeta$confint[1:4, 3])
Estmu <- Fg_1(ICeta$confint[1:4, 1])
# minuto 25:30 28 sept
rbind(ICmuLI, Estmu, ICmuLS)#Col 1 - Insecticida A
#Col 2 - Insecticida C



# Notar que el cociente de momios o odds ratio aquí es fácil de 
# calcular # minuto 29:35 - 28sept
Estmu[1] / (1 - Estmu[1]) #Cociente para InsA
Estmu[2] / (1 - Estmu[2]) #Cociente para InsB 
Estmu[3] / (1 - Estmu[3]) #Cociente para InsC 

(Estmu[2] / (1-Estmu[2])) / (Estmu[1] / (1 - Estmu[1])) #Cociente de momios

# -------------- minuto 27:00 - 28 sept -----------------------------
#Una gráfica resumen
DatosIC <- data.frame(t(rbind(ICmuLI, Estmu, ICmuLS)))
DatosIC$x <- c(0, 1, 0)
DatosIC$X <- c("Insect A", "Insect B", "Insect C")


ggplot(DatosIC, aes(X, Estmu)) +
  geom_point() + 
  geom_errorbar(aes(ymin = ICmuLI, ymax = ICmuLS)) +
  theme_bw()