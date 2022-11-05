rm(list = ls(all.names = TRUE))
gc()
#--------------------------------------*----------------------------------------
#------------------------------- Ejercicio 3 -----------------------------------
#------------------------ Modelos Lineales Generalizados -----------------------
#------------------------------ datos binarios  --------------------------------
#--------------------------------------*----------------------------------------
library(tidyverse)
library(multcomp)

#-------------- Cargamos datos y un breve analisis descriptivo -----------------

datos <- read.csv("Preg3B.csv", header = TRUE)
summary(datos) # estadisticas de las variables
str(datos) # Vistazo del dataframe

#Convertimos a tipo factor la variable Insecticide
datos$Insecticide <- factor(datos$Insecticide) # 3 niveles (A,B,C) 6 obs de cada una
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
#-------------- Grafica descriptivo -----------------

#Graficamos
library(lessR)
Datos$ZX <- factor(paste(Datos$Z,Datos$X, sep="."))
summary(Datos)
varx <- Datos$ZX
varby <- Datos$Y
b <- BarChart(x = varx, by = varby, stack100 = TRUE) # la de las notas pagina 30
#b$freq

# OBSERVACIONES IMPORTANTES:
# 1.
# Vemos que para la dosis mas pequeña (2.00mg) el insecticida C es mucho más letal,
# mientras que los insecticidas A y B son muy parecidos y no son muy letales.
# 2.
# Tambien vemos que para dosis mayores o iguales a 3.48mg todos los insecticidas 
# matan a la mitad o a más de la mitad de los mosquitos en su grupo de estudio.

ggplot(data = datos, mapping = aes(x = Deposit, y = propkilled, group=Insecticide)) +
  geom_point(size=5,alpha = 1, aes(color = Insecticide)) +
  geom_line(size=3, alpha = 1, aes(color = Insecticide)) +
  ggtitle("Letalidad de insecticidas en diferentes dosis") + 
  ylab("Proporción de insectos muertos") +
  xlab("Dosis (mg)")
#-------------- Busqueda y seleccion del modelo -----------------
Datos$lnD <- sapply(as.numeric(as.character(Datos$Z)), log)
power <- function(x){x^2}
Datos$lnD2 <- sapply(as.numeric(as.character(Datos$lnD)), power)
head(Datos)
summary(Datos)

#Ajustamos los modelos completos con covariables X=Insecticida y lnD=ln(Deposit)
fitlogit1 <- glm( I(Y == "Yes") ~ X * lnD,
                  family = binomial(link="logit"),
                  data = Datos)
summary(fitlogit1) # AIC: 789.44
#             Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)  -4.2306     0.5239  -8.075 0.000000000000000676 ***
# XB            0.1878     0.7223   0.260              0.79492    
# XC            2.1099     0.7896   2.672              0.00754 ** 
# lnD           2.7272     0.3491   7.813 0.000000000000005587 ***
# XB:lnD        0.1109     0.4867   0.228              0.81980    
# XC:lnD        0.6612     0.6712   0.985              0.32462 


fitprobit1 <- glm( I(Y == "Yes") ~ X * lnD,
                 family = binomial(link="probit"),
                 data = Datos)
summary(fitprobit1) # AIC: 789.28
#             Estimate Std. Error z value             Pr(>|z|)    
# (Intercept) -2.54267    0.28870  -8.807 < 0.0000000000000002 ***
# XB           0.10533    0.39972   0.264             0.792161    
# XC           1.50479    0.43289   3.476             0.000509 ***
# lnD          1.63390    0.19411   8.417 < 0.0000000000000002 ***
# XB:lnD       0.07229    0.27032   0.267             0.789134    
# XC:lnD       0.13739    0.34712   0.396             0.692257

fitcll1 <- glm( I(Y == "Yes") ~ X * lnD,
                 family = binomial(link="cloglog"),
                 data = Datos)
summary(fitcll1) # AIC: 800.46
#              Estimate Std. Error z value             Pr(>|z|)    
# (Intercept) -3.377033   0.392200  -8.610 < 0.0000000000000002 ***
# XB           0.260054   0.529806   0.491                0.624    
# XC           2.349663   0.485315   4.842   0.0000012884593503 ***
# lnD          1.860982   0.233745   7.962   0.0000000000000017 ***
# XB:lnD      -0.004014   0.318653  -0.013                0.990    
# XC:lnD      -0.485682   0.326891  -1.486                0.137  

#----------Ajustamos los modelos completos como antes agregando lnD^2
fitlogit2 <- glm( I(Y == "Yes") ~ X * lnD + X * lnD2,
                  family = binomial(link="logit"),
                  data = Datos)
summary(fitlogit2) # AIC: 786.61
#             Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  -8.5115     1.9931  -4.270 0.0000195 ***
# XB            2.0130     2.5885   0.778   0.43676    
# XC            6.1502     2.6844   2.291   0.02196 *  
# lnD           9.0848     2.7783   3.270   0.00108 ** 
# lnD2         -2.1670     0.9184  -2.360   0.01830 *  
# XB:lnD       -2.4788     3.6629  -0.677   0.49857    
# XC:lnD       -5.2378     4.3004  -1.218   0.22323    
# XB:lnD2       0.8392     1.2312   0.682   0.49548    
# XC:lnD2       1.9708     1.6558   1.190   0.23393 


fitprobit2 <- glm( I(Y == "Yes") ~ X * lnD + X * lnD2,
                   family = binomial(link="probit"),
                   data = Datos)
summary(fitprobit2) # AIC: 786.92
#             Estimate Std. Error z value   Pr(>|z|)    
# (Intercept)  -4.5596     1.0218  -4.462 0.00000811 ***
# XB            0.6786     1.3606   0.499    0.61797    
# XC            2.9345     1.3881   2.114    0.03451 *  
# lnD           4.7174     1.4741   3.200    0.00137 ** 
# lnD2         -1.0662     0.4987  -2.138    0.03250 *  
# XB:lnD       -0.7729     1.9818  -0.390    0.69655    
# XC:lnD       -1.8719     2.1979  -0.852    0.39440    
# XB:lnD2       0.2772     0.6782   0.409    0.68273    
# XC:lnD2       0.6276     0.8172   0.768    0.44251


fitcll2 <- glm( I(Y == "Yes") ~ X * lnD + X * lnD2,
                family = binomial(link="cloglog"),
                data = Datos)
summary(fitcll2) # AIC: 786.06
#             Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  -8.1232     1.6373  -4.961 0.0000007 ***
# XB            1.9730     2.0854   0.946  0.344091    
# XC            6.1394     1.8251   3.364  0.000769 ***
# lnD           8.5986     2.1733   3.956 0.0000761 ***
# lnD2         -2.1979     0.6910  -3.181  0.001470 ** 
# XB:lnD       -2.3761     2.7896  -0.852  0.394343    
# XC:lnD       -5.5724     2.5302  -2.202  0.027638 *  
# XB:lnD2       0.7482     0.8952   0.836  0.403244    
# XC:lnD2       1.5583     0.8425   1.850  0.064375 .

################ OTRO MODELO QUITANDO INSECTICIDA B y la interaccion con lnD^2
# fitcll <- glm( I(Y == "Yes") ~ I(X == "C") * lnD + I(X == "C") + lnD2,
#                family = binomial(link="cloglog"),
#                data = Datos)
# summary(fitcll) # AIC: 784.25
#                     Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)          -5.9712     0.7244  -8.243 < 0.0000000000000002 ***
# I(X == "C")TRUE       2.9140     0.4124   7.066     0.00000000000159 ***
# lnD                   5.8145     0.9557   6.084     0.00000000117288 ***
# lnD2                 -1.3105     0.3082  -4.252     0.00002121240543 ***
# I(X == "C")TRUE:lnD  -0.9939     0.2705  -3.674             0.000239 ***

# ----------- VERIFICACION DE SUPUESTOS ----------------------------------------
library(statmod)
fitcll2qr <- qresid(fitcll2) #Notar que estos son diferentes a los del
#panel de arriba, incluye cierta aleatorización
#para datos binarios
qqnorm(fitcll2qr, las = 1)
qqline(fitcll2qr, col="red") # notamos que si hay normalidad
nortest::lillie.test(fitcll2qr) # No rechaza, es buena noticia
shapiro.test(fitcll2qr) # No rechaza, es buena noticia
library(DHARMa)  #Los residuales simulados también son útiles en este caso
set.seed(123)
fitlogitres <- simulateResiduals(fittedModel = fitcll2)
plot(fitlogitres)  #Aquí no tenemos variables continuas,
#sería más informativa la gráfica
#El principal problema podría ser sobre
#el parámetro de dispersión, en ese caso
#por lo general se rechaza la prueba
#correspondiente al test de dispersión
#la alternativa es considerar el 
#modelo quasibinomial
# No se observan problemas con los supuestos del modelo


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--------- Estimaciones y Pruebas de Hipotesis ---------------------------------
#-------------------------------------------------------------------------------
# Con fines de continuar el ejercicio suponemos que 
# fitcll2 es el mejor modelo por tener menor AIC
fit <- fitcll2
summary(Datos)
summary(fit)
b1 <- fit$coefficients[1]
b2 <- fit$coefficients[2]
b3 <- fit$coefficients[3]
b4 <- fit$coefficients[4]
b5 <- fit$coefficients[5]
b6 <- fit$coefficients[6]
b7 <- fit$coefficients[7]
b8 <- fit$coefficients[8]
b9 <- fit$coefficients[9]

################ Inciso iv)-a)
propA <- function(x) 1 - exp(x^b4 * (-exp(b1 + b5*log(x)^2 )))
propB <- function(x) 1 - exp(x^(b4+b6) * (-exp(b1 + b2 + (b5+b8)*log(x)^2 )))
propC <- function(x) 1 - exp(x^(b4+b7) * (-exp(b1 + b3 + (b5+b9)*log(x)^2 )))
# Grafica de proporcion de insectos muertos
ggplot(data = datos, mapping = aes(x = Deposit, y = propkilled, group=Insecticide)) +
  geom_point(size=5,alpha = 1, aes(color = Insecticide)) +
  geom_line(size=1, alpha = 1, aes(color = Insecticide)) +
  ggtitle("Letalidad de insecticidas en diferentes dosis") + 
  ylab("Proporción de insectos muertos") +
  xlab("Dosis (mg)") +
  geom_function(fun = propA, colour = "red", size=2) + 
  geom_function(fun = propB, colour = "green", size=2) + 
  geom_function(fun = propC, colour = "blue", size=2)

###################### Inciso iv)-b)
# Ahora calculamos para el 75%
# x es el porcentaje/probabilidad de insecto muerto
dosisC <- function(x) exp(( sqrt((b4+b7)^2-4*(b5+b9)*(b1+b3-log(-log(1-x)))) - b4 - b7) / (2*(b5+b9)))
dosisB <- function(x) exp(( sqrt((b4+b6)^2-4*(b5+b8)*(b1+b2-log(-log(1-x)))) - b4 - b6) / (2*(b5+b8)))
#dosisA <- function(x) exp(( -sqrt((b4)^2-4*(b5)*(b1-log(-log(1-x)))) - b4) / (2*(b5)))
# Para dosis A lo hacemos en wolfram
p <- .75
(y <- log(-log(1-p)))
cat(y, b1, b4, b5) # valores para wolfram

A75 <- 7.00801
B75 <- dosisB(.75) # 5.8953
C75 <- dosisC(.75) # 2.60383

# Grafica de proporcion de insectos muertos
ggplot(data = datos, mapping = aes(x = Deposit, y = propkilled, group=Insecticide)) +
  geom_point(size=5,alpha = 1, aes(color = Insecticide)) +
  geom_line(size=1, alpha = 1, aes(color = Insecticide)) +
  ggtitle("Letalidad de insecticidas en diferentes dosis") + 
  ylab("Proporción de insectos muertos") +
  xlab("Dosis (mg)") +
  geom_function(fun = propA, colour = "red", size=2) + 
  geom_function(fun = propB, colour = "green", size=2) + 
  geom_function(fun = propC, colour = "blue", size=2) +
  geom_vline(xintercept = A75, colour = "red") +
  geom_vline(xintercept = B75, colour = "green") +
  geom_vline(xintercept = C75, colour = "blue") +
  scale_y_continuous(
    name = "Proporcion de insectos muertos",
    sec.axis = sec_axis(~., name="Probabilidad de que muera un insecto")) 

###################### Inciso iv)-c)
# En teoria el insecticida C necesita una dosis de 2.6mg para matar al 75% de insectos.
# Mientras los insecticidas A y B necesitan 5.9mg y 7.1mg respectivamente.
# Esto quiere decir que el insecticida C parece ser mejor por necesita menos dosis
# Lo cual implica que rinde más y se necesitan menos recurso para generar una sola dosis.
# En pocas palabras:
# Un insecticida es mejor si para una dosis dada, la probabilidad de que el insecto muera es mayor
# Respondemos a la pregunta 
# ¿El insecticida C es el mejor?
# Esto quiere decir que queremos probar si
# E(Y;X=C)>E(Y;X=A) <-> b3 + b7 lnD + b9 lnD2 > 0
# E(Y;X=C)>E(Y;X=B) <-> - b2 + b3 - b6 lnD + b7 lnD - b8 lnD2 + b9 lnD2 > 0

#Por lo que nuestra prueba quedaría como:
# H0: b3 + b7 lnD + b9 lnD2 <= 0   y   - b2 + b3 - b6 lnD + b7 lnD - b8 lnD2 + b9 lnD2 <= 0 
# vs
# Ha: b3 + b7 lnD + b9 lnD2 > 0   y   - b2 + b3 - b6 lnD + b7 lnD - b8 lnD2 + b9 lnD2 > 0 
dosisMin <- 2.6
lnD <- log(dosisMin)
lnD2 <- lnD^2
#Realizando la prueba:
K <- matrix(c(0, 0,1,0,0,   0,lnD,    0,lnD2,
              0,-1,1,0,0,-lnD,lnD,-lnD2,lnD2), ncol=9, nrow=2, byrow=TRUE)
m <- c(0,0)
summary(multcomp::glht(fitcll2, linfct=K, rhs=m, alternative="greater"))
#Obtenemos que ambos p-value son menores a 0.05 y por tanto podemos rechazar H0, es decir,
# hay evidencia para decir que el insecticida C es el mejor.


###################### Inciso iv)-d)
# ¿Se puede indicar que los insecticidas A y B tienen un desempeño similar?
# Esto quiere decir que queremos probar si
# E(Y; X = A, lnD, lnD2) = E(Y; X = B, lnD, lnD2) <-> b2 + b6 lnD + b8 lnD2 = 0
# La prueba de hipótesis sería:
# H0: b2 + b6 lnD + b8 lnD2 = 0
# vs
# Ha: b2 + b6 lnD + b8 lnD2 != 0
#Realizando la prueba:
K <- matrix(c(0,1,0,0,0,lnD,0,lnD2,0), ncol=9, nrow=1, byrow=TRUE)
m <- c(0)
summary(multcomp::glht(fitcll2, linfct=K, rhs=m, alternative="two.sided"))
# Obtenemos que el p-value es mayor a 0.05 y por tanto no rechazamos H0, es decir,
# hay evidencia para decir que los insecticidas A y B tienen un desempeño similar.

