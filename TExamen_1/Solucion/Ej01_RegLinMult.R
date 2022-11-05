rm(list = ls(all.names = TRUE))
gc()
#--------------------------------------*----------------------------------------
#------------------------------- Ejercicio 1 -----------------------------------
#------------------------ Regresion Lineal Multiple ----------------------------
#--------------------------------------*----------------------------------------
library(dplyr)
library(GGally)
library(multcomp)
library(car)
library(broom)
library(nortest)

#-------------- Cargamos datos y un breve analisis descriptivo -----------------

datos <- read.csv("Preg1B.csv", header = TRUE)
summary(datos) # estadisticas de las variables
str(datos) # Vistazo del dataframe

### Observamos que todas las variable son numericas/continuas.
### Nos gustaria tratar a algunas como variables continuas (bpdiast, bmi, tcresult)
### y tratar a otras como categoricas (age, sex)

#datos$age <- factor(datos$age)
datos$sex <- factor(datos$sex,
                    levels = c(1, 2),
                    labels = c("hombre", "mujer"))
summary(datos)
str(datos)
#GGally::ggpairs(data = datos, title = "Datos pacientes", aes(colour = sex))

#-------------------- Busqueda y seleccion de un modelo ------------------------
# Ajuste del modelo completo sin interacciones
# E(bpdiast; bmi, sex, age) = b0 + b1*bmi + b2*sex + b3*age
fit1 <- lm(bpdiast ~ bmi + sex + age, data = datos)
summary(fit1) 

# Se rechaza H0 en la prueba asociada a la tabla ANOVA (p-value: 7.205e-13)
# Ahora podríamos proceder a reducir un poco más el modelo
# para tratar de facilitar la interpretación

# De la salida de fit1, se puede observar que una opción es considerar b2=0
# observando la prueba t asociada al parametro de la variable dicotomica _sexmujer_,
# es decir:
# H0: b2=0 VS Ha: b2 != 0
# Con una significancia = 0.05, obtenemos que NO se rechaza H0, pues (p-value=0.0836)
# Por lo que podríamos optar por considerar un modelo con b2=0.

# El modelo reducido quedaría como
# E(bpdiast; bmi, sex, age) = b0 + b1*bmi + b2*age

# Esto nos dice que la presion arterial diastolica entre hombres y mujeres es
# en promedio la misma y podriamos modelarla solamente con el indice de masa corporal
# y la edad.

fit2 <- lm(bpdiast ~ bmi + age, data = datos)
summary(fit2)
# Se rechaza H0 en la prueba asociada a la tabla ANOVA (p-value: 7.205e-13)
# Y las pruebas individuales tambien rechazan.

# El modelo quedaria como:
# E(bpdiast; bmi, sex, age) = 51.44593 + 0.85741*bmi + 0.15745*age

# Prueba lineal general para fit2
K <- matrix(c(0,1,0,
              0,0,1),
            ncol = 3,
            nrow = 2,
            byrow = TRUE)
m <- c(0, 0)
summary(multcomp::glht(fit2, linfct = K, rhs = m), test = Ftest())

anova(fit1, fit2) #no rechaza. Es plausible quedar con el reducido

### Pruebas para fit1, solo para ver que utilizar ambos modelos es lo mismo
### y si queremos seguir tomando en cuenta el sexo para la siguientes preguntas
### entonces fit1 sigue siendo util y bueno
# Prueba lineal general
K <- matrix(c(0,1,0,0,
              0,0,1,0,
              0,0,0,1),
            ncol = 4,
            nrow = 3,
            byrow = TRUE)
m <- c(0, 0, 0)
# fit1 tiene sentido
summary(multcomp::glht(fit1, linfct = K, rhs = m), test = Ftest()) 

K <- matrix(c(0,0,1,0),
            ncol = 4,
            nrow = 1,
            byrow = TRUE)
m <- c(0)
# la variable sexo no es significativa
summary(multcomp::glht(fit1, linfct = K, rhs = m), test = Ftest())

# Comparamos AIC
c(AIC(fit1), AIC(fit2)) # (2732.973 , 2734.006)
  # Recomiendacion de modelo segun este metodo:
  library(MASS)
  select <- MASS::stepAIC(fit1, direction = c("both"))
  summary(select) # nos arroja el fit1 como recomendacion

  anova <- aov(fit1) # para ver variables significativas
  summary(anova) # fit1: sex esta al borde de ser significativo, pero no alcanza
                 # fit2: todas son significativas(bmi, age)
  
  #library(trafo)
  #trafo::assumptions(fit1)
#library(webshot) 
#library(sjPlot)
#sjPlot::tab_model(fit1, fit2, file = "plot.html")
#webshot("plot.html", "plot.png")
#-------------------- Verificacion de supuestos  -------------------------------
# Utilizando fit2
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(fit2, 1) # linealidad
plot(fit2, 3) # homocedasticidad
plot(fit2, 2) # normalidad
plot(fit2, 5, cook.levels = c(4/(dim(datos)[1]-2), 0.5, 1.0)) # Outliers: 145 pero no tan grave


# Linealidad
# H0: residuales no dependen de la variable
car::residualPlots(fit2) 
# Tukey(global) no rechaza, bmi tampoco, pero age si rechaza
# Con base en la grafica y en que globalmente y bmi no rechazan entonces podemos
# decir que se cumple la linealidad

# Homocedasticidad
# H0: la varianza es constante 
car::ncvTest(fit2) # fit1(p = 0.04456)***OJO  fit2(p = 0.072693)
# No se rechaza H0, no hay evidencia en contra de la homocedasticidad
# Se cumple la homocedasticidad

# Normalidad 
# Pruebas para normalidad errores estandarizados
# Se basa en los residuales estandarizados o estudentizados
# H0: los datos provienen de una distribución normal
datosfit2 <- broom::augment(fit2) 
#View(datosfit1)
shapiro.test(datosfit2$.std.resid) 
# Se rechaza H0 fit1(p-value = 0.0221)    fit2(p.value = 0.01843)
nortest::lillie.test(datosfit2$.std.resid) #fit1(p-value = 0.4059)  fit2(p-value = 0.5951)
# No se rechaza H0, no hay evidencia en contra de la normalidad 
# Le damos peso a Lilliefors (Kolmogorov-Smirnov) normality test y a la grafica
# ya que la distribucion de los datos no se ve tan mal.
# Por lo tanto se cumple normalidad

# Aleatoriedad 
library(randtests)
randtests::runs.test(datosfit2$.std.resid)#fit2(p-value = 0.3917)
# H0: hay aleatoriedad vs Ha: NO hay aleatoriedad
## No encontramos fuerte evidencia en contra del supuesto de Aleatoriedad,
## entonces podemos considerar plausible que se cumple con este supuesto.


#------------------ Interpretacion y Pruebas de Hipotesis  ---------------------
# ¿Se puede indicar que para una persona de cierta edad y sexo, 
# tener un índice de masa corporal alto se asocia con una alta presión arterial diastólica?

# Esta pregunta se puede expresar en terminos de la esperanza como:
# E(bpdiast; bmi, sex, age) > E(bpdiast; bmi=0, sex, age)
# b0 + b1*bmi + b2*age > b0 + b2*age
# b1*bmi > 0
# b1 > 0
# es decir, cuando el bmi se considera entonces la esperanza de la presion aumenta.
# Recordando que el bmi es positivo siempre.

# Recordando el modelo fit2 tenemos que 
# E(bpdiast; bmi, sex, age) = 51.44593 + 0.85741*bmi + 0.15745*age
summary(fit2)

# La prueba de hipótesis queda como:
# H0: b1 <= 0 
# vs
# Ha: b1 > 0

K <- matrix(c(0, 1, 0),
            ncol = 3,
            nrow = 1,
            byrow = TRUE)
m <- c(0)
summary(glht(fit2, linfct = K, rhs = m, alternative = "greater")) # pvalue= 1.51e-09

# Se rechaza H0, es decir,
# hay evidencia para concluir que la inclusion y el aumento en el indice de masa corporal
# esta asociado al aumento de la presion arterial diastolica.


#------------------------------ Graficas  -------------------------------------

with(datos, 
     plot(bmi, bpdiast, 
          main = "Relación entre Presión Arterial Diastólica e Índice de Masa Corporal\n RLM",
          col = c("#0000FF", "#FF1734"), 
          pch = c(20,20)))
legend("topleft", 
       levels(datos[,6]), 
       col = c("#0000FF", "#FF1734"), 
       pch = c(20,20), 
       pt.cex = 1.5,
       cex = .9,
       y.intersp = 1.4 ,
       bty = "n" )

#Creamos una malla de valores asociados al bmi
bmi <- seq(from = 12, to = 47, by = .5)
length(bmi)

#E(bpdiast;bmi, age)= 52.49173 + 0.86536*bmi + 0.15490*age
K_30 <- cbind(1, bmi, 30)
K_50 <- cbind(1, bmi, 50)
K_64 <- cbind(1, bmi, 64)
K <- rbind(K_30, K_50, K_64)
(K)
fitE <- glht(fit2, linfct = K) # fit estimaciones 
fitci <- confint(fitE, level = 0.90) # fit intervalos de confianza (simultaneos)

lines(bmi, coef(fitE)[1:71], lwd=2, col = "#00DDFF") # recta para 30 anios (azul claro)
lines(bmi, coef(fitE)[72:142], lwd=2, col = "#FFD100") # recta para 50 anios (amarillo)
lines(bmi, coef(fitE)[143:213], lwd=2, col = "#00FF36") # recta para 64 anios (verde)

legend(x = "topright",         # Position
       legend = c("64 años de edad", "50 años de edad", "30 años de edad"), # Legend texts
       lty = c(1, 1, 1),          # Line types
       col = c("#00FF36", "#FFD100", "#00DDFF"),          # Line colors
       lwd = 2)                # Line widths

