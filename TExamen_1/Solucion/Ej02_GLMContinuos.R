rm(list = ls(all.names = TRUE))
gc()
#--------------------------------------*----------------------------------------
#------------------------------- Ejercicio 2 -----------------------------------
#------------------------ Modelos Lineales Generalizados -----------------------
#------------------------------ datos continuos --------------------------------
#--------------------------------------*----------------------------------------
#-------------- Cargamos datos y un breve analisis descriptivo -----------------

datos <- read.csv("Preg1B.csv", header = TRUE)
summary(datos) # estadisticas de las variables
str(datos) # Vistazo del dataframe

### Observamos que todas las variable son numericas/continuas.
### Nos gustaria tratar a algunas como variables continuas (bpdiast, bmi, tcresult, age)
### y tratar a otras como categoricas (sex)

datos$sex <- factor(datos$sex,
                    levels = c(1, 2),
                    labels = c("hombre", "mujer"))
summary(datos)
str(datos)

#-------------------- Busqueda y seleccion de un modelo ------------------------
fit2 <- lm(bpdiast ~ bmi + age, data = datos)

#Ahora buscaremos entre un conjunto de posibles glm
#varios aspectos que podríamos considerar

#Componente lineal: 
# i) Transformaciones Box Tidwell (potencias) a x
# ii) Polinomio sobre x 

malla1 <- rep(c(1,2,3,4,5), each=5, len=25) # sucesion del 1 al 5 (para usarlos como grados para bmi)
malla2 <- rep(c(1,2,3,4,5), each=1, len=25) # sucesion del 1 al 5 (para usarlos como grados para age)
Poli <- cbind("poly", malla1, malla2)

malla1 <- rep(c(-2,-1.5, -1, -.5, 0, .5, 1, 1.5, 2), each=9, len=81) # -2 a 2 con pasos de .5 (para probar potencias para bmi)
malla2 <- rep(c(-2,-1.5, -1, -.5, 0, .5, 1, 1.5, 2), each=1, len=81) # -2 a 2 con pasos de .5 (para probar potencias para age)
Pot <- cbind("pot", malla1, malla2)

CompLin <- rbind(Poli, Pot) # juntamos lo anterior

#Componente aleatorio:
# i) Distribución Normal
# ii) Distribución Gamma
# iii) Distribución Inversa Gaussiana
#help("family")
Distribuciones <- c("gaussian", "Gamma", "inverse.gaussian")

#Función liga
# i) inverse
# ii) identity
# iii) log
# iv) 1/mu^2 (sólo IG)

FunLigas <- c("identity", "log", "inverse", "1/mu^2")

nFunLigas <- length(FunLigas) # 4
nDist <- length(Distribuciones)# 3
nCompLin <- dim(CompLin)[1]# 106


ModelList <- list(NA) # para guardar los ajustes de los diferentes modelos que ajustaremos
AICList <- list(NA) # para guardar los AIC de cada modelo
BICList <- list(NA) # para guardar los BIC  de cada modelo
FormList <- list(NA) # para guardar las formulas (comandos que se ejecutaron)
#Total modelos 106*2*3+106*4 = 1060 
index <- 0
for(k in 1:nCompLin){
  if(CompLin[k,1] == "poly"){
    formstring <- paste0("bpdiast ~ poly(bmi,", CompLin[k, 2], ", raw = TRUE) + poly(age, ", CompLin[k, 3], ", raw = TRUE)")
  }else{
    if(CompLin[k,2] == 0 || CompLin[k,3] == 0 ){
      if(CompLin[k,2] == 0 & CompLin[k,3] == 0){
        formstring <- paste0("bpdiast ~ I(log(bmi)) + I(log(age))")
      } else if (CompLin[k,2] == 0 & CompLin[k,3] != 0){
        formstring <- paste0("bpdiast ~ I(log(bmi)) + I(age^(", CompLin[k,3], "))")
      } else {
        formstring <- paste0("bpdiast ~ I(bmi^(", CompLin[k,2], ")) + I(log(age))")
      }
      
    }else{
      formstring <- paste0("bpdiast ~ I(bmi^(", CompLin[k,2], ")) + I(age^(", CompLin[k,2], "))")
    }
  }
  form <- as.formula(formstring) # para que tome el string como una formula
  for(j in 1:nDist){
    for(l in 1:nFunLigas){
      if(FunLigas[l] == "1/mu^2"){
        if(Distribuciones[j] == "inverse.gaussian"){
          index <- index + 1
          Dist <- get(Distribuciones[j]) # tomar la cadena como funcion
          Mod.A.Prueba <- glm(form, data = datos, family = Dist(link = FunLigas[l]))
          ModelList[[index]] <- Mod.A.Prueba
          AICList[[index]] <- AIC(Mod.A.Prueba)
          BICList[[index]] <- BIC(Mod.A.Prueba)
          FormList[[index]] <- formstring
        }
      }else{
        index <- index + 1
        Dist <- get(Distribuciones[j]) # tomar la cadena como funcion
        Mod.A.Prueba <- glm(form, data = datos, family = Dist(link = FunLigas[l]))
        ModelList[[index]] <- Mod.A.Prueba
        AICList[[index]] <- AIC(Mod.A.Prueba)
        BICList[[index]] <- BIC(Mod.A.Prueba)
        FormList[[index]] <- formstring
      }
    }
  }
}

#Índice del modelo con menor AIC

MinAIC <- which.min(unlist(AICList)) # indice 20
ModMinAIC <- ModelList[[MinAIC]]
summary(ModMinAIC)
ModMinAIC$family # Family: inverse.gaussian 
                 # Link function: 1/mu^2 

AICList[[MinAIC]] # 2718.066
BICList[[MinAIC]] # 2737.355
FormList[[MinAIC]] # "bpdiast ~ poly(bmi,1, raw = TRUE) + poly(age, 2, raw = TRUE)"

fitAIC <- glm(bpdiast ~ bmi + poly(age, 2, raw = TRUE),
              data = datos,
              family = inverse.gaussian(link = "1/mu^2"))
summary(fitAIC)

#Índice del modelo con menor BIC

MinBIC <- which.min(unlist(BICList)) # indice 927
ModMinBIC <- ModelList[[MinBIC]]
summary(ModMinBIC)
ModMinBIC$family # Family: inverse.gaussian 
                 # Link function: identity 

AICList[[MinBIC]] # 2719.157
BICList[[MinBIC]] # 2734.588
FormList[[MinBIC]] # "bpdiast ~ I(bmi^(1.5)) + I(log(age))"

fitBIC <- glm(bpdiast ~ I(bmi^(1.5)) + I(log(age)),
                 data = datos,
                 family = inverse.gaussian(link = "identity"))

summary(fitBIC)

#-------------------- Verificacion de supuestos  -------------------------------
library(ggplot2)
library(ggResidpanel)
library(statmod)
library(DHARMa)
library(SuppDists)
library(performance)

# Graficas de supuestos para fit2
ggResidpanel::resid_panel(fit2, plots = c("all"), smoother = TRUE)
fit2res <- DHARMa::simulateResiduals(fittedModel = fit2)
plot(fit2res)


# Graficas de supuestos para fitAIC
ggResidpanel::resid_panel(fitAIC, plots = c("all"), smoother = TRUE)
plot(fitAIC, 1) # linealidad
plot(fitAIC, 3) # homocedasticidad
plot(fitAIC, 5, cook.levels = c(4/(dim(datos)[1]-2), 0.5, 1.0)) # Outliers: 145 pero no tan grave
# Normalidad
fitAICqr <- statmod::qresid(fitAIC)
qqnorm(fitAICqr, las=1)
qqline(fitAICqr)
nortest::lillie.test(fitAICqr) # p-value = 0.6242
shapiro.test(fitAICqr) # p-value = 0.5928
# NO se rechaza, por lo que no hay evidencia en contra de la normalidad
fitAICres <- DHARMa::simulateResiduals(fittedModel = fitAIC)
plot(fitAICres)


# Graficas de supuestos para fitBIC
ggResidpanel::resid_panel(fitBIC, plots = c("all"), smoother = TRUE)
plot(fitBIC, 1) # linealidad
plot(fitBIC, 3) # homocedasticidad
plot(fitBIC, 5, cook.levels = c(4/(dim(datos)[1]-2), 0.5, 1.0)) # Outliers: 145 pero no tan grave
# Normalidad
fitBICqr <- statmod::qresid(fitBIC)
qqnorm(fitBICqr, las=1)
qqline(fitBICqr)
nortest::lillie.test(fitBICqr) # p-value = 0.7187
shapiro.test(fitBICqr) # p-value = 0.843
# NO se rechaza, por lo que no hay evidencia en contra de la normalidad
fitBICres <- DHARMa::simulateResiduals(fittedModel = fitBIC)
plot(fitBICres)

performance::compare_performance(fitAIC, fitBIC)

#------------------ Interpretacion y Pruebas de Hipotesis  ---------------------
# ¿Se puede indicar que para una persona de cierta edad y sexo, 
# tener un índice de masa corporal alto se asocia con una alta presión arterial diastólica?

# Esta pregunta se puede expresar en terminos de la esperanza como:
# E(bpdiast; bmi, sex, age) > E(bpdiast; bmi=0, sex, age)
# b0 + b1*bmi + b2*age > b0 + b2*age
# b1*bmi > 0
# b1 > 0
library(multcomp)
summary(fitAIC)

K <- matrix(c(0, 1, 0, 0),
            ncol = 4,
            nrow = 1,
            byrow = TRUE)
m <- c(0)
summary(glht(fitAIC, linfct = K, rhs = m, alternative = "greater"))

# NO rechaza H0, ESTE MODELO DEL AIC esta raro, Nos quedaremos con el fitBIC



summary(fitBIC)
K <- matrix(c(0, 1, 0),
            ncol = 3,
            nrow = 1,
            byrow = TRUE)
m <- c(0)
summary(glht(fitBIC, linfct = K, rhs = m, alternative = "greater")) # p-value = 1.64e-08
# Se rechaza H0, es decir,
# hay evidencia para concluir que la inclusion y el aumento en el indice de masa corporal
# esta asociado al aumento de la presion arterial diastolica.

#------------------------------ Graficas  -------------------------------------
# utilizando fitBIC
with(datos, 
     plot(bmi, bpdiast, 
          main = "Relación entre Presión Arterial Diastólica e Índice de Masa Corporal\n GLM",
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

#E(bpdiast;bmi, age)= 39.0625 + 0.1067*(bmi)^(1.5) + 7.3473*ln(age)
K_30 <- cbind(1, (bmi)^(1.5), log(30))
K_50 <- cbind(1, (bmi)^(1.5), log(50))
K_64 <- cbind(1, (bmi)^(1.5), log(64))
K <- rbind(K_30, K_50, K_64)
(K)
fitE <- glht(fitBIC, linfct = K) # fit estimaciones 
fitci <- confint(fitE, level = 0.90) # fit intervalos de confianza (simultaneos)

lines(bmi, coef(fitE)[1:71], lwd=2, col = "#00DDFF") # recta para 30 anios (azul claro)
lines(bmi, coef(fitE)[72:142], lwd=2, col = "#FFD100") # recta para 50 anios (amarillo)
lines(bmi, coef(fitE)[143:213], lwd=2, col = "#00FF36") # recta para 64 anios (verde)

legend(x = "topright",         # Position
       legend = c("64 años", "50 años", "30 años"), # Legend texts
       lty = c(1, 1, 1),          # Line types
       col = c("#00FF36", "#FFD100", "#00DDFF"),          # Line colors
       lwd = 2)                # Line widths


#------------------------------ conclusiones  -------------------------------------
# RLM vs GLM

performance::compare_performance(fit2, fitBIC)

library(jtools) # para comparar varios modelos
export_summs(fit2, fitBIC)#, scale = TRUE)
#library(sjPlot)
#sjPlot::tab_model(fit2, fitBIC)#, file = "plot.html")
#library(webshot)
#webshot("plot.html", "plot.png")
