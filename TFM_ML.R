
# PREDICCIÓN VARIABLE BINARIA (ACEPTACIÓN O RECHAZO DE UNA PROMOCIÓN DE DESCUENTO)

# *****************************************************************************
# *****************      LIBRERÍAS NECESARIAS      ****************************
# *****************************************************************************

library(readxl)
library(tidyverse)
library(dplyr)
library(nnet)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(pROC)
library(plyr)
library(parallel)
library(doParallel)

# Iniciar paralelización (si fuese necesario)

clusters <- detectCores() - 1
make_cluster <- makeCluster(clusters)
registerDoParallel(make_cluster)
showConnections()

# stopCluster(make_cluster)
# registerDoSEQ()

# *****************************************************************************
# *******************      FICHERO DE DATOS      ******************************
# *****************************************************************************

# Importación del fichero
ivcr_dataset <- read_excel("./Dataset_Dummies_Final.xlsx")

# Cambiamos la variable objetivo ("Y") a Yes (acepta), N (rechaza)
ivcr_dataset$Y<-ifelse(ivcr_dataset$Y==1,"Yes","No")

# Para evitar posibles problemas, renombramos "Y" como "promotion"
colnames(ivcr_dataset)[colnames(ivcr_dataset) == "Y"] <- "promotion"

# Archivo de datos como DataFrame
ivcr_dataset <- as.data.frame(ivcr_dataset)


# *****************************************************************************
# ***************      COMPROBACIONES INICIALES      **************************
# *****************************************************************************

# Comprobamos de nuevo que no hay valores ausentes
colSums(is.na(ivcr_dataset))

# Frecuencias de las variables categóricas
frecu<-ldply(ivcr_dataset,function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)
frecu

# No hay problemas con el número de observaciones por categoría.


# *****************************************************************************
# ***************      CHEQUEO INTUITIVO INICIAL      *************************
# *****************************************************************************

# Se van a comparar un modelo logístico con uno de RandomForest, para hacerse una
# idea sobre qué modelos podrían funcionar mejor para nuestro conjunto de datos.
# A priori, teniendo en cuenta que todas las variables son categóricas, funcionarán
# mejor los métodos basados en árboles, aunque realizamos los siguientes pasos para
# comprobarlo.

# Modelo logístico con variables obtenidas con stepwise básico. Se realiza con las
# variables más importantes según el criterio stepAIC.

# Modelo logístico -------------------------------------------------

# Variables seleccionadas por el stepAIC.
c("(Intercept)", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
  "expiration", "destination.UrgentPlace", "CoffeeHouse", "weather.AdverseClimatology", 
  "passanger.Kids", "education", "direction_same", "coupon.Bar", 
  "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
  "passanger.Alone", "coupon.CoffeeHouse", "age", "occupation.Employee", 
  "maritalStatus.Divorced", "income")

control_pr<-trainControl(method = "LGOCV",p=0.8,number=1,
                      classProbs=TRUE,savePredictions = "all") 

logi_pr <- train(promotion~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+expiration
+destination.UrgentPlace+CoffeeHouse+weather.AdverseClimatology+passanger.Kids+
education+direction_same+coupon.Bar+Restaurant20To50+gender+occupation.Retired+
maritalStatus.Single+passanger.Alone+coupon.CoffeeHouse+age+occupation.Employee+
maritalStatus.Divorced+income,
             data=ivcr_dataset,
             method="glm",trControl=control_pr)

# Evaluación rápida del modelo logístico
summary(logi_pr)
logi_pr
sal<-logi_pr$pred

salconfu<-confusionMatrix(sal$pred,sal$obs,positive="Yes")
salconfu

# Matriz de confusión

                #Reference
     #Prediction   No  Yes
            #No   621  341
            #Yes  469 1090

# Accuracy: 0.6787 (obtenida de la evaluación rápida)
# Sensitivity: 0.7617 (obtenida de la evaluación rápida)
# Specificity: 0.5697 (obtenida de la evaluación rápida)

# Calculamos AUC y curva ROC de este modelo
curvaroc <- roc(response = sal$obs, predictor = sal$Yes)
auc <- curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))


# Lo aplicamos ahora con Validación Cruzada:

set.seed(12345)
control_pr<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE) 

logi_pr<- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+expiration
             +destination.UrgentPlace+CoffeeHouse+weather.AdverseClimatology+passanger.Kids+
               education+direction_same+coupon.Bar+Restaurant20To50+gender+occupation.Retired+
               maritalStatus.Single+passanger.Alone+coupon.CoffeeHouse+age+occupation.Employee+
               maritalStatus.Divorced+income,
                  data=ivcr_dataset,
                  method="glm",
                  trControl=control_pr)

# Evaluación rápida del modelo logístico con cv (punto de corte 0.5)
logi_pr
sal<-logi_pr$pred 

salconfu<-confusionMatrix(sal$pred,sal$obs,positive="Yes")
salconfu

# Matriz de confusión                   # Cuántas observaciones "Yes", "No"

                  #Reference            
      #Prediction   No   Yes            Obs. originales "No" = 5453
            #No   3131  1702            Obs. originales "Yes" = 7157
            #Yes  2322  5455

# Accuracy: 0.6809
# Sensitivity: 0.7622
# Specificity: 0.5742

# Calculamos AUC y curva ROC de este modelo
curvaroc <- roc(response = sal$obs, predictor = sal$Yes)
auc <- curvaroc$auc
auc
plot(roc(response=sal$obs,predictor=sal$Yes))


# Random Forest Sin Tunear -------------------------------------------

# Random Forest sin tunear
control_rf_inicial <- trainControl(method = "cv", number=4, savePredictions = "all", 
                                   classProbs = TRUE)

modelo_rf_inicial <- train(factor(promotion)~.,
                         data = ivcr_dataset, method = "rf", trControl = control_rf_final,
                         linout=FALSE, ntree=300, nodesize=10, replace=TRUE,
                         importance=TRUE)
modelo_rf_inicial

sal_modelo_rf_inicial <- modelo_rf_inicial$pred


# Matriz de confusión 
confusionMatrix(reference = sal_modelo_rf_inicial$obs, data = sal_modelo_rf_inicial$pred,
                positive = "Yes")

# Curva ROC

curvaroc_modelo_rf_inicial <- roc(response = sal_modelo_rf_inicial$obs,
                                  predictor = sal_modelo_rf_inicial$Yes)

auc_modelo_rf_inicial <- curvaroc_modelo_rf_inicial$auc
auc_modelo_rf_inicial

plot(roc(response = sal_modelo_rf_final$obs, predictor = sal_modelo_rf_final$Yes))

barplot(tabla_pr$MeanDecreaseAccuracy, names.arg = rownames(tabla_pr))


# Comparación de la función logística con Random Forest ---------------


# Diferencia de accuracy = (RandomForest: 0.7412) - (ModeloLogístico: 0.6809) = 0.0603

# Observaciones de acurracy correspondientes a la diferencia de algoritmos: 
# 0.0603 * 12610 (observaciones) = 760 observaciones de diferencia

# Comparando Random Forest con el modelo logístico, vemos que el Random Forest
# tiene un Accuracy notablemente más elevado que la Regresión Logística. Esto
# parece indicar que merecerá la pena un algorirmo complejo con separación no
# lineal. Merecerá la pena explorar algoritmos de separación no lineal para 
# obtener mejores resultados de predicción.No hay problemas en cuanto a pocas
# observaciones "Yes".


# *****************************************************************************
# ****************      SELECCIÓN DE VARIABLES      ***************************
# *****************************************************************************

source("funcion steprepetido binaria.R")

archivo1 <- ivcr_dataset

dput(names(archivo1))

vardep <- "promotion"

nombres1 <- c("destination.NoUrgentPlace", "destination.UrgentPlace", "passanger.Alone", 
              "passanger.Friends", "passanger.Kids", "passanger.Partner", "maritalStatus.Divorced", 
              "maritalStatus.MarriedPartner", "maritalStatus.Single", "maritalStatus.UnmarriedPartner", 
              "maritalStatus.Widowed", "occupation.Employee", "occupation.Retired", 
              "occupation.Student", "occupation.Unemployed", "coupon.Bar", 
              "coupon.CarryOutTakeAway", "coupon.CoffeeHouse", "coupon.RestaurantBetw2050", 
              "coupon.RestaurantBelow20", "weather.AdverseClimatology", "weather.Sunny", 
              "temperature", "time", "expiration", "gender", "age", "has_children", 
              "education", "income", "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", 
              "Restaurant20To50", "direction_same", "time_toCoupon")

y<-archivo1[,vardep]
x<-archivo1[,nombres1]

# Comenzamos a probar los diferentes modelos de selección de variables

#
#
# AIC -------------------------------------------------------------------------------------------
#
#
             
# Convertimos los valores de archivo1 de "Yes" y "No" a "1" y "0" para que funcione
# este método de selección de variables.
archivo1 <- archivo1 |>   mutate(promotion = ifelse(promotion == "Yes", 1,0))

library(MASS)

full<-glm(promotion~.,data=archivo1,family = binomial(link="logit"))
null<-glm(promotion~1,data=archivo1,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = binomial(link="logit"),trace=FALSE)

vec<-(names(selec1[[1]]))

length(vec)

# 20 variables (21-1)
dput(vec)

c("(Intercept)", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
  "expiration", "destination.UrgentPlace", "CoffeeHouse", "weather.AdverseClimatology", 
  "passanger.Kids", "education", "direction_same", "coupon.Bar", 
  "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
  "passanger.Alone", "coupon.CoffeeHouse", "age", "occupation.Employee", 
  "maritalStatus.Divorced", "income")
             
#
#
# BIC -------------------------------------------------------------------------------------------
# 
#
             
# k=log(n), n=12610, k=9.

full<-glm(promotion~.,data=archivo1,family = binomial(link="logit"))
null<-glm(promotion~1,data=archivo1,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = binomial(link="logit"),trace=FALSE,k=9)

vec<-(names(selec1[[1]]))

length(vec)

# 14 variables (15-1)
dput(vec)

c("(Intercept)", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
  "expiration", "destination.UrgentPlace", "CoffeeHouse", "weather.AdverseClimatology", 
  "passanger.Kids", "education", "direction_same", "coupon.Bar", 
  "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single")
             
#
#
# Boruta -----------------------------------------------------------------------------------------
# 
#
             
library(Boruta)
out.boruta <- Boruta(promotion~., data = archivo1)

print(out.boruta)

summary(out.boruta)

# Boruta performed 12 iterations in 6.992215 mins.
# 36 attributes confirmed important: age, Bar, CarryAway, CoffeeHouse, coupon.Bar and 31 more;
# 1 attributes confirmed unimportant: maritalStatus.Widowed;

sal<-data.frame(out.boruta$finalDecision)

sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
dput(row.names(sal2))

length(dput(row.names(sal2)))

# 36 variables

c("destination.NoUrgentPlace", "destination.UrgentPlace", "passanger.Alone", 
  "passanger.Friends", "passanger.Kids", "passanger.Partner", "maritalStatus.Divorced", 
  "maritalStatus.MarriedPartner", "maritalStatus.Single", "maritalStatus.UnmarriedPartner", 
  "occupation.Employee", "occupation.Retired", "occupation.Student", 
  "occupation.Unemployed", "coupon.Bar", "coupon.CarryOutTakeAway", 
  "coupon.CoffeeHouse", "coupon.RestaurantBetw2050", "coupon.RestaurantBelow20", 
  "weather.AdverseClimatology", "weather.Sunny", "temperature", 
  "time", "expiration", "gender", "age", "has_children", "education", 
  "income", "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", 
  "Restaurant20To50", "direction_same", "time_toCoupon")

#
#
# MXM -------------------------------------------------------------------------------------------
# 
#
             
library(MXM)

mmpc2 <- MMPC(vardep, archivo1, max_k = 3, hash = TRUE, test = "testIndLogistic")

mmpc2@selectedVars

a<-dput(names(archivo1[,c(mmpc2@selectedVars)]))

length(a)

a

# 15 variables

c("destination.NoUrgentPlace", "passanger.Friends", "maritalStatus.Single", 
  "coupon.Bar", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
  "weather.Sunny", "expiration", "gender", "age", "education", 
  "Bar", "CoffeeHouse", "Restaurant20To50", "time_toCoupon")


#
#
# RFE -------------------------------------------------------------------------------------------
#
#
             
# ("promotion" debe aparecer de nuevo como num (1 y 0). También para SBF.

source("cruzadas avnnet y log binaria.R")
             
archivo1$promotion<-as.factor(archivo1$promotion)

# RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=2)

# run the RFE algorithm
results <- rfe(x, y, sizes=c(1:37), rfeControl=control)

cosa <- as.data.frame(results$results)

# Resultados en gráfico
ggplot(cosa,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+ 
  scale_y_continuous(breaks = cosa$Accuracy) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")

selecrfe<-results$optVariables[1:18]  
dput(selecrfe)


# El RFE selecciona 18 variables
c("CoffeeHouse", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
  "coupon.Bar", "Bar", "expiration", "coupon.CoffeeHouse", "income", 
  "time", "Restaurant20To50", "education", "RestaurantLessThan20", 
  "CarryAway", "gender", "age", "has_children", "destination.NoUrgentPlace", 
  "temperature")

#
#
# Step Repetido (AIC - BIC) -----------------------------------------------------------------------------
# 
#
             
archivo1$promotion<-ifelse(archivo1$promotion==1,"Yes","No")

# StepRepetido con AIC
lista<-steprepetidobinaria(data=archivo1,vardep=c("promotion"),
                           listconti=nombres1,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# Selecciona 18 variables

c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
  "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
  "passanger.Kids", "coupon.Bar", "direction_same", "education", 
  "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
  "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
  "age")

# StepRepetido con BIC
lista2<-steprepetidobinaria(data=archivo1,vardep=c("promotion"),
                           listconti=nombres1,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")


tabla<-lista2[[1]]
dput(lista2[[2]][[1]])

# Selecciona 13 variables

c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
  "destination.NoUrgentPlace", "weather.AdverseClimatology", "CoffeeHouse", 
  "passanger.Kids", "education", "direction_same", "coupon.Bar", 
  "gender", "Restaurant20To50", "occupation.Retired")
             
#
#
# SBF -------------------------------------------------------------------------------------------
#
#
             
filtro<-sbf(x,y,sbfControl = sbfControl(functions = rfSBF,
                                        method = "cv", verbose = FALSE))

a<-dput(filtro$optVariables)

length(a)

# 30 variables

c("destination.NoUrgentPlace", "destination.UrgentPlace", "passanger.Alone", 
  "passanger.Friends", "passanger.Kids", "maritalStatus.MarriedPartner", 
  "maritalStatus.Single", "maritalStatus.Widowed", "occupation.Retired", 
  "occupation.Student", "coupon.Bar", "coupon.CarryOutTakeAway", 
  "coupon.CoffeeHouse", "coupon.RestaurantBetw2050", "coupon.RestaurantBelow20", 
  "weather.AdverseClimatology", "weather.Sunny", "temperature", 
  "expiration", "gender", "age", "has_children", "education", "income", 
  "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", "Restaurant20To50", 
  "time_toCoupon")

             
# COMPARACIÓN DE CONJUNTOS MEDIANTE REGRESIÓN LOGÍSTICA

# Ponemos las variables que han sido seleccionadas por cada modelo. 
# Se incluyen todas en "listconti".

source("cruzadas avnnet y log binaria.R")

data <- archivo1

medias1<-cruzadalogistica(data=data,
                          vardep="promotion",listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
                                                         "expiration", "destination.UrgentPlace", "CoffeeHouse", "weather.AdverseClimatology", 
                                                         "passanger.Kids", "education", "direction_same", "coupon.Bar", 
                                                         "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                                         "passanger.Alone", "coupon.CoffeeHouse", "age", "occupation.Employee", 
                                                         "maritalStatus.Divorced", "income"),
                          listclass=c(""),
                          grupos=4,sinicio=1234,repe=25)

medias1$modelo="STEPAIC"

medias2<-cruzadalogistica(data=data,
                          vardep="promotion",listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
                                                         "expiration", "destination.UrgentPlace", "CoffeeHouse", "weather.AdverseClimatology", 
                                                         "passanger.Kids", "education", "direction_same", "coupon.Bar", 
                                                         "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias2$modelo="STEPBIC"

medias3<-cruzadalogistica(data=data,
                          vardep="promotion",listconti=c("destination.NoUrgentPlace", "destination.UrgentPlace", "passanger.Alone", 
                                                         "passanger.Friends", "passanger.Kids", "passanger.Partner", "maritalStatus.Divorced", 
                                                         "maritalStatus.MarriedPartner", "maritalStatus.Single", "maritalStatus.UnmarriedPartner", 
                                                         "occupation.Employee", "occupation.Retired", "occupation.Student", 
                                                         "occupation.Unemployed", "coupon.Bar", "coupon.CarryOutTakeAway", 
                                                         "coupon.CoffeeHouse", "coupon.RestaurantBetw2050", "coupon.RestaurantBelow20", 
                                                         "weather.AdverseClimatology", "weather.Sunny", "temperature", 
                                                         "time", "expiration", "gender", "age", "has_children", "education", 
                                                         "income", "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", 
                                                         "Restaurant20To50", "direction_same", "time_toCoupon"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo="Boruta"

medias4<-cruzadalogistica(data=data,
                          vardep="promotion",listconti=c("destination.NoUrgentPlace", "passanger.Friends", "maritalStatus.Single", 
                                                         "coupon.Bar", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
                                                         "weather.Sunny", "expiration", "gender", "age", "education", 
                                                         "Bar", "CoffeeHouse", "Restaurant20To50", "time_toCoupon"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias4$modelo="MXM"

medias5<-cruzadalogistica(data=data,
                     vardep="promotion",listconti=c("CoffeeHouse", "coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", 
                                                    "coupon.Bar", "Bar", "expiration", "coupon.CoffeeHouse", "income", 
                                                    "time", "Restaurant20To50", "education", "RestaurantLessThan20", 
                                                    "CarryAway", "gender", "age", "has_children", "destination.NoUrgentPlace", 
                                                    "temperature"),
                     listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="RFE"

medias6<-cruzadalogistica(data=data,
                    vardep="promotion",listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                                   "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                                   "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                                   "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                                   "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                                   "age"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias6$modelo="STEPrep1"

medias7<-cruzadalogistica(data=data,
                    vardep="promotion",listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                                   "destination.NoUrgentPlace", "weather.AdverseClimatology", "CoffeeHouse", 
                                                   "passanger.Kids", "education", "direction_same", "coupon.Bar", 
                                                   "gender", "Restaurant20To50", "occupation.Retired"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias7$modelo="STEPrep2"

medias8<-cruzadalogistica(data=data,
                    vardep="promotion",listconti=c("destination.NoUrgentPlace", "destination.UrgentPlace", "passanger.Alone", 
                                                   "passanger.Friends", "passanger.Kids", "maritalStatus.MarriedPartner", 
                                                   "maritalStatus.Single", "maritalStatus.Widowed", "occupation.Retired", 
                                                   "occupation.Student", "coupon.Bar", "coupon.CarryOutTakeAway", 
                                                   "coupon.CoffeeHouse", "coupon.RestaurantBetw2050", "coupon.RestaurantBelow20", 
                                                   "weather.AdverseClimatology", "weather.Sunny", "temperature", 
                                                   "expiration", "gender", "age", "has_children", "education", "income", 
                                                   "Bar", "CoffeeHouse", "CarryAway", "RestaurantLessThan20", "Restaurant20To50", 
                                                   "time_toCoupon"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias8$modelo="SBF"

# Creación de boxplots para comparar los ocho modelos
union_prueba<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8)

par(cex.axis=1, las=1)
boxplot(data=union_prueba,col="grey",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=1)
boxplot(data=union_prueba,col="grey",auc~modelo,main="AUC")


# El mejor set de variables es el del STEPrep1
c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
  "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
  "passanger.Kids", "coupon.Bar", "direction_same", "education", 
  "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
  "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
  "age")




# *****************************************************************************
# *********************      TUNEO DE ALGORITMOS     **************************
# *****************************************************************************

#
# Tuneo Red Neuronal ------------------------------------------------------------------------------------
#
             
vardep<-"promotion"

listconti<- c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
              "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
              "passanger.Kids", "coupon.Bar", "direction_same", "education", 
              "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
              "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
              "age")

paste(listconti,collapse = "+")

control<-trainControl(method = "cv",number=2,savePredictions = "all")

avnnetgrid <-expand.grid(size=c(4,7,9,11,13,15),
                         decay=c(0.001, 0.01, 0.1),bag=FALSE)

redavnnet<- train(promotion ~
                    coupon.CarryOutTakeAway+coupon.RestaurantBelow20+expiration+
                    weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
                    passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
                    gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
                    passanger.Alone+occupation.Employee+age,
                  data=archivo1,method="avNNet",linout = TRUE, maxit=200,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)
redavnnet

# Representación tuneo de parámetros

library(ggplot2)
ggplot(redavnnet$results, aes(x = factor(size), y = Accuracy, color = factor(decay))) +
  geom_point(position = position_dodge(width = 1), size = 5) +
  labs(x = "Size", y = "Accuracy", color = "Decay") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        aspect.ratio = 0.8)
             
#
# Tuneo Bagging --------------------------------------------------------------------------------------
#
             
# Necesario uso de semilla
set.seed(12345)

# Para mostrar el error a medida que van avanzando las iteraciones, se usa "randomForest"

rf_ba_bis <- randomForest(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
                            expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
                            passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
                            gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
                            passanger.Alone+occupation.Employee+age,
              data = archivo1, mtry = 18, ntree = 5000, sampsize = 300, nodesize = 10, replace = TRUE)

plot(rf_ba_bis$err.rate[,1])

# Podemos manipular el tamaño muestral para observar su efecto sobre el modelo bagging, incorporando
# el parámetro sampsize en la función cruzadarfbin. Se prueban diferentes tamaños de sampsize

medias_ba_50 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                            listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                            "age"),
                      listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                      nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                      sampsize = 50)

medias_ba_50$modelo = "Bagging-50"

medias_ba_100 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                             sampsize = 100)

medias_ba_100$modelo = "Bagging-100"

medias_ba_200 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti = c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                             sampsize = 200)

medias_ba_200$modelo = "Bagging-200"

medias_ba_500 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti = c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                             sampsize = 500)

medias_ba_500$modelo = "Bagging-500"

medias_ba_1000 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                             sampsize = 1000)

medias_ba_1000$modelo = "Bagging-1000"

medias_ba_3000 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE,
                             sampsize = 3000)

medias_ba_3000$modelo = "Bagging-3000"

medias_ba_BASE <- cruzadarfbin(data = archivo1, vardep = "promotion",
                             listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                             "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                             "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                             "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                             "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                             "age"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 18, ntree = 300, replace = TRUE)

medias_ba_BASE$modelo = "Bagging-BASE"

# Comparación de modelos con diferentes "sampsize".

union_modelos<-rbind(medias_ba_50, medias_ba_100, medias_ba_200, medias_ba_500, 
                     medias_ba_1000, medias_ba_3000, medias_ba_BASE)

par(cex.axis=2.5)
boxplot(data=union_modelos,col="grey",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=1.3)
boxplot(data=union_modelos,col="grey",auc~modelo,main="AUC", cex.main = 2)

# Puede comprobarse que el que mejor funciona es el sampsize=3000.


#
# Tuneo Random Forest -------------------------------------------------------------------------------------
#
             
# Random Forest (se prueba con mtry=6 para importancia de variables)
set.seed(12345)
rfgrid<-expand.grid(mtry=c(6))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf_todas<- train(factor(promotion)~.,data=archivo1,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf_todas

# Importancia de variables
final_todas <- rf_todas$finalModel
tabla_todas <- as.data.frame(importance(final_todas))
tabla_todas <- tabla_todas[order(-tabla_todas$MeanDecreaseAccuracy),]


colores <- rep("green", nrow(tabla_todas))
par(mar = c(5, 20, 4, 2))
par(cex.axis = 0.53)
par(cex.lab = 0.53)

barplot(tabla_todas$MeanDecreaseAccuracy[order(tabla_todas$MeanDecreaseAccuracy)],
        names.arg = rownames(tabla_todas)[order(tabla_todas$MeanDecreaseAccuracy)],
        col = colores, horiz = TRUE, las=1)


# Error a medida que avanzan las iteraciones
set.seed(12345)

rfbis <- randomForest(factor(promotion)~.,
                      data=archivo1,
                      metry=6,ntree=3000,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

# Parece que las variables "time", "income" y "Bar" también son importantes para 
# hacer randomForest. Serán incluidas al conjunto ganador, pero solamente
# para ejecutar este modelo.

# Segundo Tuneado sólo con las variables de interés.

ganadoras_RanFo <- c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                     "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                     "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                     "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                     "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                     "age", "time", "income", "Bar")
paste(ganadoras_RanFo,collapse="+")

set.seed(12345)
             
# Ahora sí probamos diferentes valores para elegir el mejor mtry
rfgrid <- expand.grid(mtry=c(4,5,6,7,8,9,10,11))

control <- trainControl(method = "cv", number=4, savePredictions = "all",
                        classProbs = TRUE)

rf<- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
             expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
             passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+gender+
             occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+passanger.Alone+
             occupation.Employee+age+time+income+Bar,
           data=archivo1,method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

final <- rf$finalModel

# Escogemos mtry = 6

# Tuneo de sampsize (igual que en bagging)

medias_rf_50 <- cruzadarfbin(data = archivo1, vardep = "promotion",listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                          "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                          "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                          "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                          "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                          "age", "time", "income", "Bar"),
                             listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                             nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                             sampsize = 50)

medias_rf_50$modelo = "Rand_For-50"

medias_rf_100 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                              listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                           "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                           "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                           "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                           "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                           "age", "time", "income", "Bar"),
                              listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                              nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                              sampsize = 100)

medias_rf_100$modelo = "Rand_For-100"

medias_rf_200 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                              listconti = c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                            "age", "time", "income", "Bar"),
                              listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                              nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                              sampsize = 200)

medias_rf_200$modelo = "Rand_For-200"

medias_rf_500 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                              listconti = c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                            "age", "time", "income", "Bar"),
                              listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                              nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                              sampsize = 500)

medias_rf_500$modelo = "Rand_For-500"

medias_rf_1000 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                               listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                            "age", "time", "income", "Bar"),
                               listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                               nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                               sampsize = 1000)

medias_rf_1000$modelo = "Rand_For-1000"

medias_rf_3000 <- cruzadarfbin(data = archivo1, vardep = "promotion",
                               listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                            "age", "time", "income", "Bar"),
                               listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                               nodesize = 10, mtry = 6, ntree = 300, replace = TRUE,
                               sampsize = 3000)

medias_rf_3000$modelo = "Rand_For-3000"

medias_rf_BASE <- cruzadarfbin(data = archivo1, vardep = "promotion",
                               listconti =c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                            "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                            "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                            "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                            "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                            "age", "time", "income", "Bar"),
                               listclass=c(""), grupos = 10, sinicio = 1234, repe = 20,
                               nodesize = 10, mtry = 6, ntree = 300, replace = TRUE)

medias_rf_BASE$modelo = "Rand_For-BASE"

# Comparamos los modelos con diferentes "sampsize".

union_modelos<-rbind(medias_rf_50, medias_rf_100, medias_rf_200, medias_rf_500, 
                     medias_rf_1000, medias_rf_3000, medias_rf_BASE)

par(cex.axis=1.5)
boxplot(data=union_modelos,col="grey",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=1.5)
boxplot(data=union_modelos,col="grey",auc~modelo,main="AUC", cex.main = 1.8)

# Ahora, el mejor sampsize es el BASE (máximo tamaño muestral)

#
# Tuneo Gradient Boosting -------------------------------------------------------------------------------------
#
             
# Probamos muchas posibilidades con un grid de parámetros para ver cuál es la que mejor funciona

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,4000,6000,8000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
              expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
              passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
              gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
              passanger.Alone+occupation.Employee+age,
            data=archivo1,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm)


# Fijamos algunos parámetros para ver cómo evoluciona en función de las iteraciones.
# Se han fijado los que mejores resultados han proporcionado en la prueba anterior.

gbmgrid<-expand.grid(shrinkage=c(0.2),n.minobsinnode=c(5),
                     n.trees=c(50,100,500,1000,2000,3000,4000,5000,6000,8000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
              expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
              passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
              gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
              passanger.Alone+occupation.Employee+age,
              data=archivo1,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

#  Importancia de las variables para GBM

par(cex=0.5)
summary(gbm)

tabla<-summary(gbm)
par(cex = 0.7, las = 1, mar = c(7, 15, 4, 4))

barplot(tabla$rel.inf,names.arg=row.names(tabla))

#
# Tuneo SVM --------------------------------------------------------------------------
#
             
# Tuneo del parámetro C en SVM (LINEAL):

set.seed(12345)

grid_SVM_lineal <- expand.grid(C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 50))

SVM_lineal <- train(promotion ~ coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
                      expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
                      passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
                      gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
                      passanger.Alone+occupation.Employee+age,
                    data = archivo1, method = "svmLinear", 
                    trControl = trainControl(method = "repeatedcv", number = 4, repeats = 5,
                                             savePredictions = "all", classProbs = TRUE),
                                             tuneGrid = grid_SVM_lineal, verbose = FALSE)

SVM_lineal
SVM_lineal$results
plot(SVM_lineal$results$C, SVM_lineal$results$Accuracy)

# Gráfico mejorado

library(ggplot2)

results_df <- data.frame(C = as.factor(SVM_lineal$results$C), Accuracy = SVM_lineal$results$Accuracy)

num_points <- nrow(results_df)
colores <- rainbow(num_points)

ggplot(results_df, aes(x = C, y = Accuracy, color = C)) +
  geom_point(size = 5) +
  scale_color_manual(values = colores) +
  labs(x = "C", y = "Accuracy") 


# SVM polinomial

# Tuneo de los parámetros C y sigma en SVM (RBF)

set.seed(12345)

grid_SVM_radial <- expand.grid(C = c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30), 
                               sigma = c(0.0001, 0.005, 0.01, 0.05))
  
SVM_radial <- train(promotion ~ coupon.CarryOutTakeAway+coupon.RestaurantBelow20+
                      expiration+weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+
                      passanger.Kids+coupon.Bar+direction_same+education+Restaurant20To50+
                      gender+occupation.Retired+maritalStatus.Single+coupon.CoffeeHouse+
                      passanger.Alone+occupation.Employee+age, 
                    data = archivo1, method = "svmRadial", 
                    trControl = trainControl(method = "repeatedcv", number = 4, repeats = 5,
                                             savePredictions = "all", classProbs = TRUE),
                                              tuneGrid = grid_SVM_radial, verbose = FALSE)

SVM_radial
SVM_radial$results
ggplot(SVM_radial$results, aes(x = factor(C), y = Accuracy, color = factor(sigma))) + 
  geom_point(position = position_dodge(width = 1), size = 3)


# *****************************************************************************
# *************************      ENSAMBLADO     *******************************
# *****************************************************************************

# Ya están decididos los parámetros para cada algoritmo

# 1. Lectura funciones necesarias
source("cruzadas ensamblado binaria fuente.R")

# 2. Preparamos el archivo, definimos las variables, semilla y repeticiones de CV

archivo <- archivo1
vardep <- "promotion"
listconti <- c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
              "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
              "passanger.Kids", "coupon.Bar", "direction_same", "education", 
              "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
              "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
              "age", "time", "income", "Bar")
listclass <- c("")
grupos <- 4
sinicio <- 1234
repe <- 15

# 3. Aplicación de funciones cruzadas para ensamblar

# Logística
medias1_ens <- cruzadalogistica(data=archivo,vardep=vardep,listconti=listconti,
                                listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis <- as.data.frame(medias1_ens[1])
medias1bis$modelo <- "Logística"
predi1 <- as.data.frame(medias1_ens[2])
predi1$logi <- predi1$Yes


# Red Neuronal
medias2_ens <- cruzadaavnnetbin(data=archivo,vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       size=c(7),decay=c(0.01),repeticiones=5,itera=200)

medias2bis <- as.data.frame(medias2_ens[1])
medias2bis$modelo <- "Red"
predi2 <- as.data.frame(medias2_ens[2])
predi2$avnnet <- predi2$Yes

# Bagging
medias3_ens <- cruzadarfbin(data=archivo,vardep=vardep,listconti=listconti,
                         listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                         mtry=18,ntree=300,nodesize=10,sampsize=3000,replace=TRUE)

medias3bis <- as.data.frame(medias3_ens[1])
medias3bis$modelo <- "Bagging"
predi3 <- as.data.frame(medias3_ens[2])
predi3$rf <- predi3$Yes

# Random Forest
medias4_ens <- cruzadarfbin(data=archivo,vardep=vardep,listconti=listconti,
                   listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                   mtry=6,ntree=300,nodesize=10,replace=TRUE)

medias4bis <- as.data.frame(medias4_ens[1])
medias4bis$modelo <- "RF"
predi4 <- as.data.frame(medias4_ens[2])
predi4$rf <- predi4$Yes

# Gradient Boosting
medias5_ens <- cruzadagbmbin(data=archivo,vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                    n.minobsinnode=5,shrinkage=0.2,n.trees=5000,interaction.depth=2)

medias5bis <- as.data.frame(medias5_ens[1])
medias5bis$modelo <- "GBM"
predi5 <- as.data.frame(medias5_ens[2])
predi5$gbm <- predi5$Yes

# SVM lineal
medias6_ens <- cruzadaSVMbin(data=archivo,vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                    C=0.1)

medias6bis <- as.data.frame(medias6_ens[1])
medias6bis$modelo <- "SVM Lineal"
predi6 <- as.data.frame(medias6_ens[2])
predi6$svmLinear <- predi6$Yes

# SVM radial (SBF)
medias7_ens <- cruzadaSVMbinRBF(data=archivo,vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       C=0.2,sigma=0.05)

medias7bis <- as.data.frame(medias7_ens[1])
medias7bis$modelo <- "SVM Radial"
predi7 <- as.data.frame(medias7_ens[2])
predi7$svmRadial <- predi7$Yes

# Unimos los modelos y graficamos
union1_ens <- rbind(medias1bis, medias2bis, medias3bis, medias4bis, 
                    medias5bis, medias6bis, medias7bis)
par(cex.axis = 2, mar=c(10,6,4,2), las=1)
boxplot(data = union1_ens, tasa~modelo,  col="grey", main="TASA DE FALLOS", cex.main = 2, cex.lab=1.5)
boxplot(data = union1_ens, auc~modelo, col="grey", main="AUC", cex.main = 2, cex.lab=1.5)



# 4. Construcción de ensamblados a partir del archivo unipredi de predicciones. Se están 
# realizando promedios, aunque podrían darse ponderaciones diferentes a cada modelo.

unipredi <- cbind(predi1, predi2, predi3, predi4, predi5, predi6, predi7)
unipredi <- unipredi[, !duplicated(colnames(unipredi))]

# Construcción de los ensamblados
unipredi$predi8 <- (unipredi$logi+unipredi$avnnet)/2
unipredi$predi9 <- (unipredi$logi+unipredi$rf)/2
unipredi$predi10 <- (unipredi$logi+unipredi$bag)/2
unipredi$predi11 <- (unipredi$logi+unipredi$gbm)/2
unipredi$predi12 <- (unipredi$logi+unipredi$svmLinear)/2
unipredi$predi13 <- (unipredi$logi+unipredi$svmRadial)/2
unipredi$predi14 <- (unipredi$avnnet+unipredi$rf)/2
unipredi$predi15 <- (unipredi$avnnet+unipredi$bag)/2
unipredi$predi16 <- (unipredi$avnnet+unipredi$gbm)/2
unipredi$predi17 <- (unipredi$avnnet+unipredi$svmLinear)/2
unipredi$predi18 <- (unipredi$avnnet+unipredi$svmRadial)/2
unipredi$predi19 <- (unipredi$rf+unipredi$bag)/2
unipredi$predi20 <- (unipredi$rf+unipredi$gbm)/2
unipredi$predi21 <- (unipredi$rf+unipredi$svmLinear)/2
unipredi$predi22 <- (unipredi$rf+unipredi$svmRadial)/2
unipredi$predi23 <- (unipredi$gbm+unipredi$bag)/2
unipredi$predi24 <- (unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi25 <- (unipredi$gbm+unipredi$svmRadial)/2
unipredi$predi26 <- (unipredi$bag+unipredi$svmLinear)/2
unipredi$predi27 <- (unipredi$bag+unipredi$svmRadial)/2

unipredi$predi28 <- (unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$predi29 <- (unipredi$logi+unipredi$avnnet+unipredi$bag)/3
unipredi$predi30 <- (unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi31 <- (unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi32 <- (unipredi$logi+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi33 <- (unipredi$logi+unipredi$rf+unipredi$bag)/3
unipredi$predi34 <- (unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi35 <- (unipredi$logi+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi36 <- (unipredi$logi+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi37 <- (unipredi$logi+unipredi$bag+unipredi$gbm)/3
unipredi$predi38 <- (unipredi$logi+unipredi$bag+unipredi$svmLinear)/3
unipredi$predi39 <- (unipredi$logi+unipredi$bag+unipredi$svmRadial)/3
unipredi$predi40 <- (unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi41 <- (unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi42 <- (unipredi$avnnet+unipredi$rf+unipredi$bag)/3
unipredi$predi43 <- (unipredi$avnnet+unipredi$rf+unipredi$gbm)/3
unipredi$predi44 <- (unipredi$avnnet+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi45 <- (unipredi$avnnet+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi46 <- (unipredi$avnnet+unipredi$bag+unipredi$gbm)/3
unipredi$predi47 <- (unipredi$avnnet+unipredi$bag+unipredi$svmLinear)/3
unipredi$predi48 <- (unipredi$avnnet+unipredi$bag+unipredi$svmRadial)/3
unipredi$predi49 <- (unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi50 <- (unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi51 <- (unipredi$rf+unipredi$bag+unipredi$gbm)/3
unipredi$predi52 <- (unipredi$rf+unipredi$bag+unipredi$svmLinear)/3
unipredi$predi53 <- (unipredi$rf+unipredi$bag+unipredi$svmRadial)/3
unipredi$predi54 <- (unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi55 <- (unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi56 <- (unipredi$bag+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi57 <- (unipredi$bag+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi58 <- (unipredi$bag+unipredi$rf+unipredi$gbm+unipredi$logi)/4

unipredi$predi59 <- (unipredi$bag+unipredi$rf+unipredi$gbm+unipredi$logi+unipredi$avnnet)/5


# 5. Procesado de los ensamblados, construyendo los promedios de tasa de fallos por cada
#    repetición de CV. Para ello se aplica el punto de corte, calcula la tasa de fallos
#    y se promedia por repeticiones de CV

dput(names(unipredi))

listado_ens <- c("logi", "avnnet", "rf", "bagging", "gbm", "svmLinear", "svmRadial",
                  "predi8", "predi9", "predi10", "predi11", "predi12", "predi13",
                 "predi14","predi15", "predi16", "predi17", "predi18", "predi19",
                 "predi20","predi21", "predi22", "predi23", "predi24", "predi25",
                 "predi26","predi27", "predi28", "predi29", "predi30", "predi31",
                 "predi32","predi33", "predi34", "predi35", "predi36", "predi37",
                 "predi38","predi39", "predi40", "predi41", "predi42", "predi43",
                 "predi44","predi45", "predi46", "predi47", "predi48", "predi49",
                 "predi50","predi51", "predi52", "predi53", "predi54", "predi55",
                 "predi56","predi57", "predi58", "predi59")

# Definimos la función "tasafallos"
tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

# Definimos la función "auc"
auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el número de repeticiones de validación cruzada y se calculan las
# medias de tasafallos y AUC por repetición en el dataframe "medias0"
             
repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)

medias0<-data.frame(c())
for (prediccion in listado_ens)
{
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}


# 6. Graficamos el boxplot con todos (con tasa y con AUC)
par(cex.axis=0.5,las=2)
boxplot(data=medias0,tasa~modelo,col="green",main="TASA FALLOS")
boxplot(data=medias0,auc~modelo,col="green",main="AUC")


# Tablas ordenadas para observar (por tasa y por AUC)

# Tasa de fallos
promedios_tasa <- aggregate(tasa ~ modelo, medias0, mean)
promedios_tasa <- promedios_tasa[order(promedios_tasa$tasa), ]
# AUC
promedios_auc <- aggregate(auc ~ modelo, medias0, mean)
promedios_auc <- promedios_auc[order(-promedios_auc$auc), ]

# Boxplot ordenado (por tasa y por AUC)
# Tasa de fallos
medias0$modelo <- with(medias0,reorder(modelo,tasa, mean))
par(cex.axis=1, mar = c(10, 4, 4, 2), las=2)
boxplot(data=medias0,tasa~modelo,col="green", main='TASA DE FALLOS', cex.main = 1.8)
# AUC
medias0$modelo <- with(medias0,reorder(modelo,auc, mean))
par(cex.axis=1,las=2, mar = c(10, 4, 4, 2))
boxplot(data=medias0,auc~modelo,col="green", main='AUC', cex.main = 1.8)


# Los 7 mejores ensamblados

listadobis<-c("predi55","predi14","predi42","predi43","predi20","predi51","predi19")

medias0$modelo<-as.character(medias0$modelo)
mediasver<-medias0[medias0$modelo %in% listadobis,]
mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))
par(cex.axis=2.3,las=2, mar = c(10, 8, 4, 2))
boxplot(data=mediasver,auc~modelo,col="grey",main='AUC', cex.main=2.3)


# *****************************************************************************
# ****************    SELECCIÓN DEL MODELO Y POST ANÁLISIS     ****************
# *****************************************************************************

# Comparación de modelos 

source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")
source ("cruzada gbm binaria.R")
source ("cruzada SVM binaria lineal.R")
source ("cruzada SVM binaria RBF.R")

# Esto si se deseasen ejecutar de nuevo sin ensamblados. Pueden utilizarse los calculados anteriormente.

medias_red <- cruzadaavnnetbin(data=archivo1,
                        vardep="promotion",
                        listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                     "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                     "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                     "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                     "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                     "age"),
                         listclass=c(""), grupos=4,sinicio=1234,repe=25,repeticiones=5,
                                        itera=200,size=c(7),decay=c(0.01))
medias_red$modelo="Red"

medias_bag <- cruzadarfbin(data=archivo1, vardep="promotion",
                         listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                     "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                     "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                     "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                     "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                     "age"),
                         listclass=c(""), grupos=4, sinicio=1234, repe=5, nodesize=10,
                                          mtry=18, ntree=300, sampsize=3000, replace=TRUE)

medias_bag$modelo = "Bagging"

medias_randfo <- cruzadarfbin(data=archivo1, vardep="promotion",
                         listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                    "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                     "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                    "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                    "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                    "age", "Bar", "time", "income"),
                         listclass=c(""), grupos=4, sinicio=1234, repe=10, nodesize=10,
                                          mtry=6,ntree=300,replace=TRUE)

medias_randfo$modelo="RandomForest"

medias_gbm <- cruzadagbmbin(data=archivo1, vardep="promotion",
                          listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                    "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                    "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                    "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                    "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                    "age"),
                          listclass=c(""), grupos=4,sinicio=1234,repe=10,n.minobsinnode=5,
                                           shrinkage=0.2,n.trees=5000,interaction.depth=2)

medias_gbm$modelo="GBM"

medias_svml <- cruzadaSVMbin(data=archivo1, vardep="promotion",
                            listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                      "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                      "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                      "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                      "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                      "age"),
                           listclass=c(""), grupos=4,sinicio=1234,repe=5,C=0.10)
                           
medias_svml$modelo = "SVM Lineal"

medias_svmr <- cruzadaSVMbinRBF(data=archivo1, vardep="promotion",
                            listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                          "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                          "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                          "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                          "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                          "age"),
                            listclass=c(""),grupos=4,sinicio=1234,repe=5,C=0.2,sigma=0.05)

medias_svmr$modelo = "SVM Radial"

medias_logi <- cruzadalogistica(data = archivo1, vardep="promotion",
                             listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                          "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                          "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                          "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                          "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                          "age"),
                             listclass=c(""), grupos=4, sinicio=1234, repe=5)

medias_logi$modelo = "Logística"

# Para hacer pruebas, también podría probarse un sólo árbol
medias_arb <- cruzadaarbolbin(data = archivo1, vardep="promotion",
                            listconti=c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                                        "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                                        "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                                        "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                                        "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                                        "age"),
                            listclass=c(""), grupos=4, sinicio=1234, repe=5, cp=c(0), minbucket=5)

medias_arb$modelo = "Árbol"

# Graficamos juntos todos los modelos anteriores y elegimos el mejor.
union_modelos<-rbind(medias_red,medias_bag,medias_randfo,medias_gbm,medias_svml,medias_svmr,medias_logi)

par(cex.axis = 1.8, mar = c(6, 6, 2, 2))
boxplot(data = union_modelos, col = "green", tasa ~ modelo, main = "TASA DE FALLOS", cex.main = 1.8)


par(cex.axis=1.8, mar = c(6,6,2,2))
boxplot(data=union_modelos,col="green",auc~modelo,main="AUC", cex.main = 1.8)


# *****************************************************************************
# *******************      ANÁLISIS Y CONCLUSIONES     ************************
# *****************************************************************************

# Evaluación del modelo

set.seed(1234)
variables_rf_final <- c("coupon.CarryOutTakeAway", "coupon.RestaurantBelow20", "expiration", 
                        "weather.AdverseClimatology", "CoffeeHouse", "destination.NoUrgentPlace", 
                        "passanger.Kids", "coupon.Bar", "direction_same", "education", 
                        "Restaurant20To50", "gender", "occupation.Retired", "maritalStatus.Single", 
                        "coupon.CoffeeHouse", "passanger.Alone", "occupation.Employee", 
                        "age", "Bar", "time", "income")

paste(variables_rf_final, collapse = "+")

control_rf_final <- trainControl(method = "cv", number=4, savePredictions = "all", classProbs = TRUE)

rfgrid_final <- expand.grid(mtry=c(6))

modelo_rf_final <- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+expiration+
                           weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+passanger.Kids+
                           coupon.Bar+direction_same+education+Restaurant20To50+gender+occupation.Retired+
                           maritalStatus.Single+coupon.CoffeeHouse+passanger.Alone+occupation.Employee+age+
                           Bar+time+income,
                         data = archivo1, method = "rf", trControl = control_rf_final, tuneGrid = rfgrid_final,
                         linout=FALSE, ntree=300, nodesize=10, replace=TRUE,
                         importance=TRUE)

modelo_rf_final
sal_modelo_rf_final <- modelo_rf_final$pred


# Matriz de confusión

# Medidas con punto de corte 0.5
confusionMatrix(reference = sal_modelo_rf_final$obs, data = sal_modelo_rf_final$pred, positive = "Yes")

# Medidas con punto de corte 0.3
corte <-0.3
sal_modelo_rf_final$predcorte <- ifelse(sal_modelo_rf_final$Yes>corte,"Yes","No")
sal_modelo_rf_final$predcorte <- as.factor(sal_modelo_rf_final$predcorte)

confusionMatrix(reference = sal_modelo_rf_final$obs, data = sal_modelo_rf_final$predcorte, positive = "Yes")


# Medidas con punto de corte 0.7
corte <-0.7
sal_modelo_rf_final$predcorte <- ifelse(sal_modelo_rf_final$Yes>corte,"Yes","No")
sal_modelo_rf_final$predcorte <- as.factor(sal_modelo_rf_final$predcorte)

confusionMatrix(reference = sal_modelo_rf_final$obs, data = sal_modelo_rf_final$predcorte, positive = "Yes")


# Curva ROC

curvaroc_modelo_rf_final <- roc(response = sal_modelo_rf_final$obs, predictor = sal_modelo_rf_final$Yes)

auc_modelo_rf_final <- curvaroc_modelo_rf_final$auc
auc_modelo_rf_final

plot(roc(response = sal_modelo_rf_final$obs, predictor = sal_modelo_rf_final$Yes))



########## Tabla parámetros logística ##########

control_logi_final <- trainControl(method = "none", savePredictions = "all", classProbs = TRUE)

logi_final <- train(factor(promotion)~coupon.CarryOutTakeAway+coupon.RestaurantBelow20+expiration+
                      weather.AdverseClimatology+CoffeeHouse+destination.NoUrgentPlace+passanger.Kids+
                      coupon.Bar+direction_same+education+Restaurant20To50+gender+occupation.Retired+
                      maritalStatus.Single+coupon.CoffeeHouse+passanger.Alone+occupation.Employee+age+
                      Bar+time+income,
                    data = archivo1, method = "glm", trControl = control_logi_final)

summary(logi_final)
                    
