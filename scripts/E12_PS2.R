#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Problem Set 2
#Equipo 12

#Jorge E. García
#Ingrid Lorena Molano
#Camilo Villa Moreno

#Julio 10, 2022

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. PRELIMINARES: PREPARACIÓN ESPACIO DE TRABAJO Y LIBRERÍAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Limpiar el entorno:
rm(list=ls())

#Instalar librerías
install.packages("rvest")
install.packages("fabricatr")
install.packages("stargazer")
install.packages('GGally')# Se instala un paquete para gráficos
install.packages("pacman")
install.packages("arsenal")
install.packages("janitor")
install.packages("gamlr")

#Cargar librerías:
library(rvest)
library(tidyverse)
library(fabricatr)
library(stargazer)
library(caret)
library(GGally)
library(tableone)
library(caret)
library(arsenal)
library(janitor)
require(pacman)
require(gamlr)
p_load(rio, 
       tidyverse, 
       skimr, 
       caret,
       rvest,
       stargazer)


#Recomendación de Eduard: 09/07/2022

#p_load instala y carga, en ese sentido es eficiente.
#Si no queremos usar p_load, install + require (o library)
#Pero no ambos, pues es redudante. Mantenerlo consistente.

install.packages("pacman")
library(pacman)

p_load(rio,
       doParallel,
       stargazer,
       fabricatr,
       tableone,
       arsenal,
       janitor,
       tidyverse,
       gamlr,
       skimr, 
       caret,
       rvest,
       stargazer,
       smotefamily,
       ROCR,
       pROC,
       rpart,
       rpart.plot,
       glmnet,
       xgboost)


## Resolver conflictos de paquetes
#(Definir cuáles variables usar)
predict <- stats::predict


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. PREPARACIÓN DE LA BASE DE DATOS Y ESTADÍSTICAS DESCRIPTIVAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##1.1. Cargue de las bases de datos ---- 

setwd("~/GitHub/MECA_BD_PS2")
submission_template <-read.csv("./stores/20220703_data/submission_template.csv")
test_hogares <-readRDS("./stores/20220703_data/test_hogares.rds")
test_personas <-readRDS("./stores/20220703_data/test_personas.rds")
train_hogares <-readRDS("./stores/20220703_data/train_hogares.rds")
train_personas <-readRDS("./stores/20220703_data/train_personas.rds")


##1.2. Exploración incial de los datos ----

##Exploración de las bases de datos:
skim(train_hogares)
skim(test_hogares)
skim(train_personas)
skim(test_personas)

#Variables en Hogares, comparación entre train y test:
colnames(train_hogares)
colnames(test_hogares)

#Resumen de diferencias
all_equal(train_hogares, test_hogares)
comparedf(train_hogares,test_hogares)

#Comparación de variables (columnas)
compare_df_cols(train_hogares, test_hogares)

#Comparación detallada:
summary(comparedf(train_hogares,test_hogares))


#Variables en Personas, comparación entre train y test:
colnames(train_personas)
colnames(test_personas)


#Resumen de diferencias
all_equal(train_personas,test_personas)
comparedf(train_personas,test_personas)

#Comparación de variables (columnas)
compare_df_cols(train_personas,test_personas)

#Comparación detallada:
summary(comparedf(train_personas,test_personas))



#1.3. Definición base train----

#Se define la base de datos train_h
train_h <- train_hogares

#Resumen de variables
summary(train_personas$P6800)#Horas trabajadas NAs 294901 
summary(train_personas$Pet)#Población en edad de trabajar- NAs 95438
summary(train_personas$P6210)#Nivel educativo - NAs 22685
summary(train_personas$P6020)#Sexo 1 hombre 2 mujer 
summary(train_personas$P6050)#Parentezco con el jefe de hogar
summary(train_personas$P6040)#Edad

train_personas_colaps <- train_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    horas_trabajadas=mean(P6800,na.rm = TRUE), # Se crea la var horas trabajadas
    analfabeta_h = if_else(any(Pet==1 && P6210==1), 1, 0), # Se crea la var analfabeta en el hogar
    mujer_jf_h = if_else(any(P6020==2 && P6050==1), 1, 0),# Se crea la var mujer jefe de hogar
    jf_10_18_h = if_else(any(P6050==1 && P6040>=10 && P6040<=18), 1, 0),#Se crea la var jefe de hogar entre 10 y 18 años
    jf_19_28_h = if_else(any(P6050==1 && P6040>=19 && P6040<=28), 1, 0), #Se crea la var jefe de hogar entre 19 y 28 años
    jf_29_59_h = if_else(any(P6050==1 && P6040>=29 && P6040<=59), 1, 0), #Se crea la var jefe de hogar entre 29 y 59 años 
    jf_60_h = if_else(any(P6050==1 && P6040<=60), 1, 0) ) #Se crea la var jefe de hogar mayores de 60 años


#Nota, para revisar: ¿inner join vs left join?
train_h <- 
  inner_join(train_h, train_personas_colaps,
             by = c("id","Clase","Dominio"))

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(train_h,"./stores/train_h.rds")

#1.4. Definición base test ----

#Se define la base de datos test_h
test_h <- test_hogares

#Resumen de variables
summary(test_personas$P6800)#Horas trabajadas NAs 119837 
summary(test_personas$Pet)#Población en edad de trabajar- NAs 38829
summary(test_personas$P6210)#Nivel educativo - NAs 9242
summary(test_personas$P6020)#Sexo 1 hombre 2 mujer 
summary(test_personas$P6050)#Parentezco con el jefe de hogar
summary(test_personas$P6040)#Edad

test_personas_colaps <- test_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    horas_trabajadas_t = mean(P6800,na.rm = TRUE), # Se crea la var horas trabajadas
    analfabeta_h_t = if_else(any(Pet==1 && P6210==1), 1, 0), # Se crea la var analfabeta en el hogar
    mujer_jf_h_t = if_else(any(P6020==2 && P6050==1), 1, 0),# Se crea la var mujer jefe de hogar
    jf_10_18_h_t = if_else(any(P6050==1 && P6040>=10 && P6040<=18), 1, 0),#Se crea la var jefe de hogar entre 10 y 18 años
    jf_19_28_h_t = if_else(any(P6050==1 && P6040>=19 && P6040<=28), 1, 0), #Se crea la var jefe de hogar entre 19 y 28 años
    jf_29_59_h_t= if_else(any(P6050==1 && P6040>=29 && P6040<=59), 1, 0), #Se crea la var jefe de hogar entre 29 y 59 años 
    jf_60_h_t = if_else(any(P6050==1 && P6040<=60), 1, 0) ) #Se crea la var jefe de hogar mayores de 60 años

#Nota, para revisar: ¿inner join vs left join?
test_h <- 
  inner_join(test_h, test_personas_colaps,
             by = c("id","Clase","Dominio"))

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(test_h,"./stores/test_h.rds")

#1.5. Identificar NAs base train_h ---- 

cantidad_na <- sapply(train_h, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(train_h)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 11.93% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
porcentaje_na <- porcentaje_na[!filtro,]
# 
orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)



# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

require(tidyverse)
### OJO SE BORRAN LOS NA PARA PROBAR
filtro <- porcentaje_na$cantidad_na > 0.05
variables_eliminar <- porcentaje_na$variable[filtro]
k0 <- ncol(train_h)
db <- train_h %>%
  select(variables_eliminar)
k1 <- ncol(train_h)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))
#revisar porque no están borrando..... 

# Prueba borrado manual--- ajustar o borrar
vars_drop <- c("P5100", "horas_trabajadas", "P5140", "P5130", "analfabeta_h")
train_h_v3 <- train_h[,!(names(train_h) %in% vars_drop)]
k0 <- ncol(train_h)
k1 <- ncol(train_h_v3)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

#1.6. Identificar NAs base test_h ---- 

cantidad_na <- sapply(train_h, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(test_h)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 29.74% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
porcentaje_na <- porcentaje_na[!filtro,]
# 
orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)

# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

#1.7. Tablas descriptivas ---- 

#Se usa la librería "CreateTableOne" para crear una tabla con todas las variables

#Tabla_descr <- CreateTableOne(data = train_h)
#Tabla_descr
#print(Tabla_descr,showAllLevels = TRUE)
#summary(Tabla_descr)
#Tabla_descr_csv <- print(Tabla_descr, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
#capture.output(Tabla_descr, file="Tabla_descr.doc")

## Save to a CSV file
#setwd("~/GitHub/MECA_BD_PS2")
#write.csv(Tabla_descr_csv, file = "./views/tabla_descr.csv")


#1.8. Gráficas para el análisis de datos---- 

#prueba 1 de gráfica
ggplot(train_h, aes(x=horas_trabajadas, y=Ingtotug,color=mujer_jf_h)) + 
  geom_point(aes(color=factor(mujer_jf_h))) +
  labs(x='horas trabajadas', y='Ingreso total', title='horas trabajdas vs. ingreso total')

#prueba 2 de gráfica
Conf2x2 = matrix(c(2:2), nrow=4, byrow=FALSE)
layout(Conf2x2)
hist(train_h$Ingtotug,main = "Histograma Ingreso total", xlab = "Ingreso total", col = "skyblue4")
hist(train_h$horas_trabajadas,main = "Histograma Horas Trabajadas", xlab = "Horas Trabajadas", col = "skyblue4")
boxplot(train_h$Ingtotug,main = "Boxplot Ingreso total", xlab = "Ingreso total", col = "skyblue4")
boxplot(train_h$horas_trabajadas,main = "Boxplot Horas Trabajadas", xlab = "Horas Trabajadas", col = "skyblue4")

#prueba 3 de gráfica


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. MODELOS DE CLASIFICACIÓN DE POBREZA----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Temporal: lectura BD

rm(list=ls())

setwd("~/GitHub/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")

train_h$Pobre <- factor(train_h$Pobre,levels=c("0","1"),labels=c("No_pobre","Pobre"))
# train_h$P5000 <- factor(train_h$P5000)
# train_h$P5090 <- factor(train_h$P5090)


#TEMPORAL: ELIMINAR 98 y 16
nrow(train_h)
train_h <- train_h[!(train_h$P5000=="98" | train_h$P5000=="16"),]
nrow(train_h)


#Temporal, eliminar todos los NA
#train_h <- train_h[complete.cases(train_h), ]

predict <- stats::predict

##2.1. Partición de la base de datos en tres----

#La base de datos Train se divide en tres particiones:
# Tr_train: Entrenar el modelo
# Tr_eval: Evaluar, ajustar y refinar el modelo
# Tr_test: Probar el modelo

#Balance inicial
prop.table(table(train_h$Pobre))

#Generamos las particiones
set.seed(100)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1)

other <- train_h[-split1,]
Tr_train <- train_h[split1,]

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

Tr_eval <- other[ split2,]
Tr_test <- other[-split2,]


## Balance final
prop.table(table(train_h$Pobre))
prop.table(table(Tr_train$Pobre))
prop.table(table(Tr_eval$Pobre))
prop.table(table(Tr_test$Pobre))


## 2.2. Modelos con diferentes variables: 

# modelo1
# modelo2
# modelo3
# modelo4
# modelo5

colnames(train_h)

## 2.2. Modelo sencillo Logit ----

#Se guarda la fórmula del modelo
form_logit_1 <- as.formula("Pobre ~ Npersug + factor(mujer_jf_h)")

form_logit_1 <- as.formula("Pobre ~ Npersug + factor(P5090)")

form_logit_1 <- as.formula("Pobre ~ Npersug + P5090 + P5000")

form_logit_1 <- as.formula("Pobre ~ Npersug + P5090 + P5000 + edad_p1 + 
                           mujer_jf_h + educ_p1")


#Se estima el modelo Logit
mod_logit_1 <-  glm(form_logit_1,
                    data= Tr_train,
                    family=binomial(link="logit"))

summary(mod_logit_1 ,type="text")

  #TEMPORAL:
Tr_test <- Tr_test[!(Tr_test$P5000=="98" | Tr_test$P5000=="16"),]


## Predicción Pobre sobre la base de Test
Tr_test$predict_logit <- predict(mod_logit_1, Tr_test, type="response")

## Se hace un gráfico de cajas y bigotes para explorar el punto de corte
ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

#Se agrega el resultado de la predicción según la probabilidad de Logit
Tr_test <- Tr_test %>% 
  mutate(p_logit=ifelse(predict_logit<0.5,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))


#Métricas de resultados:

## Matriz de confusión
confusionMatrix(data=Tr_test$p_logit, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")

## ROC
pred <- prediction(Tr_test$predict_logit, Tr_test$Pobre)
roc_ROCR <- performance(pred,"tpr","fpr")

plot(roc_ROCR, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

## AUC
auc_roc = performance(pred, measure = "auc")
auc_roc@y.values[[1]]


## 2.3. Rebalanceo de clases, remuestreo ----

#### Upsampling ----

set.seed(100)
upSampledTrain <- upSample(x = Tr_train,
                           y = Tr_train$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")

dim(Tr_train)
dim(upSampledTrain)
table(upSampledTrain$Pobre)

prop.table(table(Tr_train$Pobre))
prop.table(table(upSampledTrain$Pobre))


#### Downsampling ----

set.seed(100)
downSampledTrain <- downSample(x = Tr_train,
                               y = Tr_train$Pobre,
                               ## keep the class variable name the same:
                               yname = "Pobre")

dim(Tr_train)
dim(downSampledTrain)
table(downSampledTrain$Pobre)

prop.table(table(Tr_train$Pobre))
prop.table(table(upSampledTrain$Pobre))
prop.table(table(downSampledTrain$Pobre))


#### SMOTE ----


#Automatizar esto con tratamiento de cadenas de caracteres
predictors <-c ("Npersug","P5090","P5000")
head(Tr_train[predictors])


#Vuelvo dummies los factores
trainX <- data.frame(model.matrix(form_logit_1,data=Tr_train))[-1]

smote_output = SMOTE(X = trainX,
                     target = Tr_train$Pobre)


smote_output <-  SMOTE(X = Tr_train[predictors],
                       target = Tr_train$Pobre)

smotedTrain <-  smote_output$data

dim(Tr_train)
dim(upSampledTrain)
dim(downSampledTrain)
dim(smotedTrain)

table(Tr_train$Pobre)
table(smotedTrain$class)

prop.table(table(Tr_train$Pobre))
prop.table(table(upSampledTrain$Pobre))
prop.table(table(downSampledTrain$Pobre))
prop.table(table(smotedTrain$class))


#Ahora se pueden usar los TRAIN rebalanceados en los diferentes modelos
#Con SMOTE cambia la estructura de la base, ojo.




##2.4. Model Tunning con Caret ----

#Definición del control (a usarse en los demás modelos)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

## Entrenar el modelo
caret_logit  <-  train(form_logit_1,
                    data=Tr_train,
                    method="glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = c("center", "scale"))
caret_logit

## predict
Tr_test$p_caret <- predict(caret_logit , Tr_test , type="prob")[2]

## ROC
pred_cv <- prediction(Tr_test$p_caret , Tr_test$Pobre)

roc_ROCR_cv <- performance(pred_cv,"tpr","fpr")

plot(roc_ROCR_cv, main = "ROC curve", add=TRUE, colorize = FALSE, col="blue")

auc_roc = performance(pred_cv, measure = "auc")
auc_roc@y.values[[1]]


###Modelos Logit Lasso ----

install.packages("glmnet")
require(glmnet)

#Lasso
lambda_grid <- 10^seq(-4, 0.01, length = 200) #en la practica se suele usar una grilla de 200 o 300
lambda_grid


#Ajustado para sensibilidad:
mylogit_lasso_sens <- train(
  form_logit_1,
  data = Tr_train,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


#Ajustado para ROC:
mylogit_lasso_roc <- train(
  form_logit_1,
  data = Tr_train,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

#Ajustado para accuracy:
mylogit_lasso_acc <- train(
  form_logit_1,
  data = Tr_train,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Accuracy",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

#Comparación de los Logit_Lasso:

Tr_test$lasso_sens <- predict(mylogit_lasso_sens,
                              newdata = Tr_test,
                              type = "prob")[,2] #REVISAR

Tr_test$lasso_roc <- predict(mylogit_lasso_roc,
                              newdata = Tr_test,
                              type = "prob")[,2] #REVISAR

Tr_test$lasso_acc <- predict(mylogit_lasso_acc,
                             newdata = Tr_test,
                             type = "prob")[,2] #REVISAR


Tr_test$lasso_sens_clas <- factor(ifelse(Tr_test$lasso_sens<0.5,"No_pobre","Pobre"))
Tr_test$lasso_roc_clas <- factor(ifelse(Tr_test$lasso_roc<0.5,"No_pobre","Pobre"))
Tr_test$lasso_acc_clas <- factor(ifelse(Tr_test$lasso_acc<0.5,"No_pobre","Pobre"))


## Matriz de confusión
confusionMatrix(data=Tr_test$lasso_sens_clas, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")

confusionMatrix(data=Tr_test$lasso_roc_clas, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")

confusionMatrix(data=Tr_test$lasso_acc_clas, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")

# ROC
pred_lasso_sens <- prediction(Tr_test$lasso_sens, Tr_test$Pobre)
pred_lasso_roc <- prediction(Tr_test$lasso_roc, Tr_test$Pobre)
pred_lasso_acc <- prediction(Tr_test$lasso_acc, Tr_test$Pobre)

roc_ROCR_lasso_sens <- performance(pred_lasso_sens,"tpr","fpr")
roc_ROCR_lasso_roc <- performance(pred_lasso_roc,"tpr","fpr")
roc_ROCR_lasso_acc <- performance(pred_lasso_acc,"tpr","fpr")

plot(roc_ROCR_lasso_sens, main = "ROC curve", add=FALSE, colorize = F,col="red")
plot(roc_ROCR_lasso_roc, main = "ROC curve", add=TRUE, colorize = F,col="blue")
plot(roc_ROCR_lasso_acc, main = "ROC curve", add=TRUE, colorize = F,col="green")


# AUC
auc_roc_lasso_sens  <-  performance(pred_lasso_sens, measure = "auc")
auc_roc_lasso_roc <-  performance(pred_lasso_roc, measure = "auc")
auc_roc_lasso_acc <-  performance(pred_lasso_acc, measure = "auc")

auc_roc_lasso_sens@y.values[[1]]
auc_roc_lasso_roc@y.values[[1]]
auc_roc_lasso_acc@y.values[[1]]


#Ajustado para accuracy con UPSAMPLE:
mylogit_lasso_acc_ups <- train(
  form_logit_1,
  data = upSampledTrain,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Accuracy",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)



###Puntos de cortes alternativos ----

evalResults <- data.frame(Pobre = Tr_eval$Pobre)


evalResults$Roc <- predict(caret_logit, newdata = Tr_eval,
                           type = "prob")[,2] ##OJO!!! ¿1 o 2?


?roc

rfROC <- roc(evalResults$Pobre, evalResults$Roc, levels = rev(levels(evalResults$Pobre)))
rfROC

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh


evalResults <- evalResults %>% 
  mutate(hat_def_05=ifelse(evalResults$Roc>0.5,"Pobre","No_pobre"),
         hat_def_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Pobre","No_pobre"))

with(evalResults,table(Pobre,hat_def_05))
with(evalResults,table(Pobre,hat_def_rfThresh))


###Modelo LDA ----

library("MASS")

form_LDA <- as.formula("Pobre ~ Npersug + factor(P5090)")

mylda <- lda(form_LDA, data = Tr_train)
p_hat_mylda <- predict(mylda, Tr_test, type="response")
pred_mylda <- prediction(p_hat_mylda$posterior[,2], Tr_test$Pobre)

roc_mylda <- performance(pred_mylda,"tpr","fpr")


#Para agregar varias ROC:

plot(roc_ROCR, main = "ROC curve", colorize = FALSE, col="red")
plot(roc_mylda,add=TRUE, colorize = FALSE, col="blue")
abline(a = 0, b = 1)




##2.5.Otros modelos ----

# Sugerencia: empezar con el más complejo: 
  # Meterle todas las variables en un Boosting
  # Usar "Variables importance"
  # Identificar las que mejor funcionan

# Logit simple
# Logit lasso
# Otro cut off
# Balancear la muestra:
  # upsample
  # down
  # combinación (SMOTE)
# Árboles
# Random forest
# Boosting trees
# ADboost
# XGBoost
# Todas las combinaciones posibles
  #XGBoost + rebalanceo +  cutoff 


###Preparación del PC, cálculos en paralelo ----

n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

# Vamos a usar n_cores - 2 procesadores para esto
cl <- makePSOCKcluster(n_cores) 
registerDoParallel(cl)

##Ejecutar...

# Liberamos nuestros procesadores
stopCluster(cl)



###Modelo XGBoost ----

install.packages("xgboost")
require("xgboost")

form_xgboost <- as.formula("Pobre ~ Npersug + P5090 + P5000 + factor(mujer_jf_h)")

grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

xgboost <- train(
  form_xgboost,
  data = Tr_train,
  method = "xgbTree",
  trControl = control,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)

pred_xgb <- predict(xgboost,Tr_test)
confusionMatrix(Tr_test$Pobre,pred_xgb,positive="Pobre")

#Pendiente: Gráfica ROC de XGBoost:
# https://stackoverflow.com/questions/46736934/plotting-the-auc-from-an-xgboost-model-in-r



###Modelo árbol básico (CART) ----

form_tree <- as.formula("Pobre ~ Npersug + P5090 + factor(mujer_jf_h)")

#cp_alpha<-seq(from = 0, to = 0.1, length = 10)

tree <- train(
  form_tree,
  data = Tr_train,
  method = "rpart",
  trControl = control,
  parms=list(split='Gini'),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree
rpart.plot::prp(tree$finalModel)
pred_tree <- predict(tree,Tr_test)
c_matr_tree <- confusionMatrix(Tr_test$Pobre,pred_tree,positive="Pobre")
c_matr_tree



# Árbol con balance de clases, upsample:

tree_up <- train(
  form_tree,
  data = upSampledTrain,
  method = "rpart",
  trControl = control,
  parms=list(split='Gini'),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree_up
rpart.plot::prp(tree_up$finalModel)
pred_tree_up <- predict(tree_up,Tr_test)
confusionMatrix(Tr_test$Pobre,pred_tree_up,positive="Pobre")


# Arbol con downsample:

tree_down <- train(
  form_tree,
  data = downSampledTrain,
  method = "rpart",
  trControl = control,
  parms=list(split='Gini'),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree_down
rpart.plot::prp(tree_down$finalModel)
pred_tree_down <- predict(tree_down,Tr_test)
confusionMatrix(Tr_test$Pobre,pred_tree_down,positive="Pobre")




### Modelo de árbol sofisticado (clase de Lucas) ----









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#3.1. ----



#3.2. ----



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

