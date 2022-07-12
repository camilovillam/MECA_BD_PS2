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
       MASS,
       ROCR,
       pROC,
       rpart,
       rpart.plot,
       glmnet,
       xgboost)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. PREPARACIÓN DE LA BASE DE DATOS Y ESTADÍSTICAS DESCRIPTIVAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#1.1. Cargue de las bases de datos ---- 

setwd("~/GitHub/MECA_BD_PS2")
submission_template <-read.csv("./stores/20220703_data/submission_template.csv")
test_hogares <-readRDS("./stores/20220703_data/test_hogares.rds")
test_personas <-readRDS("./stores/20220703_data/test_personas.rds")
train_hogares <-readRDS("./stores/20220703_data/train_hogares.rds")
train_personas <-readRDS("./stores/20220703_data/train_personas.rds")


#1.2. Exploración incial de los datos ----

#Exploración de las bases de datos:
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


#2.2. ----



#2.2. ----


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##4.1. Carga de la base de datos----

#se carga la base ajustada
setwd("~/GitHub/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")

#se crean variables
#train_h$ln_ing <- log(train_h$Ingtotug)
train_h <- train_h %>% mutate (edadjf_cua= edad_p1*edad_p1)

#NOTA: Se eliminan las filas  que tienen Inf de la variable ln_ing (revisar cuando se termine el P2)
#train_h <- train_h[is.finite(train_h$ln_ing), ]

##4.2. Partición de la base de datos en tres----

#La base de datos Train se divide en tres particiones:
# Tr_train: Entrenar el modelo
# Tr_eval: Evaluar, ajustar y refinar el modelo
# Tr_test: Probar el modelo

#Balance inicial
prop.table(table(train_h$Pobre))

#Generamos las particiones
set.seed(100)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1) # Base de 103052

other <- train_h[-split1,]
Tr_train <- train_h[split1,]

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

Tr_eval <- other[ split2,]
Tr_test <- other[-split2,]


##4.3. Modelos de regresión ----

require("stargazer")

modelo1 <- as.formula (Ingtotug ~ Clase+Dominio+
                         mujer_jf_h+
                         edad_p1+
                         edad_p2+
                         edad_p3+
                         edad_p4+
                         edad_p5+
                         ht_p1+
                         ht_p2+
                         ht_p3+
                         of_p1+
                         of_p3+
                         hijos+
                         pj_jf_sintrabajo+
                         P5000+
                         P5010+
                         P5090+
                         educ_p1+
                         educ_p2+
                         educ_p3+
                         Npersug+
                         jf_sub+
                         jf_sintrabajo)

modelo2 <- as.formula (Ingtotug ~ Dominio+Npersug:P5010 + P5090 + P5000 + edad_p1 + edadjf_cua+ 
                         edad_p2 + edad_p3 + edad_p4 + edad_p5 + edad_p6 + edad_p7 + edad_p8 + edad_p9+
                         mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + ht_p2 + ht_p3 + ht_p4 +
                         ht_p5 + ht_p6 + ht_p7 + ht_p8+ ht_p9 + jf_sub + 
                         hijos + ht_p1 + pj_jf_sintrabajo + jf_sintrabajo)

modelo3 <- as.formula (Ingtotug ~ Npersug + P5090 + P5000 + edad_p1 +
                         mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + jf_sub + 
                         hijos + ht_p1 + pj_jf_sintrabajo +jf_sintrabajo+pj_jf_ofhogar)


reg1<-lm(modelo1,Tr_train)
reg2<-lm(modelo2,Tr_train)
reg3<-lm(modelo3,Tr_train)

stargazer(reg1,type="text")
stargazer(reg2,type="text")
stargazer(reg3,type="text")

#Entrenamiento de modelos CV K-Fold

modelo_estimado1 <- train(modelo1,
                         data = Tr_train,
                         trControl=trainControl(method="cv",number=10),
                         method="lm")

modelo_estimado2 <- train(modelo2,
                          data = Tr_train,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

modelo_estimado3 <- train(modelo3,
                          data = Tr_train,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

modelo_predicho1 <- predict(modelo_estimado1,newdata = Tr_test )
modelo_predicho2 <- predict(modelo_estimado2,newdata = Tr_test )
modelo_predicho3 <- predict(modelo_estimado3,newdata = Tr_test )



#Cálculo del MSE:
MSE_modelo1 <- with (Tr_test,mean((Ingtotug - modelo_predicho1)^2))
MSE_modelo2 <- with (Tr_test,mean((Ingtotug - modelo_predicho2)^2))
MSE_modelo3 <- with (Tr_test,mean((Ingtotug - modelo_predicho3)^2))

MSE_modelo1
MSE_modelo2
MSE_modelo3


#Guardar resultado de logaritmo de ingreso en la base
# Tr_test$log_y <- modelo_predicho

#Pasar el logaitmo del ingreso a ingreso con exponencial en la misma base

#Guardar los resultados en la base de Test
Tr_test$y1 <- modelo_predicho1
Tr_test$y2 <- modelo_predicho2
Tr_test$y3 <- modelo_predicho3

#Tr_test$y <- exp(Tr_test$log_y)

#Determinar si es pobre o no
 
Tr_test$pobre_clas_ing1 <- factor(if_else( Tr_test$y1 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_ing2 <- factor(if_else( Tr_test$y2 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_ing3 <- factor(if_else( Tr_test$y3 < Tr_test$Lp, "Pobre", "No Pobre"))
 
summary(Tr_test$pobre_clas_ing1)
summary(Tr_test$pobre_clas_ing2)
summary(Tr_test$pobre_clas_ing3)
 
cm1 <- confusionMatrix(data=Tr_test$pobre_clas_ing1, 
                                 reference=Tr_test$Pobre , 
                                 mode="sens_spec" , positive="Pobre")

cm2 <- confusionMatrix(data=Tr_test$pobre_clas_ing2, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")

cm3 <- confusionMatrix(data=Tr_test$pobre_clas_ing3, 
                reference=Tr_test$Pobre , 
                mode="sens_spec" , positive="Pobre")
 

##4.4. Lasso, Ridge, Elastic Net de los modelos ----

### Lasso ----

lambda <- 10^seq(-2, 3, length = 200)

lasso1 <- train(modelo1,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

lasso2 <- train(modelo2,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

lasso3 <- train(modelo3,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

lasso1
lasso2
lasso3

mod_pred_lass1 <- predict(lasso1,newdata = Tr_test )
mod_pred_lass2 <- predict(lasso2,newdata = Tr_test )
mod_pred_lass3 <- predict(lasso3,newdata = Tr_test )

Tr_test$y_lass1 <- mod_pred_lass1
Tr_test$y_lass2 <- mod_pred_lass2
Tr_test$y_lass3 <- mod_pred_lass3

Tr_test$pobre_clas_lass1 <- factor(if_else( Tr_test$y_lass1 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_lass2 <- factor(if_else( Tr_test$y_lass2 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_lass3 <- factor(if_else( Tr_test$y_lass3 < Tr_test$Lp, "Pobre", "No Pobre"))

summary(Tr_test$pobre_clas_lass1)
summary(Tr_test$pobre_clas_lass2)
summary(Tr_test$pobre_clas_lass3)

cm_lass1 <- confusionMatrix(data=Tr_test$pobre_clas_lass1, 
                       reference=Tr_test$Pobre , 
                       mode="sens_spec" , positive="Pobre")

cm_lass2 <- confusionMatrix(data=Tr_test$pobre_clas_lass2, 
                            reference=Tr_test$Pobre , 
                            mode="sens_spec" , positive="Pobre")

cm_lass3 <- confusionMatrix(data=Tr_test$pobre_clas_lass3, 
                            reference=Tr_test$Pobre , 
                            mode="sens_spec" , positive="Pobre")



###Ridge ----

ridge1 <- train(modelo1,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))

ridge2 <- train(modelo2,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))

ridge3 <- train(modelo3,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))


ridge1
ridge2
ridge3

mod_pred_ridge1 <- predict(ridge1,newdata = Tr_test )
mod_pred_ridge2 <- predict(ridge2,newdata = Tr_test )
mod_pred_ridge3 <- predict(ridge3,newdata = Tr_test )

Tr_test$y_ridge1 <- mod_pred_ridge1
Tr_test$y_ridge2 <- mod_pred_ridge2
Tr_test$y_ridge3 <- mod_pred_ridge3

Tr_test$pobre_clas_ridge1 <- factor(if_else( Tr_test$y_ridge1 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_ridge2 <- factor(if_else( Tr_test$y_ridge2 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_ridge3 <- factor(if_else( Tr_test$y_ridge3 < Tr_test$Lp, "Pobre", "No Pobre"))

summary(Tr_test$pobre_clas_ridge1)
summary(Tr_test$pobre_clas_ridge2)
summary(Tr_test$pobre_clas_ridge3)

cm_ridge1 <- confusionMatrix(data=Tr_test$pobre_clas_ridge1, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_ridge2 <- confusionMatrix(data=Tr_test$pobre_clas_ridge2, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_ridge3 <- confusionMatrix(data=Tr_test$pobre_clas_ridge3, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")



### Elastic Net ----

elnet1 <- train(modelo1,
            data = Tr_train,
            method = "glmnet",
            trControl = trainControl("cv", number = 10),
            preProcess = c("center", "scale"))

elnet2 <- train(modelo2,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                preProcess = c("center", "scale"))

elnet3 <- train(modelo3,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                preProcess = c("center", "scale"))

elnet1
elnet2
elnet3

mod_pred_elnet1 <- predict(elnet1,newdata = Tr_test )
mod_pred_elnet2 <- predict(elnet2,newdata = Tr_test )
mod_pred_elnet3 <- predict(elnet3,newdata = Tr_test )

Tr_test$y_elnet1 <- mod_pred_elnet1
Tr_test$y_elnet2 <- mod_pred_elnet2
Tr_test$y_elnet3 <- mod_pred_elnet3

Tr_test$pobre_clas_elnet1 <- factor(if_else( Tr_test$y_elnet1 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_elnet2 <- factor(if_else( Tr_test$y_elnet2 < Tr_test$Lp, "Pobre", "No Pobre"))
Tr_test$pobre_clas_elnet3 <- factor(if_else( Tr_test$y_elnet3 < Tr_test$Lp, "Pobre", "No Pobre"))

summary(Tr_test$pobre_clas_elnet1)
summary(Tr_test$pobre_clas_elnet2)
summary(Tr_test$pobre_clas_elnet3)

cm_elnet1 <- confusionMatrix(data=Tr_test$pobre_clas_elnet1, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_elnet2 <- confusionMatrix(data=Tr_test$pobre_clas_elnet2, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_elnet3 <- confusionMatrix(data=Tr_test$pobre_clas_elnet3, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")



### Comparación de los modelos ----

models <- list(lasso1,lasso2,lasso3,ridge1,ridge2,ridge3,elnet1,elnet2,elnet3)
models <- list(lm1=modelo_estimado1,lasso1=lasso1,ridge1=ridge1,elnet1=elnet1)
resamples(models) %>% summary(metric = "RMSE")


cm1
cm2
cm3
cm_lass1
cm_lass2
cm_lass3
cm_ridge1
cm_ridge2
cm_ridge3
cm_elnet1
cm_elnet2
cm_elnet3


##4.5. Exportación final ----

modelo_final_ing <- elnet2

setwd("~/GitHub/MECA_BD_PS2")
test_h <-readRDS("./stores/test_h_si.rds")

nrow(test_h)


## Predecir el modelo final:

#TEMPORAL!
test_h <- test_h[!(test_h$P5000=="43"),]
test_h$jf_sub <- factor(test_h$jf_sub,levels=c("0","1"),labels=c("jefe de hogar no subsidiado","jefe de hogar subsidiado"))

test_h$edadjf_cua <- test_h$edad_p1^2

test_h$pred_ing_final <- predict(modelo_final_ing,newdata = test_h)
test_h$Pobre_income <- if_else(test_h$pred_ing_final < test_h$Lp,1,0)


#submit  <-  test_h[,c("id","Pobre_classification")]

submit  <-  test_h[,c("id","Pobre_income")]
elnet1

## Guardar el .CSV
setwd("~/GitHub/MECA_BD_PS2/document")
export(submit,"./predictions_garcia_molano_villa_c12_r23.csv")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

