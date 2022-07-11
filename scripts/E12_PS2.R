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
# 3. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#3.1. Carga de la base de datos----

#se carga la base ajustada
setwd("~/GitHub/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")

#se crea la variable logaritmo de ingresos
train_h$ln_ing <- log(train_h$Ingtotug)
train_h <- train_h %>% mutate (edadjf_cua= edad_p1*edad_p1)

#NOTA: Se eliminan las filas  que tienen Inf de la variable ln_ing (revisar cuando se termine el P2)
train_h <- train_h[is.finite(train_h$ln_ing), ]

##3.2. Partición de la base de datos en tres----

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

#3.3. Prueba modelo de regresión

require("stargazer")

# modelo1 <- as.formula (Ingtotug ~ Dominio+
#                          mujer_jf_h+
#                          edad_p1+
#                          edad_p2+
#                          edad_p3+
#                          edad_p4+
#                          edad_p5+
#                          edadjf_cua+
#                          ht_p1+
#                          ht_p2+
#                          ht_p3+
#                          of_p1+
#                          of_p3+
#                          hijos+
#                          pj_jf_sintrabajo+
#                          P5000+
#                          P5010+
#                          P5090+
#                          educ_p1+
#                          educ_p2+
#                          educ_p3+
#                          Npersug+
#                          jf_sub)

reg1<-lm(modelo1,Tr_train)
stargazer(reg1,type="text")

#3.4. Prueba estimación MSE

modelo_estimado <- train(modelo1,
                         data = Tr_train,
                         trControl=trainControl(method="cv",number=5),
                         method="lm")


modelo_predicho <- predict(modelo_estimado,newdata = Tr_test )


#Cálculo del MSE:
 MSE_modelo1 <- with (Tr_test,mean((Ingtotug - modelo_predicho^2)))
 
#Guardar resultado de logaritmo de ingreso en la base
# Tr_test$log_y <- modelo_predicho

#Pasar el logaitmo del ingreso a ingreso con exponencial en la misma base
 Tr_test$y <- modelo_predicho 
 #Tr_test$y <- exp(Tr_test$log_y)

 #Determinar si es pobre o no
 
 Tr_test$pobre_clas_ing <- factor(if_else( Tr_test$y < Tr_test$Lp, "Pobre", "No Pobre"))
 
 summary(Tr_test$pobre_clas_ing)
 
 confusionMatrix(data=Tr_test$pobre_clas_ing, 
                                 reference=Tr_test$Pobre , 
                                 mode="sens_spec" , positive="Pobre")
 
 
 ## ROC
 
 pred_m1 <- prediction(Tr_test$pobre_clas_ing, Tr_test$Pobre)
 
 roc_ROCRm1 <- performance(pred_m1,"tpr","fpr")
 
 plot(roc_ROCRm1, main = "ROC curve", colorize = FALSE, col="blue")
 
 abline(a = 0, b = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

