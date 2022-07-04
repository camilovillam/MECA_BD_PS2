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
       tidyverse, 
       skimr, 
       caret,
       rvest,
       stargazer)

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



#1.3. Definición de una única base de datos train para armar el modelo del PS2 ----

#Se define la base de datos train_h
train_h <- train_hogares

#Resumen info de la variable P7045: Horas trabajadas en la semana 
summary(train_personas$P7045)#tiene 531230 NAs

#Se crea la variable promedio horas trabajadas
horas_trabajadas <- train_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(horas_trabajadas=mean(P7045,na.rm = TRUE))

#Se agrega la variable horas trabajadas a la base train_h
train_h <- 
  inner_join(train_h, horas_trabajadas,
             by = c("id","Clase","Dominio"))

summarize(train_h, horas_trabajadas)#revisar en qué momento se debe limpiar la base de NAs

#se crea la variable si en el hogar hay al menos una persona en edad de trabajar con ningun grado escolar aprobado
#grado escolar : variable P6210 a. Ninguno
#población en edad de trabajar: variable pet 1: sí 0: no

#primero resumen de la variable
summary(train_personas$P6210)#tiene 22685 NAs

#Se crea la variable analfabeta_h que es un aproxy de al menos un analfabeta en el hogar en edad de trabajar
analfabeta_h <- train_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(analfabeta_h=if_else(any(Pet==1 && P6210==1), 1, 0))

#Se agrega la variable analfabeta_h a la base train_h
train_h <- 
  inner_join(train_h, analfabeta_h,
             by = c("id","Clase","Dominio"))


#Alternativa para 1.3:
# Con un solo group_by y luego con un solo join (debería ser lo mismo, solo por
# "economizar" comandos y variables intermedias)

train_personas_colaps <- train_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    horas_trabajadas=mean(P7045,na.rm = TRUE),
    analfabeta_h=if_else(any(Pet==1 && P6210==1), 1, 0))

#Nota, para revisar: ¿inner join vs left join?
train_h_v2 <- 
  inner_join(train_h, train_personas_colaps,
             by = c("id","Clase","Dominio"))


#1.4. Definición de una única base de datos test para probar el modelo del PS2 ----

#Se define la base de datos test_h
test_h <- test_hogares

#Resumen info de la variable P7045: Horas trabajadas en la semana 
summary(test_personas$P7045)#tiene 214275 NAs

#Se crea la variable promedio horas trabajadas
horas_trabajadas_t <- test_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(horas_trabajadas_t=mean(P7045,na.rm = TRUE))

#Se agrega la variable horas trabajadas a la base train_h
test_h <- 
  inner_join(test_h, horas_trabajadas_t,
             by = c("id","Clase","Dominio"))

summarize(test_h, horas_trabajadas_t)#revisar en qué momento se debe limpiar la base de NAs

#se crea la variable si en el hogar hay al menos una persona en edad de trabajar con ningun grado escolar aprobado
#grado escolar : variable P6210 a. Ninguno
#población en edad de trabajar: variable pet 1: sí 0: no

#primero resumen de la variable
summary(test_personas$P6210)#tiene 9242 NAs

#Se crea la variable analfabeta_h_t que es un aproxy de al menos un analfabeta en el hogar en edad de trabajar
analfabeta_h_t <- test_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(analfabeta_h_t=if_else(any(Pet==1 && P6210==1), 1, 0))

#Se agrega la variable analfabeta_h_t a la base train_h
test_h <- 
  inner_join(test_h, analfabeta_h_t,
             by = c("id","Clase","Dominio"))


#Alternativa para 1.4:
# Con un solo group_by y luego con un solo join

test_personas_colaps <- test_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    horas_trabajadas_t=mean(P7045,na.rm = TRUE),
    analfabeta_h_t=if_else(any(Pet==1 && P6210==1), 1, 0))


#Nota, para revisar: ¿inner join vs left join?
test_h_v2 <- 
  inner_join(test_h, test_personas_colaps,
             by = c("id","Clase","Dominio"))



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

# Prueba borrado manual
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

#1.6. Tablas descriptivas ---- 

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



#1.6. Gráficas para el análisis de datos---- 

summary(test_h$Lp)
ggplot(test_h, aes(x = horas_trabajadas_t, y = Lp)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  theme_classic() +
  scale_y_continuous(labels = scales::dollar) +
  # scale_y_continuous(labels = scales::dollar, trans = 'log10') +
  labs(x = "Horas trabajadas", y = "Línea pobreza")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. MODELOS DE CLASIFICACIÓN DE POBREZA----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#2.2. ----



#2.2. ----


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#3.1. ----



#3.2. ----



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

