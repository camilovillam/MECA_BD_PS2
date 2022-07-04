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
submission_template <-read.csv("./stores/data/submission_template.csv")
test_hogares <-read.csv("./stores/data/test_hogares.csv")
test_personas <-read.csv("./stores/data/test_personas.csv")
train_hogares <-read.csv("./stores/data/train_hogares.csv")
train_personas <-read.csv("./stores/data/train_personas.csv.gz")


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

#Se define la base de datos definitiva train_h
train_h <- train_hogares

#Resumen info de la variable P7045: Horas trabajadas en la semana 
attach(train_personas)
summary(P7045)#tiene 531230 NAs


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
attach(train_personas)
summary(P6210)

#Se crea la variable analfabeta_h que es un aproxy de al menos un analfabeta en el hogar en edad de trabajar
analfabeta_h <- train_personas %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(analfabeta_h=if_else(any(Pet==1 && P6210==1), 1, 0))

#Se agrega la variable analfabeta_h a la base train_h
train_h <- 
  inner_join(train_h, analfabeta_h,
             by = c("id","Clase","Dominio"))

#1.4. Definición de una única base de datos test para probar el modelo del PS2 ----



#1.5. Tablas descriptivas ---- 


#Se usa la librería "CreateTableOne" para crear una tabla con todas las variables

#Tabla_descr <- CreateTableOne(data = train_personas)
#Tabla_descr
#print(Tabla_descr,showAllLevels = TRUE)
#summary(Tabla_descr)
#Tabla_descr_csv <- print(Tabla_descr, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
#capture.output(Tabla_descr, file="Tabla_descr.doc")

## Save to a CSV file
#setwd("~/GitHub/MECA_BD_PS2")
#write.csv(Tabla_descr_csv, file = "./views/tabla_descr.csv")



#1.4. Gráficas para el análisis de datos---- 


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

