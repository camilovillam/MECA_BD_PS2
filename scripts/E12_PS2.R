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



#1.3. Definición base train----


#Se crea un subconjunto con las variables de interés
#Se hace un primer filtro dejando en la base train las variables que están en el test de hogares
train_h0 <- subset(train_hogares,
                       select=c(id,
                                Clase,
                                Dominio,
                                factor(P5000),#Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar? 
                                factor(P5010),#¿En cuántos de esos cuartos duermen las personas de este hogar? 
                                factor(P5090),#vivienda propia
                                P5100,
                                P5130,
                                P5140,
                                Nper,#Número de personas en el hogar
                                Npersug,#Número de personas en la unidad de gasto
                                Li,#Pobreza Extrema (Línea de indigencia)
                                Lp,#línea de Pobreza
                                Fex_c,
                                factor(Depto),#Departamento
                                factor(Fex_dpto)))#Factor de expasión departamental



#Resumen de variables
summary(train_personas$P6800)#Horas trabajadas NAs 294901 
summary(train_personas$Pet)#Población en edad de trabajar- NAs 95438
summary(train_personas$P6210)#Nivel educativo - NAs 22685
summary(train_personas$P6020)#Sexo 1 hombre 2 mujer 
summary(train_personas$P6050)#Parentezco con el jefe de hogar
summary(train_personas$P6040)#Edad
summary(train_personas$Oficio)#oficio


edad_pivot <- train_personas %>%  pivot_wider (names_from = P6050,#Parentezco con el jefe de hogar 
                                              values_from = P6040,#edad 
                                             names_prefix = "edad",
                                              values_fill = 0)

edad_pivot <- subset(edad_pivot,
       select = c(id,
                  Clase,
                  Dominio,
                  edad1,
                  edad2,
                  edad3,
                  edad4,
                  edad5,
                  edad6,
                  edad7,
                  edad8,
                  edad9)) 

horas_trabajadas_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                                        values_from = P6800, 
                                                       names_prefix = "ht",
                                                        values_fill = 0)


horas_trabajadas_pivot <- subset(horas_trabajadas_pivot,
        select = c(id,
                  Clase,
                  Dominio,
                  ht1,
                  ht2,
                  ht3,
                  ht4,
                  ht5,
                  ht6,
                  ht7,
                  ht8,
                  ht9))

oficio_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                              values_from = Oficio, 
                                             names_prefix = "of",
                                              values_fill = 0)

oficio_pivot <- subset(oficio_pivot,
       select = c(id,
                  Clase,
                  Dominio,
                  of1,
                  of2,
                  of3,
                  of4,
                  of5,
                  of6,
                  of7,
                  of8,
                  of9))

educ_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                                  values_from = P6210, 
                                                  names_prefix = "educ",
                                                  values_fill = 0)


educ_pivot <- subset(educ_pivot,
       select = c(id,
                  Clase,
                  Dominio,
                  educ1,
                  educ2,
                  educ3,
                  educ4,
                  educ5,
                  educ6,
                  educ7,
                  educ8,
                  educ9))

train_personas_colaps <- edad_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    edad_p1 = sum (edad1,na.rm = TRUE), 
    edad_p2 = sum (edad1,na.rm = TRUE), 
    edad_p3 = sum (edad1,na.rm = TRUE), 
    edad_p4 = sum (edad1,na.rm = TRUE), 
    edad_p5 = sum (edad1,na.rm = TRUE), 
    edad_p6 = sum (edad1,na.rm = TRUE), 
    edad_p7 = sum (edad1,na.rm = TRUE), 
    edad_p8 = sum (edad1,na.rm = TRUE),
    edad_p9 = sum (edad1,na.rm = TRUE))

train_personas_colaps <- horas_trabajadas_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    ht_p1 = sum (ht1,na.rm = TRUE), 
    ht_p2 = sum (ht2,na.rm = TRUE), 
    ht_p3 = sum (ht3,na.rm = TRUE), 
    ht_p4 = sum (ht4,na.rm = TRUE), 
    ht_p5 = sum (ht5,na.rm = TRUE), 
    ht_p6 = sum (ht6,na.rm = TRUE), 
    ht_p7 = sum (ht7,na.rm = TRUE), 
    ht_p8 = sum (ht8,na.rm = TRUE),
    ht_p9 = sum (ht9,na.rm = TRUE))

train_personas_colaps <- educ_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    educ_p1 = sum (educ1,na.rm = TRUE), 
    educ_p2 = sum (educ2,na.rm = TRUE), 
    educ_p3 = sum (educ3,na.rm = TRUE), 
    educ_p4 = sum (educ4,na.rm = TRUE), 
    educ_p5 = sum (educ5,na.rm = TRUE), 
    educ_p6 = sum (educ6,na.rm = TRUE), 
    educ_p7 = sum (educ7,na.rm = TRUE), 
    educ_p8 = sum (educ8,na.rm = TRUE),
    educ_p9 = sum (educ9,na.rm = TRUE))



#Nota, para revisar: ¿inner join vs left join?
train_h0 <- 
  inner_join(train_h0, train_personas_colaps,
             by = c("id","Clase","Dominio"))



    
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





#Se define la base de datos train_h
train_h <- train_hogares



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


#3.1. ----



#3.2. ----



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

