#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Problem Set 2
#Equipo 12

#Jorge E. García
#Ingrid Lorena Molano
#Camilo Villa Moreno

#Julio 12, 2022

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
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
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




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. PREPARACIÓN DE LA BASE DE DATOS TRAIN Y ESTADÍSTICAS DESCRIPTIVAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#2.1. Primera definición base train----

#Se hace un primer filtro dejando en la base train las variables que están en el test de hogares
train_h0 <- select(train_hogares,id,
                                Clase,
                                Dominio,
                                P5000,#Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar? 
                                P5010,#¿En cuántos de esos cuartos duermen las personas de este hogar? 
                                P5090,#vivienda propia
                                P5100,
                                P5130,
                                P5140,
                                Nper,#Número de personas en el hogar
                                Npersug,#Número de personas en la unidad de gasto
                                Li,#Pobreza Extrema (Línea de indigencia)
                                Lp,#línea de Pobreza
                                Fex_c,
                                Depto,#Departamento
                                Fex_dpto)#Factor de expasión departamental



#Resumen de variables del train personas
summary(train_personas$P6800)#Horas trabajadas NAs 294901 
summary(train_personas$Pet)#Población en edad de trabajar- NAs 95438
summary(train_personas$P6210)#Nivel educativo - NAs 22685
summary(train_personas$P6020)#Sexo 1 hombre 2 mujer 
summary(train_personas$P6050)#Parentezco con el jefe de hogar
summary(train_personas$P6040)#Edad
summary(train_personas$Oficio)#oficio

#Se hace un pivot de variables del train personas

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

#Como de cada tipo de persona pueden haber varios en el hogar, 
#se agregan los casos por tipo de persona segun la función

train_personas_colaps_edad <- edad_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    edad_p1 = max (edad1,na.rm = TRUE), 
    edad_p2 = max (edad2,na.rm = TRUE), 
    edad_p3 = max (edad3,na.rm = TRUE), 
    edad_p4 = max (edad4,na.rm = TRUE), 
    edad_p5 = max (edad5,na.rm = TRUE), 
    edad_p6 = max (edad6,na.rm = TRUE), 
    edad_p7 = max (edad7,na.rm = TRUE), 
    edad_p8 = max (edad8,na.rm = TRUE),
    edad_p9 = max (edad9,na.rm = TRUE))

train_personas_colaps_ht <- horas_trabajadas_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    ht_p1 = mean (ht1,na.rm = TRUE), 
    ht_p2 = mean (ht2,na.rm = TRUE), 
    ht_p3 = mean (ht3,na.rm = TRUE), 
    ht_p4 = mean (ht4,na.rm = TRUE), 
    ht_p5 = mean (ht5,na.rm = TRUE), 
    ht_p6 = mean (ht6,na.rm = TRUE), 
    ht_p7 = mean (ht7,na.rm = TRUE), 
    ht_p8 = mean (ht8,na.rm = TRUE),
    ht_p9 = mean (ht9,na.rm = TRUE))

train_personas_colaps_oficio <- oficio_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    of_p1 = max (of1,na.rm = TRUE), 
    of_p2 = max (of2,na.rm = TRUE), 
    of_p3 = max (of3,na.rm = TRUE), 
    of_p4 = max (of4,na.rm = TRUE), 
    of_p5 = max (of5,na.rm = TRUE), 
    of_p6 = max (of6,na.rm = TRUE), 
    of_p7 = max (of7,na.rm = TRUE), 
    of_p8 = max (of8,na.rm = TRUE),
    of_p9 = max (of9,na.rm = TRUE))

train_personas_colaps_educ <- educ_pivot %>% 
  group_by(id,Clase,Dominio) %>%
  summarize(
    educ_p1 = max (educ1,na.rm = TRUE), 
    educ_p2 = max (educ2,na.rm = TRUE), 
    educ_p3 = max (educ3,na.rm = TRUE), 
    educ_p4 = max (educ4,na.rm = TRUE), 
    educ_p5 = max (educ5,na.rm = TRUE), 
    educ_p6 = max (educ6,na.rm = TRUE), 
    educ_p7 = max (educ7,na.rm = TRUE), 
    educ_p8 = max (educ8,na.rm = TRUE),
    educ_p9 = max (educ9,na.rm = TRUE))

#Nota, para revisar: ¿inner join vs left join?
train_h0 <- 
  inner_join(train_h0, train_personas_colaps_edad,
             by = c("id","Clase","Dominio"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_ht,
             by = c("id","Clase","Dominio"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_oficio,
             by = c("id","Clase","Dominio"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_educ,
             by = c("id","Clase","Dominio"))

#Creación de otras variables

train_personas_jf <- train_personas %>% mutate(
  mujer_jf_h = if_else(P6020==2 & P6050==1, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_10_18_h = if_else(P6050==1 & P6040>=10 & P6040<=18, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_19_28_h = if_else(P6050==1 & P6040>=19 & P6040<=28, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_29_59_h = if_else(P6050==1 & P6040>=29 & P6040<=59, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_60_h = if_else(P6050==1 & P6040>=60, 1, 0)
)

train_personas_jf <- train_personas_jf %>%
  group_by(id) %>%
  summarise(
    mujer_jf_h = max(mujer_jf_h),
    jf_10_18_h = max(jf_10_18_h),
    jf_19_28_h = max(jf_19_28_h),
    jf_29_59_h = max(jf_29_59_h),
    jf_60_h = max(jf_60_h),
  )


train_h0 <- 
  inner_join(train_h0, train_personas_jf,
             by = c("id"))


#2.2. Identificar NAs base train_h ---- 

train_h <- train_h0
cantidad_na <- sapply(train_h, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(train_h)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 3.06% de las entradas están vacías"

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

# Prueba borrado manual--- ajustar o borrar
vars_drop <- c("P5100", "P5140", "P5130")
train_h_si <- train_h[,!(names(train_h) %in% vars_drop)]
k0 <- ncol(train_h)
k1 <- ncol(train_h_si)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

#se crea train hogares borrando las filas de ht_p1 que están con NA

train_h_si <- train_h_si %>% filter(!is.na(ht_p1))#pasa de 164960 a 156568 obs

#2.3. Convertir en factor variables de base train ---- 

train_h_si$P5000 <- as.factor(train_h_si$P5000)
train_h_si$P5010 <- as.factor(train_h_si$P5010)
train_h_si$Depto <- as.factor(train_h_si$Depto)

train_h_si$of_p1 <- as.factor(train_h_si$of_p1)
train_h_si$of_p2 <- as.factor(train_h_si$of_p2)
train_h_si$of_p3 <- as.factor(train_h_si$of_p3)
train_h_si$of_p4 <- as.factor(train_h_si$of_p4)
train_h_si$of_p5 <- as.factor(train_h_si$of_p5)
train_h_si$of_p6 <- as.factor(train_h_si$of_p6)
train_h_si$of_p7 <- as.factor(train_h_si$of_p7)
train_h_si$of_p8 <- as.factor(train_h_si$of_p8)
train_h_si$of_p9 <- as.factor(train_h_si$of_p9)

train_h_si$educ_p1 <- as.factor(train_h_si$educ_p1)
train_h_si$educ_p2 <- as.factor(train_h_si$educ_p2)
train_h_si$educ_p3 <- as.factor(train_h_si$educ_p3)
train_h_si$educ_p4 <- as.factor(train_h_si$educ_p4)
train_h_si$educ_p5 <- as.factor(train_h_si$educ_p5)
train_h_si$educ_p6 <- as.factor(train_h_si$educ_p6)
train_h_si$educ_p7 <- as.factor(train_h_si$educ_p7)
train_h_si$educ_p8 <- as.factor(train_h_si$educ_p8)
train_h_si$educ_p9 <- as.factor(train_h_si$educ_p9)

train_h_si$mujer_jf_h <- as.factor(train_h_si$mujer_jf_h)
train_h_si$jf_10_18_h <- as.factor(train_h_si$jf_10_18_h)
train_h_si$jf_19_28_h <- as.factor(train_h_si$jf_19_28_h)
train_h_si$jf_29_59_h <- as.factor(train_h_si$jf_29_59_h)
train_h_si$jf_60_h <- as.factor(train_h_si$jf_60_h)

#2.4. Guardar la base train ----   

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(train_h_si,"./stores/train_h_si.rds")


#2.5. Tablas descriptivas ---- 

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


#2.6. Gráficas para el análisis de datos---- 

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PREPARACIÓN DE LA BASE DE DATOS TEST Y ESTADÍSTICAS DESCRIPTIVAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. MODELOS DE CLASIFICACIÓN DE POBREZA----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#4.1. ----



#4.2. ----


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#5.1. ----



#5.2. ----



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

