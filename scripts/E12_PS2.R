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
#Se dejan 2 variables de trian hogares que no están en tres hogares Pobre e Ingtotug
train_h0 <- select(train_hogares,id,
                                Clase,
                                Dominio,
                                Pobre,
                                Ingtotug,
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
summary(train_personas$P6090)#Es beneficiario de alguna entidad social en salud Nas 95438
summary(train_personas$P6100)#Regimenes de seguridad social en salud #126064
summary(train_personas$P6040)#Edad
summary(train_personas$Oficio)#oficio

#Se hace un pivot de variables del train personas

edad_pivot <- train_personas %>%  pivot_wider (names_from = P6050,#Parentezco con el jefe de hogar 
                                              values_from = P6040,#edad 
                                             names_prefix = "edad",
                                              values_fill = 0)

edad_pivot <- select(edad_pivot,
                  id,
                  edad1,
                  edad2,
                  edad3,
                  edad4,
                  edad5,
                  edad6,
                  edad7,
                  edad8,
                  edad9) 

horas_trabajadas_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                                        values_from = P6800, 
                                                       names_prefix = "ht",
                                                        values_fill = 0)


horas_trabajadas_pivot <- select (horas_trabajadas_pivot,
                  id,
                  ht1,
                  ht2,
                  ht3,
                  ht4,
                  ht5,
                  ht6,
                  ht7,
                  ht8,
                  ht9)

oficio_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                              values_from = Oficio, 
                                             names_prefix = "of",
                                              values_fill = 0)

oficio_pivot <- select (oficio_pivot,
                  id,
                  of1,
                  of2,
                  of3,
                  of4,
                  of5,
                  of6,
                  of7,
                  of8,
                  of9)

educ_pivot <- train_personas %>% pivot_wider(names_from = P6050, 
                                                  values_from = P6210, 
                                                  names_prefix = "educ",
                                                  values_fill = 0)


educ_pivot <- select (educ_pivot,
                  id,
                  educ1,
                  educ2,
                  educ3,
                  educ4,
                  educ5,
                  educ6,
                  educ7,
                  educ8,
                  educ9)

#Como de cada tipo de persona pueden haber varios en el hogar, 
#se agregan los casos por tipo de persona segun la función

train_personas_colaps_edad <- edad_pivot %>% 
  group_by(id) %>%
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
  group_by(id) %>%
  summarize(
    ht_p1 = max (ht1,na.rm = TRUE), 
    ht_p2 = max (ht2,na.rm = TRUE), 
    ht_p3 = max (ht3,na.rm = TRUE), 
    ht_p4 = max (ht4,na.rm = TRUE), 
    ht_p5 = max (ht5,na.rm = TRUE), 
    ht_p6 = max (ht6,na.rm = TRUE), 
    ht_p7 = max (ht7,na.rm = TRUE), 
    ht_p8 = max (ht8,na.rm = TRUE),
    ht_p9 = max (ht9,na.rm = TRUE))

train_personas_colaps_oficio <- oficio_pivot %>% 
  group_by(id) %>%
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
  group_by(id) %>%
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

#Join
train_h0 <- 
  inner_join(train_h0, train_personas_colaps_edad,
             by = c("id"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_ht,
             by = c("id"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_oficio,
             by = c("id"))

train_h0 <- 
  inner_join(train_h0, train_personas_colaps_educ,
             by = c("id"))

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

train_personas_jf <- train_personas_jf %>% mutate(
  jf_sub = if_else(P6050==1 & P6100==3, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_afiliado = if_else(P6050==1 & P6090==2 | P6090==3, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_sintrabajo = if_else(P6050==1 & P6240==2 | P6240==5, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  pj_jf_ofhogar = if_else(P6050==2 & P6240==4, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  pj_jf_sintrabajo = if_else(P6050==2 & P6240==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_nc_pension = if_else(P6050==1 & P6920==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_P7422 = if_else(P6050==1 & P7422==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  pj_jf_P7422 = if_else(P6050==2 & P7422==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_P7472 = if_else(P6050==1 & P7472==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  pj_jf_P7472 = if_else(P6050==2 & P7472==2, 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  es_hijo = if_else(P6050==3, 1, 0)
)

train_personas_jf <- train_personas_jf %>%
  group_by(id) %>%
  summarise(
    mujer_jf_h = max(mujer_jf_h),
    jf_10_18_h = max(jf_10_18_h),
    jf_19_28_h = max(jf_19_28_h),
    jf_29_59_h = max(jf_29_59_h),
    jf_60_h = max(jf_60_h),
    jf_sub = max(jf_sub),
    jf_afiliado = max(jf_afiliado),
    jf_sintrabajo = max(jf_sintrabajo),
    pj_jf_ofhogar = max(pj_jf_ofhogar),
    pj_jf_sintrabajo = max(pj_jf_sintrabajo),
    jf_nc_pension = max(jf_nc_pension),
    jf_P7422 = max(jf_P7422),
    pj_jf_P7422 = max(pj_jf_P7422),
    jf_P7472 = max(jf_P7472),
    pj_jf_P7472 = max(pj_jf_P7472),
    hijos = sum(es_hijo)
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
#En promedio el 8.06% de las entradas están vacías"

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
vars_drop <- c("P5100", 
               "jf_P7422",
               "jf_P7472",
               "P5140", 
               "pj_jf_P7422",
               "P5130",
               "jf_afiliado",
               "jf_sintrabajo",
               "pj_jf_P7472",
               "jf_nc_pension")
train_h_si <- train_h[,!(names(train_h) %in% vars_drop)]
k0 <- ncol(train_h)
k1 <- ncol(train_h_si)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

#se crea train hogares borrando las filas de ht_p1 que están con NA

train_h_si <- train_h_si %>% filter(!is.na(jf_sub))#pasa de 164960 a 155089 obs
train_h_si <- train_h_si [is.finite(train_h_si$ht_p1), ] #pasa de 155089 a 147231 obs
train_h_si <- train_h_si %>% filter(!is.na(pj_jf_ofhogar)) #pasa de 147231 a 147215 obs
train_h_si <- train_h_si %>% filter(!is.na(pj_jf_sintrabajo)) #pasa de 147215a 147215 obs

#2.3. Convertir en factor variables de base train y etiquetar---- 

#se llama la librería haven 
library(haven)

train_h_si$P5000 <- factor(train_h_si$P5000)
train_h_si$P5010 <- factor(train_h_si$P5010)
train_h_si$P5090 <- factor(train_h_si$P5090)
train_h_si$Depto <- factor(train_h_si$Depto)
train_h_si$Pobre <- factor(train_h_si$Pobre, labels = c("No Pobre", "Pobre" ))


train_h_si$of_p1 <- factor(train_h_si$of_p1)
train_h_si$of_p2 <- factor(train_h_si$of_p2)
train_h_si$of_p3 <- factor(train_h_si$of_p3)
train_h_si$of_p4 <- factor(train_h_si$of_p4)
train_h_si$of_p5 <- factor(train_h_si$of_p5)
train_h_si$of_p6 <- factor(train_h_si$of_p6)
train_h_si$of_p7 <- factor(train_h_si$of_p7)
train_h_si$of_p8 <- factor(train_h_si$of_p8)
train_h_si$of_p9 <- factor(train_h_si$of_p9)

train_h_si$educ_p1 <- factor(train_h_si$educ_p1)
train_h_si$educ_p2 <- factor(train_h_si$educ_p2)
train_h_si$educ_p3 <- factor(train_h_si$educ_p3)
train_h_si$educ_p4 <- factor(train_h_si$educ_p4)
train_h_si$educ_p5 <- factor(train_h_si$educ_p5)
train_h_si$educ_p6 <- factor(train_h_si$educ_p6)
train_h_si$educ_p7 <- factor(train_h_si$educ_p7)
train_h_si$educ_p8 <- factor(train_h_si$educ_p8)
train_h_si$educ_p9 <- factor(train_h_si$educ_p9)

train_h_si$mujer_jf_h <- factor(train_h_si$mujer_jf_h, labels = c("Hombre Jefe de hogar", "Mujer Jefe de Hogar"))
train_h_si$jf_10_18_h <- factor(train_h_si$jf_10_18_h)
train_h_si$jf_19_28_h <- factor(train_h_si$jf_19_28_h)
train_h_si$jf_29_59_h <- factor(train_h_si$jf_29_59_h)
train_h_si$jf_60_h <- factor(train_h_si$jf_60_h)
train_h_si$jf_sub <- factor(train_h_si$jf_sub, labels = c("jefe de hogar no subsidiado", "jefe de hogar subsidiado" ))
train_h_si$pj_jf_ofhogar <- factor(train_h_si$pj_jf_ofhogar)
train_h_si$pj_jf_sintrabajo <- factor(train_h_si$pj_jf_sintrabajo, labels = c("Pareja Sin Trabajo", "Pareja Con Trabajo"))

#2.4. Guardar la base train ----   

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(train_h_si,"./stores/train_h_si.rds")


#2.5. Tablas descriptivas ---- 

#se carga la base ajustada
setwd("~/GitHub/MECA_BD_PS2")
train_h_si <-readRDS("./stores/train_h_si.rds")

#se instala y se carga el paquete de tablas lindas
install.packages("gtsummary")
require ("gtsummary") #buen paquete para tablas descriptivas
require("haven")
train_h_si <- zap_labels(train_h_si)


#Tabla de Pobre versus Dominio
train_h_si %>%
  select(Dominio, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

#Tabla de Pobre versus mujer_jf_h
train_h_si %>%
  select(mujer_jf_h, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

#Tabla de Pobre versus mujer_jf_h
train_h_si %>%
  select(jf_10_18_h, jf_19_28_h, jf_29_59_h, jf_60_h, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

#Tabla de P5000, P5010, P5090, Pobre
train_h_si %>%
  select(P5000, P5010, P5090, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()


#2.6. Gráficas para el análisis de datos---- 

#prueba 1 de gráfica
ggplot(train_h_si, aes(x=ht_p1, y=Ingtotug,color=mujer_jf_h)) + 
  geom_point(aes(color=factor(Pobre))) +
  labs(x='Horas trabajadas Jefe Hogar', y='Ingreso total')

#prueba 2 de gráfica
Conf2x2 = matrix(c(2:2), nrow=4, byrow=FALSE)
layout(Conf2x2)
hist(train_h$Ingtotug,main = "Histograma Ingreso total", xlab = "Ingreso total", col = "skyblue4")
hist(train_h$horas_trabajadas,main = "Histograma Horas Trabajadas", xlab = "Horas Trabajadas", col = "skyblue4")
boxplot(train_h$Ingtotug,main = "Boxplot Ingreso total", xlab = "Ingreso total", col = "skyblue4")
boxplot(train_h$horas_trabajadas,main = "Boxplot Horas Trabajadas", xlab = "Horas Trabajadas", col = "skyblue4")

#prueba 3 de gráfica
train_h_si_rows <- nrow(train_h_si)

ggplot(train_h_si, aes(x=mujer_jf_h,fill=Pobre)) +
  scale_y_continuous(labels = function(x) paste0(round((x*100)/train_h_si_rows, 2), "%")) +
  geom_bar(stat='count', position=position_dodge()) + 
  labs(x='Género', y='Cantidad') +
  theme_minimal() +
  scale_fill_brewer(palette="Blues")

# juntar graficas

ggplot(train_h_si, aes(x=pj_jf_sintrabajo,fill=Pobre)) +
  scale_y_continuous(labels = function(x) paste0(round((x*100)/train_h_si_rows, 2), "%")) +
  geom_bar(stat='count', position=position_dodge()) + 
  labs(x='Estado Laboral Pareja Jefe Hogar', y='Cantidad') +
  theme_minimal() +
  scale_fill_brewer(palette="Blues")
 
#2.7. Identificar variables importantes en modelo de clasificación---- 

# Se cargan las librerías necesarias
library(pacman)
p_load(tidyverse, ggplot2, doParallel, rattle, MLmetrics,
       janitor, fastDummies, tidymodels, caret)

# Creamos el primer modelo
modelo1 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Dado que estos modelos son computacionalmente demandantes, vamos a distribuir los cálculos en diferentes nucleos de nuestro procesador para acelerar el proceso.

# Identificamos cuántos cores tiene nuestra máquina
n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))


# Vamos a usar n_cores - 2 procesadores para esto
cl <- makePSOCKcluster(n_cores - 6) 
registerDoParallel(cl)

# Se define Pobre como factor
train_h_si$Pobre <- as.factor(train_h_si$Pobre)

# Entrenamos el modelo utilizando procesamiento en paralelo
modelo1_fit <- fit(modelo1, Pobre ~ ., data = train_h_si)

# Liberamos nuestros procesadores
stopCluster(cl)

# Importancia de las variables
importancia <- varImp(modelo1_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()

summary (train_h_si$P5010)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PREPARACIÓN DE LA BASE DE DATOS TEST Y ESTADÍSTICAS DESCRIPTIVAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#3.1. Primera definición base test----

test_h0 <- test_hogares

#Resumen de variables del test personas
summary(test_personas$P6800)#Horas trabajadas NAs 119837
summary(test_personas$Pet)#Población en edad de trabajar- NAs 38829
summary(test_personas$P6210)#Nivel educativo - NAs 9242
summary(test_personas$P6020)#Sexo 1 hombre 2 mujer 
summary(test_personas$P6050)#Parentezco con el jefe de hogar
summary(test_personas$P6040)#Edad
summary(test_personas$Oficio)#oficio
summary(test_personas$P6090)#Es beneficiario de alguna entidad social en salud Nas 119837
summary(test_personas$P6100)#Regimenes de seguridad social en salud #126064

#Se hace un pivot de variables del test personas

edad_pivot <- test_personas %>%  pivot_wider (names_from = P6050,#Parentezco con el jefe de hogar 
                                               values_from = P6040,#edad 
                                               names_prefix = "edad",
                                               values_fill = 0)

edad_pivot <- select (edad_pivot,
                                id,
                                edad1,
                                edad2,
                                edad3,
                                edad4,
                                edad5,
                                edad6,
                                edad7,
                                edad8,
                                edad9)

horas_trabajadas_pivot <- test_personas %>% pivot_wider(names_from = P6050, 
                                                         values_from = P6800, 
                                                         names_prefix = "ht",
                                                         values_fill = 0)


horas_trabajadas_pivot <- select (horas_trabajadas_pivot,
                                            id,
                                            ht1,
                                            ht2,
                                            ht3,
                                            ht4,
                                            ht5,
                                            ht6,
                                            ht7,
                                            ht8,
                                            ht9)

oficio_pivot <- test_personas %>% pivot_wider(names_from = P6050, 
                                               values_from = Oficio, 
                                               names_prefix = "of",
                                               values_fill = 0)

oficio_pivot <- select (oficio_pivot,
                                  id,
                                  of1,
                                  of2,
                                  of3,
                                  of4,
                                  of5,
                                  of6,
                                  of7,
                                  of8,
                                  of9)

educ_pivot <- test_personas %>% pivot_wider(names_from = P6050, 
                                             values_from = P6210, 
                                             names_prefix = "educ",
                                             values_fill = 0)


educ_pivot <- select (educ_pivot,
                                id,
                                educ1,
                                educ2,
                                educ3,
                                educ4,
                                educ5,
                                educ6,
                                educ7,
                                educ8,
                                educ9)

#Como de cada tipo de persona pueden haber varios en el hogar, 
#se agregan los casos por tipo de persona segun la función

test_personas_colaps_edad <- edad_pivot %>% 
  group_by(id) %>%
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

test_personas_colaps_ht <- horas_trabajadas_pivot %>% 
  group_by(id) %>%
  summarize(
    ht_p1 = max (ht1,na.rm = TRUE), 
    ht_p2 = max (ht2,na.rm = TRUE), 
    ht_p3 = max (ht3,na.rm = TRUE), 
    ht_p4 = max (ht4,na.rm = TRUE), 
    ht_p5 = max (ht5,na.rm = TRUE), 
    ht_p6 = max (ht6,na.rm = TRUE), 
    ht_p7 = max (ht7,na.rm = TRUE), 
    ht_p8 = max (ht8,na.rm = TRUE),
    ht_p9 = max (ht9,na.rm = TRUE))

test_personas_colaps_oficio <- oficio_pivot %>% 
  group_by(id) %>%
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

test_personas_colaps_educ <- educ_pivot %>% 
  group_by(id) %>%
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

#Se hace join
test_h0 <- 
  inner_join(test_h0, test_personas_colaps_edad,
             by = c("id"))

test_h0 <- 
  inner_join(test_h0, test_personas_colaps_ht,
             by = c("id"))

test_h0 <- 
  inner_join(test_h0, test_personas_colaps_oficio,
             by = c("id"))

test_h0 <- 
  inner_join(test_h0, test_personas_colaps_educ,
             by = c("id"))

#Creación de otras variables

#Creación de otras variables

test_personas_jf <- test_personas %>% mutate(
  mujer_jf_h = if_else(P6020==2 & P6050==1, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_10_18_h = if_else(P6050==1 & P6040>=10 & P6040<=18, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_19_28_h = if_else(P6050==1 & P6040>=19 & P6040<=28, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_29_59_h = if_else(P6050==1 & P6040>=29 & P6040<=59, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_60_h = if_else(P6050==1 & P6040>=60, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_sub = if_else(P6050==1 & P6100==3, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_afiliado = if_else(P6050==1 & P6090==2 | P6090==3, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_sintrabajo = if_else(P6050==1 & P6240==2 | P6240==5, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  pj_jf_ofhogar = if_else(P6050==2 & P6240==4, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  pj_jf_sintrabajo = if_else(P6050==2 & P6240==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_nc_pension = if_else(P6050==1 & P6920==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_P7422 = if_else(P6050==1 & P7422==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  pj_jf_P7422 = if_else(P6050==2 & P7422==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_P7472 = if_else(P6050==1 & P7472==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  pj_jf_P7472 = if_else(P6050==2 & P7472==2, 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  es_hijo = if_else(P6050==3, 1, 0)
)

test_personas_jf <- test_personas_jf %>%
  group_by(id) %>%
  summarise(
    mujer_jf_h = max(mujer_jf_h),
    jf_10_18_h = max(jf_10_18_h),
    jf_19_28_h = max(jf_19_28_h),
    jf_29_59_h = max(jf_29_59_h),
    jf_60_h = max(jf_60_h),
    jf_sub = max(jf_sub),
    jf_afiliado = max(jf_afiliado),
    jf_sintrabajo = max(jf_sintrabajo),
    pj_jf_ofhogar = max(pj_jf_ofhogar),
    pj_jf_sintrabajo = max(pj_jf_sintrabajo),
    jf_nc_pension = max(jf_nc_pension),
    jf_P7422 = max(jf_P7422),
    pj_jf_P7422 = max(pj_jf_P7422),
    jf_P7472 = max(jf_P7472),
    pj_jf_P7472 = max(pj_jf_P7472),
    hijos = sum(es_hijo)
  )

test_h0 <- 
  inner_join(test_h0, test_personas_jf,
             by = c("id"))


#3.2. Identificar NAs base test_h ---- 

test_h <- test_h0
cantidad_na <- sapply(test_h, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(test_h)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 8.3 % de las entradas están vacías"

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
vars_drop <- c("P5100", 
               "jf_P7422",
               "jf_P7472",
               "P5140", 
               "pj_jf_P7422",
               "P5130",
               "jf_afiliado",
               "jf_sintrabajo",
               "pj_jf_P7472",
               "jf_nc_pension")
test_h_si <- test_h[,!(names(test_h) %in% vars_drop)]
k0 <- ncol(test_h)
k1 <- ncol(test_h_si)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

#se crea test hogares borrando las filas de ht_p1 que están con NA

test_h_si <- test_h_si %>% filter(!is.na(jf_sub))#pasa de 66168 a 62479 obs
test_h_si <- test_h_si [is.finite(test_h_si$ht_p1), ] #pasa de 62479 a 59307 obs
test_h_si <- test_h_si %>% filter(!is.na(pj_jf_ofhogar)) #pasa de 59307 a 59298 obs
test_h_si <- test_h_si %>% filter(!is.na(pj_jf_sintrabajo)) #pasa de 59298 a 59298 obs


#3.3. Convertir en factor variables de base test ---- 

test_h_si$P5000 <- factor(test_h_si$P5000)
test_h_si$P5010 <- factor(test_h_si$P5010)
test_h_si$P5090 <- factor(test_h_si$P5090)
test_h_si$Depto <- factor(test_h_si$Depto)

test_h_si$of_p1 <- factor(test_h_si$of_p1)
test_h_si$of_p2 <- factor(test_h_si$of_p2)
test_h_si$of_p3 <- factor(test_h_si$of_p3)
test_h_si$of_p4 <- factor(test_h_si$of_p4)
test_h_si$of_p5 <- factor(test_h_si$of_p5)
test_h_si$of_p6 <- factor(test_h_si$of_p6)
test_h_si$of_p7 <- factor(test_h_si$of_p7)
test_h_si$of_p8 <- factor(test_h_si$of_p8)
test_h_si$of_p9 <- factor(test_h_si$of_p9)

test_h_si$educ_p1 <- factor(test_h_si$educ_p1)
test_h_si$educ_p2 <- factor(test_h_si$educ_p2)
test_h_si$educ_p3 <- factor(test_h_si$educ_p3)
test_h_si$educ_p4 <- factor(test_h_si$educ_p4)
test_h_si$educ_p5 <- factor(test_h_si$educ_p5)
test_h_si$educ_p6 <- factor(test_h_si$educ_p6)
test_h_si$educ_p7 <- factor(test_h_si$educ_p7)
test_h_si$educ_p8 <- factor(test_h_si$educ_p8)
test_h_si$educ_p9 <- factor(test_h_si$educ_p9)

test_h_si$mujer_jf_h <- factor(test_h_si$mujer_jf_h)
test_h_si$jf_10_18_h <- factor(test_h_si$jf_10_18_h)
test_h_si$jf_19_28_h <- factor(test_h_si$jf_19_28_h)
test_h_si$jf_29_59_h <- factor(test_h_si$jf_29_59_h)
test_h_si$jf_60_h <- factor(test_h_si$jf_60_h)
test_h_si$jf_sub <- factor(test_h_si$jf_sub)
test_h_si$pj_jf_ofhogar <- factor(test_h_si$pj_jf_ofhogar)
test_h_si$pj_jf_sintrabajo <- factor(test_h_si$pj_jf_sintrabajo)


#3.4. Guardar la base test ----   

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(test_h_si,"./stores/test_h_si.rds")

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

