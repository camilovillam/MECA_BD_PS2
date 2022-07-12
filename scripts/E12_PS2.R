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
       MASS,
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
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
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
  jf_sub = if_else(P6050==1 & (P6100==3 | P6100==4), 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_afiliado = if_else(P6050==1 & (P6090==2 | P6090==3), 1, 0)
)

train_personas_jf <- train_personas_jf %>% mutate(
  jf_sintrabajo = if_else(P6050==1 & (P6240==2 | P6240==5), 1, 0)
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
#En promedio el 6.94% de las entradas están vacías"

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
               "pj_jf_P7472",
               "jf_nc_pension")
train_h_si <- train_h[,!(names(train_h) %in% vars_drop)]
k0 <- ncol(train_h)
k1 <- ncol(train_h_si)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

train_h_si_bu <- train_h_si#se guarda bu para crear mas adelante otra base imputando

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
train_h_si$jf_afiliado <- factor(train_h_si$jf_afiliado)
train_h_si$jf_sintrabajo <- factor(train_h_si$jf_sintrabajo)

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
train_h_si_rows <- nrow(train_h_si)

grafica_3 <- 
  ggplot(train_h_si, aes(x=mujer_jf_h,fill=Pobre)) +
  scale_y_continuous(labels = function(x) paste0(round((x*100)/train_h_si_rows, 2), "%")) +
  geom_bar(stat='count', position=position_dodge()) + 
  labs(x='Género', y='Cantidad') +
  theme_minimal() +
  scale_fill_brewer(palette="Blues")

#prueba 4 de gráfica
grafica_4 <- 
  ggplot(train_h_si, aes(x=pj_jf_sintrabajo,fill=Pobre)) +
  scale_y_continuous(labels = function(x) paste0(round((x*100)/train_h_si_rows, 2), "%")) +
  geom_bar(stat='count', position=position_dodge()) + 
  labs(x='Estado Laboral Pareja Jefe Hogar', y='Cantidad') +
  theme_minimal() +
  scale_fill_brewer(palette="Blues")

# juntar graficas 3 y 4
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

figuras_3_y_4 <- ggarrange(grafica_3, grafica_4,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
# Ver figura
figuras_3_y_4
 
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
  jf_sub = if_else(P6050==1 & (P6100==3 | P6100==4), 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_afiliado = if_else(P6050==1 & (P6090==2 | P6090==3), 1, 0)
)

test_personas_jf <- test_personas_jf %>% mutate(
  jf_sintrabajo = if_else(P6050==1 & (P6240==2 | P6240==5), 1, 0)
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
#En promedio el 7.14 % de las entradas están vacías"

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
               "pj_jf_P7472",
               "jf_nc_pension")
test_h_si <- test_h[,!(names(test_h) %in% vars_drop)]
k0 <- ncol(test_h)
k1 <- ncol(test_h_si)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

#se crea test hogares imputando de las filas que están con NA

library("recipes")

test_h_si_bu  <- test_h_si

test_h_si <- test_h_si_bu

# Agregar variable temporal con valores sin NA. Lo que sea NA se reemplaza con 1.
test_h_si <- test_h_si %>% mutate(
  jf_sub_SNA =  if_else(is.na(jf_sub), 1,jf_sub)
)
# Eliminar columna con NA
drops <- c("jf_sub")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  jf_sub = jf_sub_SNA
)

# Agregar variable temporal con valores sin NA. Lo que sea NA se reemplaza con 1.
test_h_si <- test_h_si %>% mutate(
  pj_jf_ofhogar_SNA =  if_else(is.na(pj_jf_ofhogar), 1, pj_jf_ofhogar)
)
# Eliminar columna con NA
drops <- c("pj_jf_ofhogar")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  pj_jf_ofhogar = pj_jf_ofhogar_SNA
)

# Agregar variable temporal con valores sin NA. Lo que sea NA se reemplaza con 1.
test_h_si <- test_h_si %>% mutate(
  pj_jf_sintrabajo_SNA =  if_else(is.na(pj_jf_sintrabajo), 1, pj_jf_sintrabajo)
)
# Eliminar columna con NA
drops <- c("pj_jf_sintrabajo")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  pj_jf_sintrabajo = pj_jf_sintrabajo_SNA
)

# Agregar variable temporal con valores sin NA. Lo que sea NA se reemplaza con 1.
test_h_si <- test_h_si %>% mutate(
  jf_afiliado_SNA =  if_else(is.na(jf_afiliado), 1, jf_afiliado)
)
# Eliminar columna con NA
drops <- c("jf_afiliado")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  jf_afiliado = jf_afiliado_SNA
)

# Agregar variable temporal con valores sin NA. Lo que sea NA se reemplaza con 1.
test_h_si <- test_h_si %>% mutate(
  jf_sintrabajo_SNA =  if_else(is.na(jf_sintrabajo), 1, jf_sintrabajo)
)
# Eliminar columna con NA
drops <- c("jf_sintrabajo")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  jf_sintrabajo = jf_sintrabajo_SNA
)

# Agregar variable temporal con valores sin Infinito. Lo que sea Infinito se reemplaza con la mediana.
test_h_si <- test_h_si %>% mutate(
  ht_p1_SINF =  if_else(is.infinite(ht_p1), median(test_h_si$ht_p1), ht_p1)
)
# Eliminar columna con Infinito
drops <- c("ht_p1")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  ht_p1 = ht_p1_SINF
)

# Agregar variable temporal con valores sin Infinito. Lo que sea Infinito se reemplaza con 0.
test_h_si <- test_h_si %>% mutate(
  of_p1_SINF =  if_else(is.infinite(of_p1), 0, of_p1)
)
# Eliminar columna con Infinito
drops <- c("of_p1")
test_h_si <- test_h_si[, !names(test_h_si) %in% drops]
# Renombrar columna a nombre anterior
test_h_si <- test_h_si %>% rename(
  of_p1 = of_p1_SINF
)

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

test_h_si$mujer_jf_h <- factor(test_h_si$mujer_jf_h, labels = c("Hombre Jefe de hogar", "Mujer Jefe de Hogar"))
test_h_si$jf_10_18_h <- factor(test_h_si$jf_10_18_h)
test_h_si$jf_19_28_h <- factor(test_h_si$jf_19_28_h)
test_h_si$jf_29_59_h <- factor(test_h_si$jf_29_59_h)
test_h_si$jf_60_h <- factor(test_h_si$jf_60_h)
test_h_si$jf_sub <- factor(test_h_si$jf_sub,labels = c("jefe de hogar no subsidiado", "jefe de hogar subsidiado" )) 
test_h_si$pj_jf_ofhogar <- factor(test_h_si$pj_jf_ofhogar)
test_h_si$pj_jf_sintrabajo <- factor(test_h_si$pj_jf_sintrabajo, labels = c("Pareja Sin Trabajo", "Pareja Con Trabajo"))
test_h_si$jf_afiliado <- factor(test_h_si$jf_afiliado)
test_h_si$jf_sintrabajo <- factor(test_h_si$jf_sintrabajo)

#3.4. Guardar la base test ----   

#Se guarda la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS2")
saveRDS(test_h_si,"./stores/test_h_si.rds")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. MODELOS DE CLASIFICACIÓN DE POBREZA----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Temporal: lectura BD

rm(list=ls())
gc()

setwd("~/GitHub/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")

train_h$Pobre <- factor(train_h$Pobre,levels=c("No Pobre","Pobre"),labels=c("No_pobre","Pobre"))
# train_h$P5000 <- factor(train_h$P5000)
# train_h$P5090 <- factor(train_h$P5090)


#TEMPORAL: ELIMINAR 98 y 16
#nrow(train_h)
#train_h <- train_h[!(train_h$P5000=="98" | train_h$P5000=="16"),]
#nrow(train_h)


#Temporal, eliminar todos los NA
#train_h <- train_h[complete.cases(train_h), ]

predict <- stats::predict

##4.1. Partición de la base de datos en tres----

#La base de datos Train se divide en tres particiones:
# Tr_train: Entrenar el modelo
# Tr_eval: Evaluar, ajustar y refinar el modelo
# Tr_test: Probar el modelo

#Balance inicial
prop.table(table(train_h$Pobre))

### Generación de particiones ----
set.seed(100)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1)

other <- train_h[-split1,]
Tr_train <- train_h[split1,]

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

Tr_eval <- other[ split2,]
Tr_test <- other[-split2,]


### Balance final ----
prop.table(table(train_h$Pobre))
prop.table(table(Tr_train$Pobre))
prop.table(table(Tr_eval$Pobre))
prop.table(table(Tr_test$Pobre))

nrow(Tr_train)
nrow(Tr_test)

rm(other)


## 4.2. Modelos Logit ----
# Se ensayan 5 modelos diferentes con distintas variables, como una primera
# aproximación.
# Más adelante se introducen más variaciones y métodos
# sobre los mejores modelos.

colnames(train_h)

#Se guarda la fórmula del modelo

# form_logit_1 <- as.formula("Pobre ~ Npersug + P5090 + P5000 + edad_p1 + 
#                            mujer_jf_h + educ_p1 + ht_p1 + jf_sub")

fmodelo1 <- as.formula("Pobre ~ jf_sub + educ_p3 + Clase + P5000 + ht_p1")

fmodelo2 <- as.formula("Pobre ~ Npersug + P5010 + P5090 + P5000 + edad_p1 + 
                       mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + jf_sub + 
                       hijos + ht_p1 + pj_jf_sintrabajo + jf_sintrabajo")

fmodelo3 <- as.formula("Pobre ~ Npersug + P5090 + P5000 + edad_p1 + 
                       mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + jf_sub + 
                       hijos + ht_p1 + pj_jf_sintrabajo + jf_sintrabajo")

fmodelo4 <- as.formula("Pobre ~ Npersug:P5010 + P5090 + P5000 + edad_p1 + 
                       mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + jf_sub + 
                       hijos + ht_p1 + pj_jf_sintrabajo + jf_sintrabajo")

fmodelo5 <- as.formula("Pobre ~ Npersug + P5090 + P5000 + edad_p1 + 
                       mujer_jf_h + educ_p1 + educ_p3 + ht_p1 + jf_sub + 
                       hijos + ht_p1 + pj_jf_sintrabajo")


#Se estima el modelo Logit
#mod_logit_1 <-  glm(form_logit_1,data= Tr_train, family=binomial(link="logit"))

mod_logit_1 <-  glm(fmodelo1,data= Tr_train, family=binomial(link="logit"))
mod_logit_2 <-  glm(fmodelo2,data= Tr_train, family=binomial(link="logit"))
mod_logit_3 <-  glm(fmodelo3,data= Tr_train, family=binomial(link="logit"))
mod_logit_4 <-  glm(fmodelo4,data= Tr_train, family=binomial(link="logit"))
mod_logit_5 <-  glm(fmodelo5,data= Tr_train, family=binomial(link="logit"))

stargazer(mod_logit_1, type="text")
stargazer(mod_logit_2, type="text")
stargazer(mod_logit_3, type="text")
stargazer(mod_logit_4, type="text")
stargazer(mod_logit_5, type="text")

summary(mod_logit_1, type="text")
summary(mod_logit_2, type="text")
summary(mod_logit_3, type="text")
summary(mod_logit_4, type="text")
summary(mod_logit_5, type="text")

#TEMPORAL:
#Tr_test <- Tr_test[!(Tr_test$P5000=="98" | Tr_test$P5000=="16"),]


## Predicción Pobre sobre la base de Test
Tr_test$predict_logit_1 <- predict(mod_logit_1, Tr_test, type="response")
Tr_test$predict_logit_2 <- predict(mod_logit_2, Tr_test, type="response")
Tr_test$predict_logit_3 <- predict(mod_logit_3, Tr_test, type="response")
Tr_test$predict_logit_4 <- predict(mod_logit_4, Tr_test, type="response")
Tr_test$predict_logit_5 <- predict(mod_logit_5, Tr_test, type="response")


## Se hace un gráfico de cajas y bigotes para explorar el punto de corte
ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_1)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_2)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_3)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_4)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_5)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

#Se agrega el resultado de la predicción según la probabilidad de Logit
#El punto de corte se define por inspección gráfica

Tr_test <- Tr_test %>% 
  mutate(p_logit_1 = ifelse(predict_logit_1 < 0.23,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

Tr_test <- Tr_test %>% 
  mutate(p_logit_2 = ifelse(predict_logit_2 < 0.22,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

Tr_test <- Tr_test %>% 
  mutate(p_logit_3 = ifelse(predict_logit_3 < 0.20,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

Tr_test <- Tr_test %>% 
  mutate(p_logit_4 = ifelse(predict_logit_4 < 0.18,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

Tr_test <- Tr_test %>% 
  mutate(p_logit_5 = ifelse(predict_logit_5 < 0.2,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

#Métricas de resultados:

## Matrices de confusión

cmat_lg_1 <- confusionMatrix(data=Tr_test$p_logit_1, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cmat_lg_2 <- confusionMatrix(data=Tr_test$p_logit_2, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cmat_lg_3 <- confusionMatrix(data=Tr_test$p_logit_3, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cmat_lg_4 <- confusionMatrix(data=Tr_test$p_logit_4, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cmat_lg_5 <- confusionMatrix(data=Tr_test$p_logit_5, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cmat_lg_1
cmat_lg_2
cmat_lg_3
cmat_lg_4
cmat_lg_5


## ROC
pred_lg_1 <- prediction(Tr_test$predict_logit_1, Tr_test$Pobre)
pred_lg_2 <- prediction(Tr_test$predict_logit_2, Tr_test$Pobre)
pred_lg_3 <- prediction(Tr_test$predict_logit_3, Tr_test$Pobre)
pred_lg_4 <- prediction(Tr_test$predict_logit_4, Tr_test$Pobre)
pred_lg_5 <- prediction(Tr_test$predict_logit_5, Tr_test$Pobre)

roc_ROCR_lg_1 <- performance(pred_lg_1,"tpr","fpr")
roc_ROCR_lg_2 <- performance(pred_lg_2,"tpr","fpr")
roc_ROCR_lg_3 <- performance(pred_lg_3,"tpr","fpr")
roc_ROCR_lg_4 <- performance(pred_lg_4,"tpr","fpr")
roc_ROCR_lg_5 <- performance(pred_lg_5,"tpr","fpr")

plot(roc_ROCR_lg_1, main = "ROC curve", colorize = FALSE, col="orange")
plot(roc_ROCR_lg_2, main = "ROC curve", add=TRUE, colorize = FALSE, col="blue")
plot(roc_ROCR_lg_3, main = "ROC curve", add=TRUE, colorize = FALSE, col="green")
plot(roc_ROCR_lg_4, main = "ROC curve", add=TRUE, colorize = FALSE, col="red")
plot(roc_ROCR_lg_5, main = "ROC curve", add=TRUE, colorize = FALSE, col="yellow")
abline(a = 0, b = 1)


## AUC
auc_roc_lg_1  <-  performance(pred_lg_1, measure = "auc")
auc_roc_lg_2  <-  performance(pred_lg_2, measure = "auc")
auc_roc_lg_3  <-  performance(pred_lg_3, measure = "auc")
auc_roc_lg_4  <-  performance(pred_lg_4, measure = "auc")
auc_roc_lg_5  <-  performance(pred_lg_5, measure = "auc")

auc_roc_lg_1@y.values[[1]]
auc_roc_lg_2@y.values[[1]]
auc_roc_lg_3@y.values[[1]]
auc_roc_lg_4@y.values[[1]]
auc_roc_lg_5@y.values[[1]]



## Validación cruzada K-Fold:

#Modelo_seleccionado:

#ESTO SE DEBE AJUSTAR SI SE CAMBIAN LOS MODELOS:

fmodelo_sel <- fmodelo4
modelo_sel <- mod_logit_4


#Definición del control (a usarse en los demás modelos)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

## Entrenar el modelo
caret_logit  <-  train(fmodelo_sel,
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

auc_roc_cv = performance(pred_cv, measure = "auc")
auc_roc_cv@y.values[[1]]


## Punto de corte óptimo:

evalResults <- data.frame(Pobre = Tr_eval$Pobre)


evalResults$Roc <- predict(caret_logit, newdata = Tr_eval,
                           type = "prob")[,2] ##OJO!!! ¿1 o 2?


rfROC <- roc(evalResults$Pobre, evalResults$Roc, levels = rev(levels(evalResults$Pobre)))
rfROC

rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh


evalResults <- evalResults %>% 
  mutate(hat_def_05=ifelse(evalResults$Roc < 0.5,"No_pobre","Pobre"),
         hat_def_rfThresh=ifelse(evalResults$Roc < rfThresh$threshold,"No_pobre","Pobre"))

with(evalResults,table(Pobre,hat_def_05))
with(evalResults,table(Pobre,hat_def_rfThresh))

#Ahora en Test con el cut off óptimo:

Tr_test$caret_logit_roc <- predict(caret_logit, newdata = Tr_test,
                                   type = "prob")[,2] ##OJO!!! ¿1 o 2?


Tr_test <- Tr_test %>% 
  mutate(caret_logit_hat_def_05=ifelse(Tr_test$caret_logit_roc < 0.5,"No_pobre","Pobre"),
         caret_logit_hat_def_rfThresh=ifelse(Tr_test$caret_logit_roc < rfThresh$threshold,"No_pobre","Pobre"))

Tr_test$caret_logit_hat_def_05 <- factor(Tr_test$caret_logit_hat_def_05)
Tr_test$caret_logit_hat_def_rfThresh <- factor(Tr_test$caret_logit_hat_def_rfThresh)

## Matriz de confusión
cm_caret_coff_opt <- confusionMatrix(data=Tr_test$caret_logit_hat_def_rfThresh, 
                                     reference=Tr_test$Pobre , 
                                     mode="sens_spec" , positive="Pobre")

cm_caret_coff_opt


## 4.3. Rebalanceo de clases, remuestreo ----

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

fmodelo_sel



predictors <-c ("Npersug" , "P5010" , "P5090" , "P5000" , "edad_p1" ,
                "mujer_jf_h" , "educ_p1" , "educ_p3" , "ht_p1" , "jf_sub" ,
                "hijos" , "ht_p1" , "pj_jf_sintrabajo" , "jf_sintrabajo")

head(Tr_train[predictors])


#Vuelvo dummies los factores
trainX <- data.frame(model.matrix(modelo_sel,data=Tr_train))[-1]

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


#Corro el logit básico con UpSample y DownSample:

modelo_sel_ups <-  glm(fmodelo_sel,data= upSampledTrain, family=binomial(link="logit"))
modelo_sel_downs <-  glm(fmodelo_sel,data= downSampledTrain, family=binomial(link="logit"))

stargazer(modelo_sel_ups, type="text")
stargazer(modelo_sel_downs, type="text")


#TEMPORAL:
#Tr_test <- Tr_test[!(Tr_test$P5000=="98" | Tr_test$P5000=="16"),]
Tr_test <- Tr_test[!(Tr_test$P5000=="12"),]

Tr_test$predict_logit_ups <- predict(modelo_sel_ups, Tr_test, type="response")
Tr_test$predict_logit_downs <- predict(modelo_sel_downs, Tr_test, type="response")


ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_ups)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=Tr_test , mapping = aes(Pobre, predict_logit_downs)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()


Tr_test <- Tr_test %>% 
  mutate(p_logit_mod_sel_ups = ifelse(predict_logit_ups < 0.5,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

Tr_test <- Tr_test %>% 
  mutate(p_logit_mod_sel_downs = ifelse(predict_logit_downs < 0.5,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))


confmat_mod_sel_ups <- confusionMatrix(data=Tr_test$p_logit_mod_sel_ups, 
                                       reference=Tr_test$Pobre , 
                                       mode="sens_spec" , positive="Pobre")

confmat_mod_sel_downs <- confusionMatrix(data=Tr_test$p_logit_mod_sel_downs, 
                                         reference=Tr_test$Pobre , 
                                         mode="sens_spec" , positive="Pobre")

confmat_mod_sel_ups
confmat_mod_sel_downs

#Optimal Cut off

## 4.4. Model Tunning con Caret ----

#Definición del control (a usarse en los demás modelos)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

## Entrenar el modelo
caret_logit  <-  train(fmodelo_sel,
                       data=Tr_train,
                       method="glm",
                       trControl = control,
                       family = "binomial",
                       preProcess = c("center", "scale"))
caret_logit

## predict
Tr_test$p_caret <- predict(caret_logit, Tr_test , type="prob")[2]

## ROC
pred_cv <- prediction(Tr_test$p_caret , Tr_test$Pobre)

roc_ROCR_cv <- performance(pred_cv,"tpr","fpr")

plot(roc_ROCR_cv, main = "ROC curve", add=F, colorize = FALSE, col="blue")

auc_roc_cv = performance(pred_cv, measure = "auc")
auc_roc_cv@y.values[[1]]


###Modelos Logit Lasso ----

#Lasso
lambda_grid <- 10^seq(-4, 0.01, length = 200) #en la practica se suele usar una grilla de 200 o 300
lambda_grid


#Ajustado para sensibilidad:
mylogit_lasso_sens <- train(
  fmodelo_sel,
  data = Tr_train,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

gc()

#Ajustado para ROC:
mylogit_lasso_roc <- train(
  fmodelo_sel,
  data = Tr_train,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

gc()

#Ajustado para accuracy:
mylogit_lasso_acc <- train(
  fmodelo_sel,
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


Tr_test$lasso_sens_clas <- factor(ifelse(Tr_test$lasso_sens<0.2,"No_pobre","Pobre"))
Tr_test$lasso_roc_clas <- factor(ifelse(Tr_test$lasso_roc<0.2,"No_pobre","Pobre"))
Tr_test$lasso_acc_clas <- factor(ifelse(Tr_test$lasso_acc<0.2,"No_pobre","Pobre"))



## Matriz de confusión
cmat_lasso_sens <- confusionMatrix(data=Tr_test$lasso_sens_clas, 
                                   reference=Tr_test$Pobre , 
                                   mode="sens_spec" , positive="Pobre")

cmat_lasso_roc <- confusionMatrix(data=Tr_test$lasso_roc_clas, 
                                  reference=Tr_test$Pobre , 
                                  mode="sens_spec" , positive="Pobre")

cmat_lasso_acc <- confusionMatrix(data=Tr_test$lasso_acc_clas, 
                                  reference=Tr_test$Pobre , 
                                  mode="sens_spec" , positive="Pobre")

cmat_lasso_sens 
cmat_lasso_roc
cmat_lasso_acc


# ROC
pred_lasso_sens <- prediction(Tr_test$lasso_sens, Tr_test$Pobre)
pred_lasso_roc <- prediction(Tr_test$lasso_roc, Tr_test$Pobre)
pred_lasso_acc <- prediction(Tr_test$lasso_acc, Tr_test$Pobre)

roc_ROCR_lasso_sens <- performance(pred_lasso_sens,"tpr","fpr")
roc_ROCR_lasso_roc <- performance(pred_lasso_roc,"tpr","fpr")
roc_ROCR_lasso_acc <- performance(pred_lasso_acc,"tpr","fpr")

plot(roc_ROCR_lasso_sens, main = "ROC curve", add=TRUE, colorize = F,col="red")
plot(roc_ROCR_lasso_roc, main = "ROC curve", add=TRUE, colorize = F,col="orange")
plot(roc_ROCR_lasso_acc, main = "ROC curve", add=TRUE, colorize = F,col="green")


# AUC
auc_roc_lasso_sens  <-  performance(pred_lasso_sens, measure = "auc")
auc_roc_lasso_roc <-  performance(pred_lasso_roc, measure = "auc")
auc_roc_lasso_acc <-  performance(pred_lasso_acc, measure = "auc")

auc_roc_lasso_sens@y.values[[1]]
auc_roc_lasso_roc@y.values[[1]]
auc_roc_lasso_acc@y.values[[1]]


###Modelos Logit Lasso remuestreo ----

gc()

#Ajustado para sensitivity con UPSAMPLE:
mylogit_lasso_sens_ups <- train(
  fmodelo_sel,
  data = upSampledTrain,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

gc()

#Ajustado para sensitivity con DOWNSAMPLE:
mylogit_lasso_sens_downs <- train(
  fmodelo_sel,
  data = downSampledTrain,
  method = "glmnet",
  trControl = control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)


Tr_test$lasso_sens_ups <- predict(mylogit_lasso_sens_ups,
                                  newdata = Tr_test,
                                  type = "prob")[,2] #REVISAR

Tr_test$lasso_sens_downs <- predict(mylogit_lasso_sens_downs,
                                    newdata = Tr_test,
                                    type = "prob")[,2] #REVISAR

Tr_test$lasso_sens_ups_clas <- factor(ifelse(Tr_test$lasso_sens_ups<0.5,"No_pobre","Pobre"))
Tr_test$lasso_sens_downs_clas <- factor(ifelse(Tr_test$lasso_sens_downs<0.5,"No_pobre","Pobre"))


## Matriz de confusión
cmat_lasso_sens_ups <- confusionMatrix(data=Tr_test$lasso_sens_ups_clas, 
                                       reference=Tr_test$Pobre , 
                                       mode="sens_spec" , positive="Pobre")

cmat_lasso_sens_downs <- confusionMatrix(data=Tr_test$lasso_sens_downs_clas, 
                                         reference=Tr_test$Pobre , 
                                         mode="sens_spec" , positive="Pobre")


cmat_lasso_sens_downs

# ROC
pred_lasso_sens_ups <- prediction(Tr_test$lasso_sens_ups, Tr_test$Pobre)
pred_lasso_sens_downs <- prediction(Tr_test$lasso_sens_downs, Tr_test$Pobre)

roc_ROCR_lasso_sens_ups <- performance(pred_lasso_sens_ups,"tpr","fpr")
roc_ROCR_lasso_sens_downs <- performance(pred_lasso_sens_downs,"tpr","fpr")

plot(roc_ROCR_lasso_sens_ups, main = "ROC curve", add=TRUE, colorize = F,col="black")
plot(roc_ROCR_lasso_sens_downs, main = "ROC curve", add=TRUE, colorize = F,col="yellow")
plot(roc_ROCR_lasso_sens_downs, main = "ROC curve", add=FALSE, colorize = T)

# AUC
auc_roc_lasso_sens_ups  <-  performance(pred_lasso_sens_ups, measure = "auc")
auc_roc_lasso_sens_downs  <-  performance(pred_lasso_sens_downs, measure = "auc")

auc_roc_lasso_sens_ups@y.values[[1]]
auc_roc_lasso_sens_downs@y.values[[1]]



###Puntos de cortes alternativos ----

evalResults <- data.frame(Pobre = Tr_eval$Pobre)


evalResults$Roc <- predict(caret_logit, newdata = Tr_eval,
                           type = "prob")[,2] ##OJO!!! ¿1 o 2?


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

mylda <- lda(fmodelo_sel, data = Tr_train)
p_hat_mylda <- predict(mylda, Tr_test, type="response")
pred_mylda <- prediction(p_hat_mylda$posterior[,2], Tr_test$Pobre)

roc_mylda <- performance(pred_mylda,"tpr","fpr")



#Para agregar varias ROC:

plot(roc_ROCR, main = "ROC curve", colorize = FALSE, col="red")
plot(roc_mylda,add=TRUE, colorize = FALSE, col="blue")
abline(a = 0, b = 1)




## 4.5.Otros modelos ----


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

form_xgboost <- fmodelo_sel


grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))
gc()

xgboost <- train(
  form_xgboost,
  data = Tr_train,
  method = "xgbTree",
  trControl = control,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)


gc()

xgboost_downs <- train(
  form_xgboost,
  data = downSampledTrain,
  method = "xgbTree",
  trControl = control,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)


gc()

xgboost_ups <- train(
  form_xgboost,
  data = upSampledTrain,
  method = "xgbTree",
  trControl = control,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)

pred_xgb <- predict(xgboost,Tr_test)
pred_xgb_downs <- predict(xgboost_downs,Tr_test)
pred_xgb_ups <- predict(xgboost_ups,Tr_test)

cmat_xgboost <- confusionMatrix(Tr_test$Pobre,pred_xgb,positive="Pobre")
cmat_xgboost_downs <- confusionMatrix(Tr_test$Pobre,pred_xgb_downs,positive="Pobre")
cmat_xgboost_ups <- confusionMatrix(Tr_test$Pobre,pred_xgb_ups,positive="Pobre")

cmat_xgboost_downs

#Pendiente: Gráfica ROC de XGBoost:
# https://stackoverflow.com/questions/46736934/plotting-the-auc-from-an-xgboost-model-in-r



###Modelo árbol básico (CART) ----

form_tree <- fmodelo_sel

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
c_matr_tree_ups <- confusionMatrix(Tr_test$Pobre,pred_tree_up,positive="Pobre")

c_matr_tree_ups


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
c_matr_tree_downs <- confusionMatrix(Tr_test$Pobre,pred_tree_down,positive="Pobre")

c_matr_tree_downs


##4.6. Comparación final de modelos de clasificación ----

#Se presentan las matrices de confusión de los diferentes modelos,
#para elegir el mejor

#Matrices de confusión completas

cmat_lg_1
cmat_lg_2
cmat_lg_3
cmat_lg_4
cmat_lg_5
cm_caret_coff_opt
confmat_mod_sel_ups
confmat_mod_sel_downs
cmat_lasso_sens
cmat_lasso_roc
cmat_lasso_acc
cmat_lasso_sens_ups
cmat_lasso_sens_downs
cmat_xgboost
cmat_xgboost_downs
cmat_xgboost_ups
c_matr_tree
c_matr_tree_ups
c_matr_tree_downs


#Tabla de comparación:

#Guardamos todas las matrices de confusión en una lista:

Matrices_conf <- vector("list",19)

# Matrices_conf <- list(cmat_lg_1,
#                       cmat_lg_2,
#                       cmat_lg_3,
#                       cmat_lg_4,
#                       cmat_lg_5,
#                       cm_caret_coff_opt,
#                       confmat_mod_sel_ups,
#                       confmat_mod_sel_downs,
#                       cmat_lasso_sens,
#                       cmat_lasso_roc,
#                       cmat_lasso_acc,
#                       cmat_lasso_sens_ups,
#                       cmat_lasso_sens_downs,
#                       cmat_xgboost,
#                       cmat_xgboost_downs,
#                       cmat_xgboost_ups,
#                       c_matr_tree,
#                       c_matr_tree_ups,
#                       c_matr_tree_downs)

Matrices_conf <- list(cmat_lg_1,
                      cmat_lg_2,
                      cmat_lg_3,
                      cmat_lg_4,
                      cmat_lg_5,
                      cm_caret_coff_opt,
                      confmat_mod_sel_ups,
                      confmat_mod_sel_downs,
                      cmat_lasso_sens_downs,
                      cmat_xgboost_downs,
                      c_matr_tree,
                      c_matr_tree_ups,
                      c_matr_tree_downs)


tabla_comp_clasif <- matrix(rep(0,19*9),nrow=19,ncol=9)
colnames(tabla_comp_clasif) <- c("Modelo","TN","FN","TP","FP","Sensitivity","Specificity","Accuracy")

#Ejemplo para una matriz de confusión:

tabla_comp_clasif[1,1] <- "Lasso 1"
tabla_comp_clasif[1,2] <- 
tabla_comp_clasif[1,3] <- 
tabla_comp_clasif[1,4] <- 
tabla_comp_clasif[1,5] <- 
tabla_comp_clasif[1,6] <- Matrices_conf[[1]]@byClass$Sensitivity #Sensitivity
tabla_comp_clasif[1,7] <- #Specificity
tabla_comp_clasif[1,8] <- #Accuracy
tabla_comp_clasif[1,9] <- #Puntaje final (75% / 25%)
  
  
acc_mod1 <- Matrices_conf[[1]]$byClass$Sensitivity

sens_mod1 <- cmat_lg_1$byClass$Sensitivity


view(tabla_comp_clasif)
tabla_comp_clasif

##4.7. Exportación final ----


modelo_final <- caret_logit

setwd("~/GitHub/MECA_BD_PS2")
test_h <-readRDS("./stores/test_h_si.rds")

nrow(test_h)

levels(Tr_train$P5000)
levels(Tr_test$P5000)
levels(test_h$P5000)
levels(test_h$jf_sub)
levels(Tr_test$jf_sub)

## Predecir el modelo final:

#TEMPORAL!
test_h <- test_h[!(test_h$P5000=="43"),]
test_h$jf_sub <- factor(test_h$jf_sub,levels=c("0","1"),labels=c("jefe de hogar no subsidiado","jefe de hogar subsidiado"))


test_h$prediccion_final <- predict(modelo_final, test_h , type="prob")[2]
test_h$Pobre_classification <- ifelse(test_h$prediccion_final < rfThresh$threshold,0,1)

submit  <-  test_h[,c("id","Pobre_classification")]

fmodelo5

## Guardar el .CSV
setwd("~/GitHub/MECA_BD_PS2/document")
export(submit,"./predictions_garcia_molano_villa_c12_r5.csv")




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. MODELOS DE PREDICCIÓN DE INGRESOS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##5.1. Carga de la base de datos----

#se carga la base ajustada
setwd("~/GitHub/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")

#se crean variables
#train_h$ln_ing <- log(train_h$Ingtotug)
train_h <- train_h %>% mutate (edadjf_cua= edad_p1*edad_p1)

#NOTA: Se eliminan las filas  que tienen Inf de la variable ln_ing (revisar cuando se termine el P2)
#train_h <- train_h[is.finite(train_h$ln_ing), ]

##5.2. Partición de la base de datos en tres----

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


##5.3. Modelos de regresión ----

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


##5.4. Lasso, Ridge, Elastic Net de los modelos ----

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


##5.5. Exportación final ----

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



