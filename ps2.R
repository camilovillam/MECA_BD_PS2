setwd("/Users/jorgeeduardogarcia/Desktop/BIG_DATA/MECA_BD_PS2")
train_h <-readRDS("./stores/train_h_si.rds")


#Cargar librerías:
install.packages("pacman")
require(pacman)
p_load(rio, 
       tidyverse, 
       skimr, 
       caret,
       rvest,
       stargazer,
       revest,
       fabricatr,
       GGally,
       tableone,
       arsenal,
       janitor,
       pacmam)

#Dado que se debe realizar la predicciónn del ingreso, se escoge la variable Ingtotugarr
predict <- stats::predict

# se realizan diferentes boxplot de las variables aparentemente m?s releventes 

boxplot(train_h$Ingtotug,main = "BOXPLOT - Ingreso Total", xlab = "Ingreso total", col = "110")
boxplot(train_h$Ingtotug ~ train_h$mujer_jf_h, main ="BOXPLOT - Sexo jefe del hogar", 
        xlab = "Sexo jefe del hogar (hombre = 0 - mujer = 1)", ylab = "Ingreso total", col= "110")
boxplot(train_h$Ingtotug ~ train_h$Clase, main ="BOXPLOT - Clase", 
        xlab = "Clase (urbano = 1 - resto = 2)", ylab = "Ingreso total", col= "110")
boxplot(train_h$Ingtotug ~ train_h$educ_p1, main ="BOXPLOT - educaci?n", 
        xlab = "educaci?n", ylab = "Ingreso total", col= "110")

#se realiza la partición de la base 
set.seed(10101)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]

set.seed(100)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1) 

other <- train_h[-split1,]
Tr_train <- train_h[split1,]

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

Tr_eval <- other[ split2,]
Tr_test <- other[-split2,]

#Modelos
model_1<-as.formula(Ingtotug ~ Clase + P5090 + educ_p1 + Nper + jf_10_18_h + jf_60_h + hijos)

#Ahora se hace la estimación
model_1_lm <- lm(model_1 , data = Tr_train)
stargazer(model_1_lm,type="text")

#Se entrena el modelo

model_1estimado <- train(model_1,
                          data = Tr_train,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

#Ahora se predice el modelo
Predicción_modelo1 <- predict(model_1estimado,newdata = Tr_test )

#Ahora se calcula el MSE 

#Cálculo del MSE:
MSE_model_1 <- with (Tr_test,mean((Ingtotug - Predicción_modelo1)^2))

MSE_model_1


#Se guardan los resultados
Tr_test$M_1 <- Predicción_modelo1

#Se determina si la persona es pobre o no a partir del ingreso
Tr_test$clasif_pobre_M_1 <- factor(if_else( Tr_test$M_1 < Tr_test$Lp, "Pobre", "No Pobre"))
summary(Tr_test$clasif_pobre_M_1)

#Se hace la matriz de confusión 

Matriz_M_1 <- confusionMatrix(data=Tr_test$clasif_pobre_M_1, 
                       reference=Tr_test$Pobre , 
                       mode="sens_spec" , positive="Pobre")
Matriz_M_1

#Ahora se hace el modelo Lasso

lambda <- 10^seq(-2, 3, length = 200)

lasso1 <- train(model_1,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

#Se hace la predicción
Predicc_lass1 <- predict(lasso1,newdata = Tr_test )

#Se guardan los resultados
Tr_test$M_1_lass1 <- Predicc_lass1


#Se determina si la persona es pobre o no a partir del ingreso
Tr_test$clasif_pobre_lass1 <- factor(if_else( Tr_test$M_1_lass1 < Tr_test$Lp, "Pobre", "No Pobre"))

summary(Tr_test$clasif_pobre_lass1)

#Se hace la matriz de confusión para Lasso

Matriz_Lasso_1 <- confusionMatrix(data=Tr_test$clasif_pobre_lass1, 
                            reference=Tr_test$Pobre , 
                            mode="sens_spec" , positive="Pobre")

###Ridge
Model_ridge1 <- train(model_1,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))

#Se predice Ridge
Predicc_ridge1 <- predict(Model_ridge1,newdata = Tr_test )

#Se guardan los resultados
Tr_test$M_1_ridge1 <- Predicc_ridge1 

#Se determina si la persona es pobre o no a partir del ingreso
Tr_test$clasif_pobre_ridge1 <- factor(if_else( Tr_test$M_1_ridge1 < Tr_test$Lp, "Pobre", "No Pobre"))

#Se hace la matriz de confusión
Matriz_ridge1 <- confusionMatrix(data=Tr_test$clasif_pobre_ridge1, 
                             reference=Tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

##Elastic Net
Model_1_elnet1 <- train(model_1,
                data = Tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                preProcess = c("center", "scale"))

#Se hace la predicción
Predicc_elnet1 <- predict(Model_1_Elnet1,newdata = Tr_test )

#Se guarda el modelo
Tr_test$M_1_Elnet1 <- Predicc_elnet1

#Se determina si la persona es pobre o no a partir del ingreso
Tr_test$clasif_pobre_elnet1 <- factor(if_else( Tr_test$M_1_Elnet1 < Tr_test$Lp, "Pobre", "No Pobre"))

#Se hace la matriz de confusión
Matriz_elnet1 <- confusionMatrix(data=Tr_test$clasif_pobre_elnet1, 
                                 reference=Tr_test$Pobre , 
                                 mode="sens_spec" , positive="Pobre")

##SE COMPARAN LOS MODELOS
models <- list(lasso1, Model_ridge1, Model_1_elnet1)
here
