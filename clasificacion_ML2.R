library(tidyverse)
library(cowplot)
library(PerformanceAnalytics)
library(psych)
library(caret)
library(Boruta)
library(Metrics)

##### Cargar datos
X_train <- read_csv("X_train.csv")
Y_train <- read_csv("Y_train.csv")

## EDA

##### Explorar variables dependientes
str(Y_train)

Y_train <- as.data.frame(lapply(Y_train, as.factor))
summary(Y_train)

str(Y_train)

sapply(Y_train,function(x){sum(is.na(x))})

head(Y_train)

y1 <- ggplot(Y_train, aes(x=M)) +
  geom_bar()+
  theme_minimal()+
  ggtitle('M')

y2 <- ggplot(Y_train, aes(x=MM)) +
  geom_bar()+
  theme_minimal()+
  ggtitle('MM')

y3 <- ggplot(Y_train, aes(x=Combo)) +
  geom_bar()+
  theme_minimal()+
  ggtitle('Combo')

y4 <- ggplot(Y_train, aes(x=DCombo)) +
  geom_bar()+
  theme_minimal()+
  ggtitle('DCombo')

plot_grid(y1, y2, y3, y4, labels = c('AUTO'), nrow=2, ncol=2)


##### Explorar variables independientes
features <- X_train[-1]
str(features)

features <- as.data.frame(unclass(features), stringsAsFactors = TRUE)

summary(features)

str(features)

NAs <- sapply(features,function(x){(sum(is.na(x))/nrow(features))*100})
var_NAs <- NAs[NAs != 0]
var_NAs

features <- features %>% select(-c(hº.act.cerca.sem, origen.antepasados..extranjeros., hº.act.cerca.sem, Grupo.fot, fototipo, origen.antepasados..españa.))

NAs <- sapply(features,function(x){(sum(is.na(x))/nrow(features))*100})
var_NAs <- NAs[NAs != 0]
var_NAs

num_NAs <- names(which(sapply(features[,which(NAs != 0)], is.numeric)))
cat_NAs <- names(which(sapply(features[,which(NAs != 0)], is.factor)))

# Imputar missing values para variables numéricas
for (vnum in num_NAs){
  features[[vnum]][is.na(features[[vnum]])] <- median(features[[vnum]],na.rm = TRUE)
}

# Añadir categoría NoData para las variables categóricas con missing values
for (vcat in cat_NAs){
  levels(features[[vcat]]) <- c(levels(features[[vcat]]), "NoData")
  features[[vcat]][which(is.na(features[[vcat]]))] <- "NoData"
}

NAs <- sapply(features,function(x){(sum(is.na(x))/nrow(features))*100})
var_NAs <- NAs[NAs != 0]
var_NAs

head(features)

chart.Correlation(features[, sapply(features, is.numeric)], histogram = TRUE)

numericas<-features%>%
  select_if(is.numeric)
multi.hist(x = numericas, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = colnames(numericas))

numericas[-10] <- scale(numericas[-10])

# Normalidad: shapiro test para todas las variables numéricas continuas
norm <- sapply(numericas[-c(1,10)], function(x){shapiro.test(x)$p.value})

# Determinamos un umbral para el p-valor, si el p-valor es mayor que 0.05 no tengo evidencias para decir que la variable no sigue una distribución normal
sesgadas <- norm[norm < 0.05]

# Función normalize
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))}

# Transformamos las variables excesivamente sesgadas
for(n in names(sesgadas)){
  numericas[[n]] <- normalize(numericas[[n]])
}

multi.hist(x = numericas, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = colnames(numericas))

numericas_long <- numericas %>%
  pivot_longer(names_to = "variable",
                 values_to = "value",
                 cols = 1:32)
  
duptimes <- c(rep(ncol(numericas),132))
idx <- rep(1:nrow(Y_train), duptimes)
target <- Y_train[idx,]

numericas_long_target <- cbind(numericas_long, target)

numericas_long_target %>% 
  ggplot(aes(x=M, y=value))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(shape = ".", position = "jitter", alpha = 0.1) +
  facet_wrap(~ variable) +
  theme_minimal()

numericas_long_target %>% 
  ggplot(aes(x=MM, y=value))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(shape = ".", position = "jitter", alpha = 0.1) +
  facet_wrap(~ variable) +
  theme_minimal()

numericas_long_target %>% 
  ggplot(aes(x=Combo, y=value))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(shape = ".", position = "jitter", alpha = 0.1) +
  facet_wrap(~ variable) +
  theme_minimal()

numericas_long_target %>% 
  ggplot(aes(x=DCombo, y=value))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(shape = ".", position = "jitter", alpha = 0.1) +
  facet_wrap(~ variable) +
  theme_minimal()

# glm

numericas_target <- cbind(numericas, Y_train)

glmp <- function(model){
  p <- summary(model)$coefficients[2,4]
  return(p)}
# Realizamos un glm para cada feature con SalePrice
allModels_m <-  sapply(names(numericas), function(x){
  glm <- glm(formula = paste0("numericas_target$M ~ numericas_target$`", x, "`"), family = binomial(link = logit))
  glmp(glm)})

allModels_mm <-  sapply(names(numericas), function(x){
  glm <- glm(formula = paste0("numericas_target$MM ~ numericas_target$`", x, "`"), family = binomial(link = logit))
  glmp(glm)})

allModels_combo <-  sapply(names(numericas), function(x){
  glm <- glm(formula = paste0("numericas_target$Combo ~ numericas_target$`", x, "`"), family = binomial(link = logit))
  glmp(glm)})

allModels_dcombo <-  sapply(names(numericas), function(x){
  glm <- glm(formula = paste0("numericas_target$DCombo ~ numericas_target$`", x, "`"), family = binomial(link = logit))
  glmp(glm)})

vnum_significativas <- names(which(allModels_m < 0.2))

categoricas <- features %>%
  select_if(is.factor)

categoricas_target <- cbind(categoricas, Y_train)

# chi-cuadrado

lista_chicuadrado_pvalues_m <- list()
par(mfrow=c(3,4))
for (cat in names(categoricas)){
  tabla <- table(categoricas_target[[cat]],categoricas_target$M)
  plot(tabla, col = c("red", "blue"), main = paste0(cat," vs. Miopía"))
  lista_chicuadrado_pvalues_m[[cat]] <- chisq.test(tabla)$p.value
}
chicuadrado_pvalues_m <- lista_chicuadrado_pvalues_m

var_significativa_m <- sapply(lista_chicuadrado_pvalues_m, function(x){x<0.2})
var_significativa_m <- names(var_significativa_m)[which(var_significativa_m)]

lista_chicuadrado_pvalues_mm <- list()
par(mfrow=c(3,4))
for (cat in names(categoricas)){
  tabla <- table(categoricas_target[[cat]],categoricas_target$MM)
  plot(tabla, col = c("red", "blue"), main = paste0(cat," vs. Miopía Magna"))
  lista_chicuadrado_pvalues_mm[[cat]] <- chisq.test(tabla)$p.value
}
chicuadrado_pvalues_mm <- lista_chicuadrado_pvalues_mm

var_significativa_mm <- sapply(lista_chicuadrado_pvalues_mm, function(x){x<0.2})
var_significativa_mm <- names(var_significativa_mm)[which(var_significativa_mm)]

lista_chicuadrado_pvalues_combo <- list()
par(mfrow=c(3,4))
for (cat in names(categoricas)){
  tabla <- table(categoricas_target[[cat]],categoricas_target$Combo)
  plot(tabla, col = c("red", "blue"), main = paste0(cat," vs. Combo"))
  lista_chicuadrado_pvalues_combo[[cat]] <- chisq.test(tabla)$p.value
}
chicuadrado_pvalues_combo <- lista_chicuadrado_pvalues_combo

var_significativa_combo <- sapply(lista_chicuadrado_pvalues_combo, function(x){x<0.2})
var_significativa_combo <- names(var_significativa_combo)[which(var_significativa_combo)]

lista_chicuadrado_pvalues_dcombo <- list()
par(mfrow=c(3,4))
for (cat in names(categoricas)){
  tabla <- table(categoricas_target[[cat]],categoricas_target$DCombo)
  plot(tabla, col = c("red", "blue"), main = paste0(cat," vs. DCombo"))
  lista_chicuadrado_pvalues_dcombo[[cat]] <- chisq.test(tabla)$p.value
}
chicuadrado_pvalues_dcombo <- lista_chicuadrado_pvalues_dcombo

var_significativa_dcombo <- sapply(lista_chicuadrado_pvalues_dcombo, function(x){x<0.2})
var_significativa_dcombo <- names(var_significativa_dcombo)[which(var_significativa_dcombo)]

datos_juntos <- cbind(numericas, categoricas, Y_train)

#Creamos los Data Frames para poder seleccionar las variables para el modelo de cada variable respuesta
datos_juntos_M= select(datos_juntos, -MM, -Combo,-DCombo)
datos_juntos_MM=select(datos_juntos, -M, -Combo,-DCombo)
datos_juntos_Combo=select(datos_juntos, -M, -MM,-DCombo)
datos_juntos_DCombo=select(datos_juntos, -M, -MM,-Combo)

#Selección de las variables para predecir M
Feature_Selection_M<-Boruta(datos_juntos_M$M~.,data=datos_juntos_M,maxRuns=1000)
print(Feature_Selection_M)
Final_Feature_Selection_M<-TentativeRoughFix(Feature_Selection_M)
print(Final_Feature_Selection_M)

collist_M <- getSelectedAttributes(Final_Feature_Selection_M, withTentative = F)
collist_Mbis <- c(collist_M,"M")
features_M <- datos_juntos_M[,names(datos_juntos_M)%in%collist_Mbis]
str(features_M)

#Selección de las variables para predecir MM
Feature_Selection_MM<-Boruta(datos_juntos_MM$MM~.,data=datos_juntos_MM,maxRuns=1000)
print(Feature_Selection_MM)
Final_Feature_Selection_MM<-TentativeRoughFix(Feature_Selection_MM)
print(Final_Feature_Selection_MM)

collist_MM <- getSelectedAttributes(Final_Feature_Selection_MM, withTentative = F)
collist_MMbis <- c(collist_MM,"MM")
features_MM <- datos_juntos_MM[,names(datos_juntos_MM)%in%collist_MMbis]
str(features_MM)

#Selección de las variables para predecir Combo
Feature_Selection_Combo<-Boruta(datos_juntos_Combo$Combo~.,data=datos_juntos_Combo,maxRuns=1000)
print(Feature_Selection_Combo)
Final_Feature_Selection_Combo<-TentativeRoughFix(Feature_Selection_Combo)
print(Final_Feature_Selection_Combo)

collist_Combo <- getSelectedAttributes(Final_Feature_Selection_Combo, withTentative = F)
collist_Combobis <- c(collist_Combo,"Combo")
features_Combo <- datos_juntos_Combo[,names(datos_juntos_Combo)%in%collist_Combobis]
str(features_Combo)

#Selección de las variables para predecir DCombo
Feature_Selection_DCombo<-Boruta(datos_juntos_DCombo$DCombo~.,data=datos_juntos_DCombo,maxRuns=1000)
print(Feature_Selection_DCombo)
Final_Feature_Selection_DCombo<-TentativeRoughFix(Feature_Selection_DCombo)
print(Final_Feature_Selection_DCombo)

collist_DCombo <- getSelectedAttributes(Final_Feature_Selection_DCombo, withTentative = F)
collist_DCombobis <- c(collist_DCombo,"DCombo")
features_DCombo <- datos_juntos_DCombo[,names(datos_juntos_DCombo)%in%collist_DCombobis]
str(features_DCombo)

# Crear training/test sets (75% training y 25% test)
set.seed(1234)
#M
ind <- sample(1:nrow(datos_juntos),nrow(datos_juntos)*0.75)
train_M = features_M[ind,]
test_M = features_M[-ind,]
#MM
ind <- sample(1:nrow(datos_juntos),nrow(datos_juntos)*0.75)
train_MM = features_MM[ind,]
test_MM = features_MM[-ind,]
#Combo
ind <- sample(1:nrow(datos_juntos),nrow(datos_juntos)*0.75)
train_Combo = features_Combo[ind,]
test_Combo = features_Combo[-ind,]
#DCombo
ind <- sample(1:nrow(datos_juntos),nrow(datos_juntos)*0.75)
train_DCombo = features_DCombo[ind,]
test_DCombo = features_DCombo[-ind,]

head(train_M)
head(test_M)

## MODELOS

control1 <- trainControl(method="cv",
                         number=3,
                         search="grid",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         sampling = "up")

# Source: https://github.com/topepo/caret/issues/107
multiClassSummary <- function (data, lev = NULL, model = NULL){
  
  #Load Libraries
  require(Metrics)
  require(caret)
  
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs  <- ifelse(data[,  "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats) 
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  
  #Combine overall with class-wise stats and remove some stats we don't want 
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                       'Prevalence', 'Detection Prevalence')]
  
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  return(stats)

}

control2 <- trainControl(method="cv",
                         number=3,
                         search="grid",
                         classProbs = TRUE,
                         summaryFunction = multiClassSummary,
                         sampling = "up")

### NEURAL NETWORKS

#M
nnet_model_M <-train(M~., data = train_M, metric="ROC",
                     method = "nnet", trControl = control1,
                     maxit=100,verbose=FALSE)

# Resultados M
pred_nnet_M <- predict(nnet_model_M, test_M)
cm_nnet_M <- confusionMatrix(pred_nnet_M, test_M$M, positive="SI")
cm_nnet_M

#MM
nnet_model_MM <-train(MM~., data = train_MM, metric="ROC",
                      method = "nnet", trControl = control1,
                      maxit=100,verbose=FALSE)

# Resultados MM
pred_nnet_MM <- predict(nnet_model_MM, test_MM)
cm_nnet_MM <- confusionMatrix(pred_nnet_MM, test_MM$MM, positive = "SI")
cm_nnet_MM

#Combo
nnet_model_Combo<-train(Combo~., data = train_Combo, metric="ROC",
                        method = "nnet", trControl = control2,
                        maxit=100,verbose=FALSE)

#Resultados Combo
pred_nnet_Combo <- predict(nnet_model_Combo, test_Combo)
cm_nnet_Combo <- confusionMatrix(pred_nnet_Combo, test_Combo$Combo)
cm_nnet_Combo

#DCombo
nnet_model_DCombo <-train(DCombo~., data = train_DCombo, metric="ROC",
                          method = "nnet", trControl = control2,
                          maxit=100,verbose=FALSE)  

#Resultados DCombo
pred_nnet_DCombo <- predict(nnet_model_DCombo, test_DCombo)
cm_nnet_DCombo <- confusionMatrix(pred_nnet_DCombo, test_DCombo$DCombo)
cm_nnet_DCombo

### RANDOM FOREST

#M
model_rf_M <- train(M~.,
                    train_M,
                    method="rf",
                    metric="ROC",
                    trControl=control1)
pred_rf_M <- predict(model_rf_M, test_M)
cm_rf_M <- confusionMatrix(pred_rf_M, test_M$M, positive = "SI")
cm_rf_M

#MM
model_rf_MM <- train(MM~.,
                     train_MM,
                     method="rf",
                     metric="ROC",
                     trControl=control1)
pred_rf_MM <- predict(model_rf_MM, test_MM)
cm_rf_MM <- confusionMatrix(pred_rf_MM, test_MM$MM, positive = "SI")
cm_rf_MM

#Combo
model_rf_Combo <- train(Combo~.,
                        train_Combo,
                        method="rf",
                        metric="ROC",
                        trControl=control2)
pred_rf_Combo <- predict(model_rf_Combo, test_Combo)
cm_rf_Combo <- confusionMatrix(pred_rf_Combo, test_Combo$Combo)
cm_rf_Combo

#DCombo
model_rf_DCombo <- train(DCombo~.,
                         train_DCombo,
                         method="rf",
                         metric="ROC",
                         trControl=control2)
pred_rf_DCombo <- predict(model_rf_DCombo, test_DCombo)
cm_rf_DCombo <- confusionMatrix(pred_rf_DCombo, test_DCombo$DCombo)
cm_rf_DCombo

### KNN

#M
model_knn_M <- train(M~.,
                     train_M,
                     method="knn",
                     metric="ROC",
                     trControl=control1)
pred_knn_M <- predict(model_knn_M, test_M)
cm_knn_M <- confusionMatrix(pred_knn_M, test_M$M, positive = "SI")
cm_knn_M

#MM
model_knn_MM <- train(MM~.,
                      train_MM,
                      method="knn",
                      metric="ROC",
                      trControl=control1)
pred_knn_MM <- predict(model_knn_MM, test_MM)
cm_knn_MM <- confusionMatrix(pred_knn_MM, test_MM$MM, positive = "SI")
cm_knn_MM

#Combo
model_knn_Combo <- train(Combo~.,
                         train_Combo,
                         method="knn",
                         metric="ROC",
                         trControl=control2)
pred_knn_Combo <- predict(model_knn_Combo, test_Combo)
cm_knn_Combo <- confusionMatrix(pred_knn_Combo, test_Combo$Combo)
cm_knn_Combo

#DCombo
model_knn_DCombo <- train(DCombo~.,
                          train_DCombo,
                          method="knn",
                          metric="ROC",
                          trControl=control2)
pred_knn_DCombo <- predict(model_knn_DCombo, test_DCombo)
cm_knn_DCombo <- confusionMatrix(pred_knn_DCombo, test_DCombo$DCombo)
cm_knn_DCombo


## Comprobación

predict_function <- function(dataset){
  
  features_test <- dataset[-1]
  features_test <- as.data.frame(unclass(features_test), stringsAsFactors = TRUE)
  features_test <- features_test %>% select(-c(hº.act.cerca.sem, origen.antepasados..extranjeros., hº.act.cerca.sem, Grupo.fot, fototipo, origen.antepasados..españa.))
  NAs2 <- sapply(features_test,function(x){(sum(is.na(x))/nrow(features_test))*100})
  var_NAs2 <- NAs2[NAs2 != 0]
  
  num_NAs2 <- names(which(sapply(features_test[,which(NAs2 != 0)], is.numeric)))
  cat_NAs2 <- names(which(sapply(features_test[,which(NAs2 != 0)], is.factor)))
  
  # Imputar missing values para variables numéricas
  for (vnum2 in num_NAs2){
    features_test[[vnum2]][is.na(features_test[[vnum2]])] <- median(features_test[[vnum2]],na.rm = TRUE)
  }
  
  # Añadir categoría NoData para las variables categóricas con missing values
  for (vcat2 in cat_NAs2){
    levels(features_test[[vcat2]]) <- c(levels(features_test[[vcat2]]), "NoData")
    features_test[[vcat2]][which(is.na(features_test[[vcat2]]))] <- "NoData"
  }
  
  numericas2<-features_test%>%
    select_if(is.numeric)
  
  # Escalamos todas las variables menos Familiar.miope.num.
  numericas2[-10] <- scale(numericas2[-10])
  
  # Normalidad: shapiro test para todas las variables numéricas continuas
  norm2 <- sapply(numericas2[-c(1,10)], function(x){shapiro.test(x)$p.value})
  # Determinamos un umbral para el p-valor, si el p-valor es mayor que 0.05 no tengo evidencias para decir que la variable no sigue una distribución normal
  sesgadas2 <- norm2[norm2 < 0.05]
  # Tranformamos las variables excesivamente sesgadas
  for(n in names(sesgadas2)){
    numericas2[[n]] <- normalize(numericas2[[n]])
  }
  
  categoricas2 <- features_test %>%
    select_if(is.factor)
  
  datos_juntos2 <- cbind(numericas2, categoricas2)
  
  # A partir de datos_juntos2 y los vectores con las variables predictoras importantes para cada variables respuesta, creamos los datasets que nos permitirán predecir cada una
  features_test_M <- datos_juntos2[,names(datos_juntos2)%in%collist_M]
  features_test_MM <- datos_juntos2[,names(datos_juntos2)%in%collist_MM]
  features_test_Combo <- datos_juntos2[,names(datos_juntos2)%in%collist_Combo]
  features_test_DCombo <- datos_juntos2[,names(datos_juntos2)%in%collist_DCombo]
  
  # Usamos el modelo de rf para predecir cada variable respuesta
  yhat_M <- predict(model_rf_M, features_test_M)
  yhat_MM <- predict(model_rf_MM, features_test_MM)
  yhat_Combo <- predict(model_rf_Combo, features_test_Combo)
  yhat_DCombo <- predict(model_rf_DCombo, features_test_DCombo)
  
  # Creamos el output dataset (4 columnas con las predicciones de cada variable respuesta)
  prediccion_final <- data.frame("M" = yhat_M, "MM" = yhat_MM, "Combo" = yhat_Combo, "DCombo" = yhat_DCombo)
  return(prediccion_final)
  
}

#prediccion_final <- predict_function()
