set.seed(87)
setwd("~/Documentos/AA/Trabajo4")
library("caret", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("leaps", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("glmnet", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("randomForest", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("ada", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("neuralnet", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("ROCR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("kernlab", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")


#################################### Preprocesado de los datos ####################################

#Cargamos la base de datos
BCW <- read.table("Data/wdbc.data", sep=',')

#Quitamos la primera columna
BCW <- BCW[,-1]

#Guardamos las etiquetas
BCW_labels <- as.character(BCW[,1])

#Transformamos las etiquetas para que sea un problema de clasificacion binaria. Benigno=1 y Maligno=0
BCW_labels[BCW_labels=='B'] =1
BCW_labels[BCW_labels=='M'] =0
BCW_labels <- as.numeric(BCW_labels)
#Quitamos las etiquetas de nuestros datos
BCW <- BCW[,-1]

#Escogemos el conjunto de entrenamiento
train_set = sample(1:nrow(BCW), nrow(BCW)*0.7, replace = F)

#Preprocesamiento de los datos: Método de Yeo Johnson, centrado, escalado y análisis de 
#componentes principales.
PreProccesPCA = preProcess(BCW[train_set, ], thres=0.9, 
                           method = c("YeoJohnson", "center", "scale", "pca"))
#Preprocesamiento de los datos: Método de Yeo Johnson, centrado, y escalado.
PreProccesNoPCA = preProcess(BCW[train_set, ], thres=0.9,
                             method = c("YeoJohnson", "center", "scale"))


#Aplicamos la fórmula obtenida a los datos
BCW_PreProcPCA = predict(PreProccesPCA, BCW)
BCW_PreProcNoPCA = predict(PreProccesNoPCA, BCW)

#Restauramos las etiquetas en los datos para poder especificarlas como
#variable de respuesta en la regresion lineal
BCW_PreProcPCA = cbind(BCW_PreProcPCA, BCW_labels)
BCW_PreProcNoPCA = cbind(BCW_PreProcNoPCA, BCW_labels)

#################################### Modelo Lineal ####################################


ProcessClassifModel <- function(form, Data, Train_index, RVM_Index, 
                                Family, f_valley, CalcErr){
  
  #Obtenemos el modelo en base a la fórmula dada como argumento
  model = glm(as.formula(form) , data = Data, subset = Train_index, family = Family)
  #Obtenemos las predicciones para los conjuntos de train y test
  model_predict_test = predict(model, Data[-Train_index, -RVM_Index], type = "response")
  model_predict_train = predict(model, Data[Train_index, -RVM_Index], type = "response")
  #Calculamos las etiquetas en base a la predicción
  calculated_labels_test = rep(0, length(model_predict_test))
  calculated_labels_test[model_predict_test >= 0.5] = 1
  calculated_labels_train = rep(0, length(model_predict_train))
  calculated_labels_train[model_predict_train >= 0.5] = 1
  #Calculamos el error mediante la matriz de confusión
  error_test = CalcErr(calculated_labels_test, Data[-Train_index, RVM_Index])
  error_train = CalcErr(calculated_labels_train, Data[Train_index, RVM_Index])
  
  #Obtenemos la curva de ROC
  pred = prediction(model_predict_test, Data[-Train_index, RVM_Index])
  perfArea = performance(pred, "auc")
  perfCurva = performance(pred, "tpr", "fpr")
  #Devolvemos los errores y los parámetros utilizados para obtenerlos, 
  #además de la curva de ROC y el valor del área bajo ella
  list(E_train = error_train, E_test = error_test, formula = form, modelo = model, 
       ROC = perfCurva, AreaROC = perfArea@y.values, FValley = f_valley)
  
} #Fin de ProcessClassifModel

GetBestModel <- function(ResponseVarName, Data, Train_index, Method, 
                         Nvmax, ProcessModel, Family, CalcErr = calcularErrorCuadratico){
  
  if(class(ProcessModel) != "function"){
    print("Error: ProcessModel debe ser una funcion que procese un modelo")
    return(-1)
  }
  
  #Obtenemos la fórmula que incuye la variable de respuesta 
  # y las demás variables como predictores.
  RVM_Formula = paste(ResponseVarName, "~.")
  #Obtenemos la columna en la que se encuentra la variable de respuesta
  RVM_Index = which(colnames(Data) == ResponseVarName)
  #Obtenemos lo subconjuntos de mejores características 
  #dentro del conjunto de datos de entrada
  subsets = regsubsets(as.formula(RVM_Formula), data = Data[Train_index, ], 
                       method = Method, nvmax = Nvmax, really.big = T)
  
  summ = summary(subsets)
  matrix_summ = summary(subsets)$which[ , -1]
  #Obtenemos el primer valle de la lista proporcionada por C_p
  valleys = which(diff(sign(diff(summ$cp)), na.pad = FALSE) > 0)
  first_valley = ifelse(length(valleys) == 0, length(summ$cp), valleys[1] + 1)
  var_set = matrix_summ[first_valley, ]
  #Obtenemos los resultados utilizando como subconjunto de variables 
  #el indicado por el primer valle de la lista proporcionada por C_p
  no_format_set = which(var_set)
  names_list = names(no_format_set)
  form = paste(names_list, collapse = '+')
  form = paste(ResponseVarName, "~",form, collapse = '')
  #Obtenemos y procesamos el modelo mediante la función dada como argumento
  ProcessModel(form, Data, Train_index, RVM_Index, Family, first_valley, CalcErr)
}



ConfMatrixError <- function(calculated_labels, original_labels){
  #Obtenemos la matriz de confusión
  matrix_c = table(calculated_labels, original_labels)
  #Calculamos los aciertos
  p_aciertos = (matrix_c[1,1] + matrix_c[2,2])/sum(matrix_c)
  #Devolvemos los fallos
  (1 - p_aciertos)*100
}

rocplot =function (pred , truth , plot=T,...){  
  
  predob = ROCR::prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  if(plot){plot(perf ,...)}  
  as.numeric(performance(predob, "auc")@y.values)
  
}

set.seed(87)

glmPCAGaussian = GetBestModel("BCW_labels", BCW_PreProcPCA, train_set, 
                              "exhaustive", 0.8*ncol(BCW_PreProcPCA), 
                              ProcessModel = ProcessClassifModel, Family = "gaussian",
                              CalcErr = ConfMatrixError)

glmNoPCAGaussian = GetBestModel("BCW_labels", BCW_PreProcNoPCA, train_set, 
                                "exhaustive", as.integer(0.8*ncol(BCW_PreProcNoPCA)), 
                                ProcessModel = ProcessClassifModel, Family = "gaussian",
                                CalcErr = ConfMatrixError)

glmPCABinomial = GetBestModel("BCW_labels", BCW_PreProcPCA, train_set, 
                              "exhaustive", 0.8*ncol(BCW_PreProcPCA), 
                              ProcessModel = ProcessClassifModel, Family = "binomial",
                              CalcErr = ConfMatrixError)

glmNoPCABinomial = GetBestModel("BCW_labels", BCW_PreProcNoPCA, train_set, 
                                "exhaustive", as.integer(0.8*ncol(BCW_PreProcNoPCA)), 
                                ProcessModel = ProcessClassifModel, Family = "binomial",
                                CalcErr = ConfMatrixError)

print(c("Area ROC test glmPCAGaussian: ", glmPCAGaussian$AreaROC))
print(c("Area ROC test glmNoPCAGaussian: ", glmNoPCAGaussian$AreaROC))
print(c("Area ROC test glmPCABinomial: ", glmPCABinomial$AreaROC))
print(c("Area ROC test glmNoPCABinomial: ", glmNoPCABinomial$AreaROC))

par(mfrow = c(2,2))

plot(glmPCAGaussian$ROC); plot(glmNoPCAGaussian$ROC)
plot(glmPCABinomial$ROC); plot(glmNoPCABinomial$ROC)

######################################################################################

trainControl = trainControl(method = "cv", number = 10, p = 0.75)
#http://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines

#################################### Random Forest ####################################

set.seed(87)

number_trees_list = c(seq(10,100,10),seq(200,2000,100))
number_trees_list = seq(100,5000,200)

#Estimación de parámetros
RFTune = tune(randomForest, as.factor(BCW_labels)~., 
              data = BCW_PreProcNoPCA[train_set, ],
              mtry = sqrt(ncol(BCW_PreProcNoPCA)-1),  
              # xtest = BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)], 
              # ytest = as.factor(BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)]), 
              ranges = list(ntree=number_trees_list),
              importance = T, keep.forest = T,
              tunecontrol = tune.control(sampling = "cross", cross = 10))

#Obtención del modelo
RFModel = randomForest(as.factor(BCW_labels)~., 
                       data = BCW_PreProcNoPCA, 
                       ntree = as.numeric(RFTune$best.parameters[1]),
                       mtry = sqrt(ncol(BCW_PreProcNoPCA)-1),  
                       # xtest = BCW_PreProcNoPCA[, -ncol(BCW_PreProcNoPCA)], 
                       # ytest = as.factor(BCW_PreProcNoPCA[, ncol(BCW_PreProcNoPCA)]), 
                       subset = train_set, keep.forest = T, importance = T)

#Estimación de las etiquetas
RF_test_labels = predict(RFModel, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
RF_train_labels = predict(RFModel, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

#Obtención de mediciones
par(mfrow = c(1,2))
areaRocRF_test = rocplot(as.numeric(RF_test_labels), 
                         BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)])

areaRocRF_train = rocplot(as.numeric(RF_train_labels), 
                         BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)])

print(c("Area ROC test RF: ", areaRocRF_test))
print(c("Area ROC train RF: ", areaRocRF_train))
print(c("Numero de arboles utilizados: ", as.numeric(RFTune$best.parameters[1])))

#################################### Support Vector Machine ####################################
set.seed(87)

SVMTrainGrid = expand.grid(c(0.1,1,10,100,1000), seq(0,1,0.01))
colnames(SVMTrainGrid) = c("C", "sigma")

SVMTrain = train( x=data.frame(BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)]),
                  y=as.factor(BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)]),
                  method="svmRadialSigma",
                  tuneGrid = data.frame(SVMTrainGrid),
                  trControl = trainControl)

#Obtención del modelo
SVMModel = svm(as.factor(BCW_labels)~.,
                data = BCW_PreProcNoPCA[train_set, ],
                gamma = as.numeric(SVMTrain$bestTune[1]),
                cost = as.numeric(SVMTrain$bestTune[2]),
                kernel = "radial")

#Estimación de las etiquetas
SVM_test_labels = predict(SVMModel, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
SVM_train_labels = predict(SVMModel, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

#Obtención de mediciones
par(mfrow = c(1,2))
areaRocSVM_test = rocplot(as.numeric(SVM_test_labels), 
                          BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)])

areaRocSVM_train = rocplot(as.numeric(SVM_train_labels), 
                           BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)])

print(c("Area ROC test SVM: ", areaRocSVM_test))
print(c("Area ROC train SVM: ", areaRocSVM_train))
print(c("Coste missclass SVM: ", as.numeric(SVMTrain$bestTune[2])))
print(c("Gamma SVM: ", as.numeric(SVMTrain$bestTune[1])))

#################################### Boosting ####################################

set.seed(87)

#Estimación de parámetros
SeqBy50 = seq(0,500,50); SeqBy50[1] = 1
BoostTrainGrid = matrix(c(SeqBy50, rep(1,length(SeqBy50)), rep(1,length(SeqBy50))), 
                        nrow = length(SeqBy50), ncol = 3)
colnames(BoostTrainGrid) = c("iter", "maxdepth","nu")

BoostTrain = train(x=data.frame(BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)]),
                   y=as.factor(BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)]),
                   method="ada",
                   tuneGrid = data.frame(BoostTrainGrid),
                   trControl = trainControl,
                   control = rpart.control(maxdepth=1, cp=-1, minsplit=0, xval=0), type = "real")

#Obtención del modelo
BoostModel = ada(as.factor(BCW_labels)~.,
                  data = BCW_PreProcNoPCA[train_set,],
                  iter = as.numeric(BoostTrain$bestTune[1]),
                  control = rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0),
                  type = "real")

#Estimación de las etiquetas
Boost_test_labels = predict(BoostModel, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
Boost_train_labels = predict(BoostModel, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

#Obtención de mediciones
par(mfrow = c(1,2))
areaRocBoost_train = rocplot(as.numeric(Boost_train_labels), 
                             BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)])

areaRocBoost_test = rocplot(as.numeric(Boost_test_labels), 
                            BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)])

print(c("Area ROC test Boostin: ", areaRocBoost_test))
print(c("Area ROC train Boostin: ", areaRocBoost_train))
print(c("Numero iteraciones Boostin: ", as.numeric(BoostTrain$bestTune[1])))


################################### Redes Neuronales ##############################################

set.seed(87)

SeqBy5 = seq(0,50,5); SeqBy5[1] = 1
SeqBy10 = seq(0,50,10); SeqBy10[1] = 1
hiddens1 = matrix(data = c(1:50, rep(0,50), rep(0,50)), nrow = 50, ncol = 3)
hiddens2 = expand.grid(SeqBy5, SeqBy5); hiddens2 = cbind(hiddens2, rep(0,121))
hiddens3 = expand.grid(SeqBy10, SeqBy10, SeqBy10)

colnames(hiddens1) = c("layer1", "layer2", "layer3")
colnames(hiddens2) = c("layer1", "layer2", "layer3")
colnames(hiddens3) = c("layer1", "layer2", "layer3")

trainNN1 = train(x=data.frame(BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)]),
                    y = BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)],
                    method="neuralnet",
                    tuneGrid = data.frame(hiddens1), 
                    trControl = trainControl,
                    linear.output = FALSE)

trainNN2 = train(x=data.frame(BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)]),
                     y = BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)],
                     method="neuralnet",
                     tuneGrid = data.frame(hiddens2), 
                     trControl = trainControl,
                     linear.output = FALSE)

trainNN3 = train(x=data.frame(BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)]),
                     y = BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)],
                     method="neuralnet",
                     tuneGrid = data.frame(hiddens3), 
                     trControl = trainControl,
                     linear.output = FALSE)

form = paste(colnames(BCW_PreProcNoPCA)[-ncol(BCW_PreProcNoPCA)], collapse = '+')
form = as.formula(paste("BCW_labels", "~",form, collapse = ''))

NNModel1 = neuralnet(as.formula(form),data=BCW_PreProcNoPCA[train_set,],
                         hidden = as.numeric(trainNN1$bestTune[1]),
                         linear.output = FALSE)

NNModel2 = neuralnet(as.formula(form),data=BCW_PreProcNoPCA[train_set,],
                        hidden = c(trainNN2$bestTune[1], trainNN2$bestTune[2]),
                        linear.output = FALSE)

NNModel3 = neuralnet(as.formula(form),data=BCW_PreProcNoPCA[train_set,],
                        hidden = as.vector(trainNN3$bestTune), 
                        linear.output = FALSE)

#Estimación de las etiquetas1
Neural_test_labels1 = compute(NNModel1, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
Neural_train_labels1 = compute(NNModel1, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

calculated_labels_test1 = rep(0, length(Neural_test_labels1$net.result))
calculated_labels_test1[Neural_test_labels1$net.result >= 0.5] = 1
calculated_labels_train1 = rep(0, length(Neural_train_labels1$net.result))
calculated_labels_train1[Neural_train_labels1$net.result >= 0.5] = 1

#Obtención de mediciones1
par(mfrow = c(1,2))
areaRocNeural_train1 = rocplot(calculated_labels_train1,
                             BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)], F)

areaRocNeural_test1 = rocplot(calculated_labels_test1,
                            BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)], F)

#Estimación de las etiquetas2
Neural_test_labels2 = compute(NNModel2, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
Neural_train_labels2 = compute(NNModel2, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

calculated_labels_test2 = rep(0, length(Neural_test_labels2$net.result))
calculated_labels_test2[Neural_test_labels2$net.result >= 0.5] = 1
calculated_labels_train2 = rep(0, length(Neural_train_labels2$net.result))
calculated_labels_train2[Neural_train_labels2$net.result >= 0.5] = 1

#Obtención de mediciones2
par(mfrow = c(1,2))
areaRocNeural_train2 = rocplot(calculated_labels_train2,
                               BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)], F)

areaRocNeural_test2 = rocplot(calculated_labels_test2,
                              BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)], F)

#Estimación de las etiquetas3
Neural_test_labels3 = compute(NNModel3, BCW_PreProcNoPCA[-train_set, -ncol(BCW_PreProcNoPCA)])
Neural_train_labels3 = compute(NNModel3, BCW_PreProcNoPCA[train_set, -ncol(BCW_PreProcNoPCA)])

calculated_labels_test3 = rep(0, length(Neural_test_labels3$net.result))
calculated_labels_test3[Neural_test_labels3$net.result >= 0.5] = 1
calculated_labels_train3 = rep(0, length(Neural_train_labels3$net.result))
calculated_labels_train3[Neural_train_labels3$net.result >= 0.5] = 1

#Obtención de mediciones1
par(mfrow = c(1,2))
areaRocNeural_train3 = rocplot(calculated_labels_train3,
                               BCW_PreProcNoPCA[train_set, ncol(BCW_PreProcNoPCA)], F)

areaRocNeural_test3 = rocplot(calculated_labels_test3,
                              BCW_PreProcNoPCA[-train_set, ncol(BCW_PreProcNoPCA)], F)

print(c("Area ROC test Neural 1: ", areaRocNeural_test1))
print(c("Area ROC train Neural 1: ", areaRocNeural_train1))

print(c("Area ROC test Neural 2: ", areaRocNeural_test2))
print(c("Area ROC train Neural 2: ", areaRocNeural_train2))

print(c("Area ROC test Neural 3: ", areaRocNeural_test3))
print(c("Area ROC train Neural 3: ", areaRocNeural_train3))
