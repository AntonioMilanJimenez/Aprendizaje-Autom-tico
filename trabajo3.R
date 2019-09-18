## ---- fig.height=3, fig.width=6, fig.align='center', echo=FALSE----------
set.seed(3)

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
#Cargamos la base de datos
Spam <- read.table("EMailSpam/spam.data")
#Cargamos los indicadores del conjunto de datos
TrainTest <- read.table("EMailSpam/spam.traintest")
#Obtenemos las etiquetas de la base de datos, que se encuentran en la última columna.
Spam_Labels = Spam[ ,ncol(Spam)]
#Retiramos las etiquetas del conjunto de datos para el preprocesamiento
Spam = Spam[, -ncol(Spam)]
#Obtenemos los índices correspondientes al conjunto de entrenamiento
train_set = c(1:nrow(Spam))
train_set = train_set[TrainTest == 0]

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
#Preprocesamiento de los datos: Método de Yeo Johnson, centrado, escalado y análisis de 
#componentes principales.
PreProccesPCA = preProcess(Spam[train_set, ], thres=0.9, 
                           method = c("YeoJohnson", "center", "scale", "pca"))
#Preprocesamiento de los datos: Método de Yeo Johnson, centrado, y escalado.
PreProccesNoPCA = preProcess(Spam[train_set, ], thres=0.9,
                             method = c("YeoJohnson", "center", "scale"))

#Aplicamos la fórmula obtenida a los datos
Spam_PreProcPCA = predict(PreProccesPCA, Spam)
Spam_PreProcNoPCA = predict(PreProccesNoPCA, Spam)

#Restauramos las etiquetas en los datos para poder especificarlas como
#variable de respuesta en la regresion lineal
Spam_PreProcPCA = cbind(Spam_PreProcPCA, Spam_Labels)
Spam_PreProcNoPCA = cbind(Spam_PreProcNoPCA, Spam_Labels)

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------

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


## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
ConfMatrixError <- function(calculated_labels, original_labels){
  #Obtenemos la matriz de confusión
  matrix_c = table(calculated_labels, original_labels)
  #Calculamos los aciertos
  p_aciertos = (matrix_c[1,1] + matrix_c[2,2])/sum(matrix_c)
  #Devolvemos los fallos
  (1 - p_aciertos)*100
}

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
glmPCAGaussian = GetBestModel("Spam_Labels", Spam_PreProcPCA, train_set, 
                            "exhaustive", 0.6*ncol(Spam_PreProcPCA), 
                            ProcessModel = ProcessClassifModel, Family = "gaussian",
                            CalcErr = ConfMatrixError)

glmNoPCAGaussian = GetBestModel("Spam_Labels", Spam_PreProcNoPCA, train_set, 
                              "forward", as.integer(0.6*ncol(Spam_PreProcNoPCA)), 
                              ProcessModel = ProcessClassifModel, Family = "gaussian",
                              CalcErr = ConfMatrixError)

glmPCABinomial = GetBestModel("Spam_Labels", Spam_PreProcPCA, train_set, 
                             "exhaustive", 0.6*ncol(Spam_PreProcPCA), 
                             ProcessModel = ProcessClassifModel, Family = "binomial",
                             CalcErr = ConfMatrixError)

glmNoPCABinomial = GetBestModel("Spam_Labels", Spam_PreProcNoPCA, train_set, 
                               "forward", as.integer(0.6*ncol(Spam_PreProcNoPCA)), 
                               ProcessModel = ProcessClassifModel, Family = "binomial",
                               CalcErr = ConfMatrixError)

## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------

plot(glmPCAGaussian$ROC, col = "blue", lwd = 1)
plot(glmNoPCAGaussian$ROC, col = "red", lwd = 1, add = T)
plot(glmPCABinomial$ROC, col = "green", lwd = 1, add = T)
plot(glmNoPCABinomial$ROC, col = "orange", lwd = 1, add = T)
legend("bottomright",col = c("blue", "red", "green", "orange"), lwd = 2, bty = "n", 
       c("PCA Gaussian","No PCA Gaussian", "PCA Binomial", "No PCA Binomial"))


## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------
#Obtenemos la matriz y las etiquetas asociadas al conjunto de datos
WD_DataMatrix = model.matrix(Spam_Labels ~., Spam_PreProcNoPCA)
labels = Spam_PreProcNoPCA[ ,ncol(Spam_PreProcNoPCA)]

#Obtenemos la estimación de lambda (especificamos alpha = 0 para aplicar Weight Decay)
WDCrossValLamb = cv.glmnet(WD_DataMatrix[train_set, ], labels[train_set], alpha = 0)
WD_best_lambda = WDCrossValLamb$lambda.min

## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------
#Obtenemos el modelo (especificamos alpha = 0 para aplicar Weight Decay)
WeightDecayModel = glmnet(WD_DataMatrix[train_set, ], labels[train_set], 
                          alpha = 0, lambda = WD_best_lambda, standardize = F)
#Pedecimos las etiquetas del conjunto de test
test_prediction = predict(WeightDecayModel, WD_DataMatrix[-(train_set), ])
calculated_labels_test = rep(0, length(test_prediction))
calculated_labels_test[test_prediction >= 0.5] = 1

#Pedecimos las etiquetas del conjunto de train
train_prediction = predict(WeightDecayModel, WD_DataMatrix[train_set, ])
calculated_labels_train = rep(0, length(train_prediction))
calculated_labels_train[train_prediction >= 0.5] = 1

WD_Error_test = ConfMatrixError(calculated_labels_test, labels[-(train_set)])
WD_Error_train = ConfMatrixError(calculated_labels_train, labels[train_set])

print(c("Error_test obtenido con Weight Decay: ", WD_Error_test))
print(c("Error_train obtenido Weight Decay: ", WD_Error_train))

## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------
#Obtenemos la matriz y las etiquetas asociadas al conjunto de datos
LSS_DataMatrix = model.matrix(Spam_Labels ~., Spam_PreProcNoPCA)
labels = Spam_PreProcNoPCA[ ,ncol(Spam_PreProcNoPCA)]

#Obtenemos la estimación de lambda (especificamos alpha = 1 para aplicar Lasso)
LassoCrossValLamb = cv.glmnet(LSS_DataMatrix[train_set, ], labels[train_set], alpha = 1)
LSS_best_lambda = LassoCrossValLamb$lambda.min

## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------
#Obtenemos el modelo (especificamos alpha = 0 para aplicar Weight Decay)
LassoModel = glmnet(LSS_DataMatrix[train_set, ], labels[train_set], 
                          alpha = 1, lambda = LSS_best_lambda, standardize = F)
#Pedecimos las etiquetas del conjunto de test
test_prediction = predict(LassoModel, LSS_DataMatrix[-(train_set), ])
calculated_labels_test = rep(0, length(test_prediction))
calculated_labels_test[test_prediction >= 0.5] = 1

#Pedecimos las etiquetas del conjunto de train
train_prediction = predict(LassoModel, LSS_DataMatrix[train_set, ])
calculated_labels_train = rep(0, length(train_prediction))
calculated_labels_train[train_prediction >= 0.5] = 1

LSS_Error_test = ConfMatrixError(calculated_labels_test, labels[-(train_set)])
LSS_Error_train = ConfMatrixError(calculated_labels_train, labels[train_set])

print(c("Error_test obtenido con Lasso: ", LSS_Error_test))
print(c("Error_train obtenido con Lasso: ", LSS_Error_train))

## ---- fig.height=4, fig.width=6, fig.align='center', echo=TRUE-----------
weight.coeff = predict(WeightDecayModel, type = "coefficients")[1:ncol(WD_DataMatrix), ]
lasso.coeff = predict(LassoModel, type = "coefficients")[1:ncol(LSS_DataMatrix), ]

print("Características no consideradas por lasso:")
print(lasso.coeff[lasso.coeff == 0])

print("Características no consideradas por weigth decay:")
print(weight.coeff[weight.coeff == 0])

## ---- echo=TRUE----------------------------------------------------------
#Calculamos los parámetros
N_test = length(train_set)
N_train = nrow(Spam_PreProcPCA) - length(train_set)
d_vc = ncol(Spam_PreProcPCA)+1; delta = 0.05
#Calculamos la cota basada en E_in
BasadaE_in = glmPCABinomial$E_train + sqrt(8/N_test*log((4*(2*N_test)^(d_vc) + 1)/delta))
print(c("Cota basada en E_in: ", BasadaE_in))
#Calculamos la cota basada en E_test
epsilon = sqrt((log(2) - log(delta))/(N_train))
BasadaE_test = glmPCABinomial$E_test + epsilon
print(c("Cota basada en E_test: ", BasadaE_test))


## ---- fig.height=3, fig.width=6, fig.align='center', echo=FALSE----------
set.seed(3)

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
Ozone <- read.table("LAOzone/LAozone.data", sep = ',', header = T)

train_set = sample(1:nrow(Ozone), nrow(Ozone)*0.7, replace = F)
#Obtenemos las etiquetas del conjunto de datos
Ozone_Labels = Ozone[ ,1]
#Obtenemos las etiquetas transformadas según el logaritmo
Ozone_Labels_Log = apply(as.array(Ozone_Labels), MARGIN = 1, FUN =  log)
#Obtenemos las etiquetas transformadas según la raiz cuadrada
Ozone_Labels_Sqrt = apply(as.array(Ozone_Labels), MARGIN = 1, FUN =  sqrt)

#Eliminamos las etiquetas del conjunto de datos para el preprocesamiento
Ozone = Ozone[, -1]

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
#Preparacion de los datos: Método de Yeo Johnson, centrado, escalado y análisis de 
#componentes principales
PreProccesPCA = preProcess(Ozone[train_set, ], thres=0.9,
                           method = c("YeoJohnson", "center", "scale", "pca"))

PreProccesNoPCA = preProcess(Ozone[train_set, ], thres=0.9,
                           method = c("YeoJohnson", "center", "scale"))

#Aplicamos la fórmula obtenida a los datos
Ozone_PreProcPCA = predict(PreProccesPCA, Ozone)
Ozone_PreProcNoPCA = predict(PreProccesNoPCA, Ozone)


#Aplicamos una transformación polinómica de grado 2 a los datos
Ozone_PreProcPCAPoly2 = data.matrix(Ozone_PreProcPCA)
Ozone_PreProcPCAPoly2 = poly(Ozone_PreProcPCAPoly2, degree = 2)

#Restauramos las etiquetas en los datos para poder especificarlas como
#variable de respuesta en la regresion lineal
Ozone_PreProcPCALog = cbind(Ozone_PreProcPCA, Ozone_Labels_Log)
Ozone_PreProcPCASqrt = cbind(Ozone_PreProcPCA, Ozone_Labels_Sqrt)
Ozone_PreProcPCA = cbind(Ozone_PreProcPCA, Ozone_Labels)
Ozone_PreProcNoPCA = cbind(Ozone_PreProcNoPCA, Ozone_Labels)
Ozone_PreProcPCAPoly2Log = data.frame(cbind(Ozone_PreProcPCAPoly2, Ozone_Labels_Log))
Ozone_PreProcPCAPoly2Sqrt = data.frame(cbind(Ozone_PreProcPCAPoly2, Ozone_Labels_Sqrt))
Ozone_PreProcPCAPoly2 = data.frame(cbind(Ozone_PreProcPCAPoly2, Ozone_Labels))

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------

ProcessRegModel <- function(form, Data, Train_index, RVM_Index, Family, 
                            f_valley, CalcErr = calcularErrorCuadratico){
  
  model = glm(as.formula(form) , data = Data, subset = Train_index, family = Family)
  
  calculated_labels_test = predict(model, Data[-Train_index, -RVM_Index], 
                                   type = "response")
  
  calculated_labels_train = predict(model, Data[Train_index, -RVM_Index], 
                                    type = "response")
  
  error_test = CalcErr(calculated_labels_test, Data[-Train_index, RVM_Index])
  error_train = CalcErr(calculated_labels_train, Data[Train_index, RVM_Index])
  results = list(E_train = error_train, E_test = error_test, 
                 formula = form, modelo = model, FValley = f_valley)
  
}


## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
calcularErrorCuadratico <- function(calculadas, originales){
  
  diferencia = calculadas-originales
  diferencia = diferencia^2
  (sum(diferencia)/length(diferencia))
  
}

## ---- fig.height=3, fig.width=6, fig.align='center', echo=TRUE-----------
glmPCAGaussian = GetBestModel("Ozone_Labels", Ozone_PreProcPCA, train_set, 
                              "exhaustive", 0.6*ncol(Ozone_PreProcPCA), 
                              ProcessModel = ProcessRegModel, Family = "gaussian")

glmNoPCAGaussian = GetBestModel("Ozone_Labels", Ozone_PreProcNoPCA, train_set, 
                                "exhaustive", 0.6*ncol(Ozone_PreProcNoPCA), 
                                ProcessModel = ProcessRegModel, Family = "gaussian")

glmPCAGaussianPoly2 = GetBestModel("Ozone_Labels", Ozone_PreProcPCAPoly2, train_set, 
                                    "forward", as.numeric(ncol(Ozone_PreProcPCAPoly2)), 
                                    ProcessModel = ProcessRegModel, Family = "gaussian")

## ---- fig.height=5.5, fig.width=6, fig.align='center', echo=TRUE---------
par(mfrow = c(2,2))
plot(glmPCAGaussian$modelo, which = c(1))
plot(glmNoPCAGaussian$modelo, which = c(1))
plot(glmPCAGaussianPoly2$modelo, which = c(1))

## ---- fig.height=6, fig.width=6, fig.align='center', echo=TRUE-----------
#Deshace la transformación logarítmica en el cálculo del error
calcularErrorCuadraticoLog <- function(calculadas, originales){
  originales = exp(originales); calculadas = exp(calculadas)
  diferencia = calculadas-originales
  diferencia = diferencia^2
  (sum(diferencia)/length(diferencia))
  
}

#Deshace la transformación raiz cuadrada en el cálculo del error
calcularErrorCuadraticoSqrt <- function(calculadas, originales){
  originales = originales^2; calculadas = calculadas^2
  diferencia = calculadas-originales
  diferencia = diferencia^2
  (sum(diferencia)/length(diferencia))
  
}

## ---- fig.height=6, fig.width=6, fig.align='center', echo=TRUE-----------
glmPCAGaussianLog = GetBestModel("Ozone_Labels_Log", Ozone_PreProcPCALog, train_set, 
                                    "exhaustive", ncol(Ozone_PreProcPCALog), 
                                    ProcessModel = ProcessRegModel, Family = "gaussian",
                                    CalcErr = calcularErrorCuadraticoLog)

glmPCAGaussianSqrt = GetBestModel("Ozone_Labels_Sqrt", Ozone_PreProcPCASqrt, train_set, 
                                   "exhaustive", ncol(Ozone_PreProcPCASqrt), 
                                   ProcessModel = ProcessRegModel, Family = "gaussian",
                                   CalcErr = calcularErrorCuadraticoSqrt)

glmPCAGaussianPolyLog = GetBestModel("Ozone_Labels_Log", Ozone_PreProcPCAPoly2Log, 
                                   train_set, "forward", ncol(Ozone_PreProcPCAPoly2Log), 
                                   ProcessModel = ProcessRegModel, Family = "gaussian",
                                   CalcErr = calcularErrorCuadraticoLog)

glmPCAGaussianPolySqrt = GetBestModel("Ozone_Labels_Sqrt", Ozone_PreProcPCAPoly2Sqrt, 
                                   train_set, "forward", ncol(Ozone_PreProcPCAPoly2Sqrt), 
                                   ProcessModel = ProcessRegModel, Family = "gaussian",
                                   CalcErr = calcularErrorCuadraticoSqrt)

## ---- fig.height=6, fig.width=6, fig.align='center', echo=TRUE-----------
par(mfrow = c(2,2))
plot(glmPCAGaussianLog$modelo, which = c(1))
plot(glmPCAGaussianSqrt$modelo, which = c(1))
plot(glmPCAGaussianPolyLog$modelo, which = c(1))
plot(glmPCAGaussianPolySqrt$modelo, which = c(1))

## ---- echo=TRUE----------------------------------------------------------
#Calculamos los parámetros
N_test = length(train_set)
N_train = nrow(Ozone_PreProcPCAPoly2Sqrt) - length(train_set)
d_vc = ncol(Ozone_PreProcPCAPoly2Sqrt)+1; delta = 0.05
#Calculamos la cota basada en E_in
BasadaE_in = glmPCAGaussianPolySqrt$E_train + sqrt(8/N_test*log((4*(2*N_test)^(d_vc) + 1)/delta))
print(c("Cota basada en E_in: ", BasadaE_in))
#Calculamos la cota basada en E_test
epsilon = sqrt((log(2) - log(delta))/(N_train))
BasadaE_test = glmPCAGaussianPolySqrt$E_test + epsilon
print(c("Cota basada en E_test: ", BasadaE_test))

