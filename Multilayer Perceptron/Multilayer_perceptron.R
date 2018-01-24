#install.packages("RSNNS")
library(RSNNS)

#fijar semilla para poder reproducir experimentos
set.seed(1)

#Leer ficheros de los tres ficheros (cuidado con el formato)

trainSet <- read.csv("Train.csv",dec=".",sep=",",header = F)
validSet <- read.csv( "Validacion.csv",dec=".",sep=",",header = F)
testSet  <- read.csv("Test.csv",dec=".",sep=",",header = F)


#num de columna de salida
target <- ncol(trainSet)

#SELECCION DE LOS PARAMETROS DE LA RED
topologia        <- c(50)   #una capa oculta de 50 neuronas. Más capas ej: c(20,10) 
razonAprendizaje <- 0.2
ciclosMaximos    <- 1000

#EJECUCION DEL APRENDIZAJE Y GENERACION DEL MODELO
# en Rsnns se llama test a nuestro fichero de validación
model <- mlp(x= trainSet[,-target],
             y= trainSet[, target],
             inputsTest=  validSet[,-target],
             targetsTest= validSet[, target],
             size= topologia,
             maxit=ciclosMaximos,
             learnFuncParams=c(razonAprendizaje),
             shufflePatterns = F
             )

#GRAFICO DE LA EVOLUCION DEL ERROR 
plotIterativeError(model)

#FUNCION que calcula el error cuadrático medio MSE
MSE <- function(pred,obs) sum((pred - obs)^2) / length(obs)

#VECTOR DE LOS ERRORES
errors <- data.frame(TrainRMSE= MSE(pred= predict(model,trainSet[,-target]),obs= trainSet[,target]),
            ValidRMSE= MSE(pred= predict(model,validSet[,-target]),obs= validSet[,target]),
            TestRMSE=  MSE(pred= predict(model, testSet[,-target]),obs=  testSet[,target]))

#TABLA CON LOS ERRORES POR CICLO
iterativeErrors <- data.frame(MSETrain= (model$IterativeFitError/ nrow(trainSet)),
                              MSEValid= (model$IterativeTestError/nrow(validSet)))

#SALIDAS DE LA RED
outputs <- list(train=   c(predict(model,trainSet[,-target])),
                      valid= c(predict(model,validSet[,-target])),
                      test=  c(predict(model, testSet[,-target])))
                

#GUARDAR RESULTADOS
saveRDS(model,"nnet.rds")   ## para leeer usar readRDS(model, ")
write.csv(errors,"finalErrors.csv",row.names=F)
write.csv(iterativeErrors,"iterativeErrors.csv")
write.csv(outputs$train,"trainOutputs.csv")
write.csv(outputs$valid,"validOutputs.csv")
write.csv(outputs$test,"testOutputs.csv")

# #plot de la salida de test
# x=1:nrow(testSet)
# plot(x,outputs$test,type="b",col="red",main="pred (red) vs obs (blue)")
# lines(x,testSet[,target],type="b",col="blue")


