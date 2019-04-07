library(keras)

# Define a simple dense model.
model_dense <- function(inputSize, outputSize, hiddenUnits=c(64), hiddenActivation="relu", outputActivation="linear") {
  model <- keras_model_sequential()
  
  model %>% layer_dense(units=c(inputSize), input_shape = c(inputSize), activation="linear") 
  
  for(i in 1:length(hiddenUnits)) {
    model %>% layer_dense(units=hiddenUnits[i], activation=hiddenActivation)
  }
  model %>% layer_dense(units= outputSize, activation=outputActivation)
}

# Compile the model
compile_model <- function(model, optimizer, loss, metrics) {
  model %>% compile(
    optimizer=optimizer,
    loss = loss,
    metrics = metrics
  )
}

partition_data <- function(data, trainPercent=0.6, validPercent=0.2, testPercent=0.2) {
  
  set.seed(42L)
  n <- nrow(data)
  
  nTrain <- nrow(data)*trainPercent
  idx <- sample(1:nrow(data), nTrain)
  
  inputX <- X[idx,]
  testX <- X[-idx,]
  
  inputY <- Y[idx,]
  testY <- Y[-idx,]
  
  nValid <- nrow(data)*validPercent
  idx <- sample(1:nrow(inputX), nValid)
  
  trainX <- inputX[-idx,]
  validX <- inputX[idx,]
  trainY <- inputY[-idx,]
  validY <- inputY[idx,]
  
  list(
    trainX=trainX,
    trainY=trainY,
    validX=validX,
    validY=validY,
    testX=testX,
    testY=testY
  )
}





