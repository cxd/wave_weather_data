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



