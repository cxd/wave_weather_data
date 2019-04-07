source("lib/config.R")
source("lib/read_data.R")
source("lib/network.R")
source("lib/model_metrics.R")

conf <- loadConfig()

#data1 <- read_climate_files(conf$climateDir, "sunshine_coast_airport", conf$climateColumns)

#data2 <- read_wave_data(conf$waveData, "mooloolaba", conf$waveColumns)

#data3 <- data2 %>% aggregateWaveData()

#mergeData <- inner_join(data3, data1, by="LOCAL_DATE")

#data <- readAllSites(conf$waveData, conf$climateDir, conf$siteMapping, conf$climateColumns, conf$waveColumns)

#write.csv(data, "data/alldata_summary.csv", row.names=FALSE)

data <- read.csv("data/alldata_summary.csv", header=TRUE)

require(corrplot)


# use the climate variables as the input variables and the wave metrics as the target
# variables. However we may want to use SST as an input variable later.

inputCols <- c("Evapotranspiration_mm",
               "Rain_mm",
               "PanEvaporation_mm",
               "MaximumTemperature_C",
               "MaxRelativeHumidity_pc",
               "MinRelativeHumidity_pc",
               "Avg10mWindSpeed_m_sec",
               "SolarRadiation_MJ_sqm")
targetCols <- c("meanHs", "Hmax", "meanTz", "meanTp", "meanDirTp", "meanSST")

model <- model_dense(length(inputCols), 
                     length(targetCols), 
                     hiddenUnits=c(32), 
                     hiddenActivation="relu", 
                     outputActivation="linear") %>% 
  compile_model("nadam", "mse", 
                list(
                  "mean_squared_error",
                  "mean_absolute_error",
                  "accuracy"
                ))

summary(model)

idx <- which(data[,targetCols] <= 0)
data <- data[-idx,]


M <- data[,c("meanHs", "Hmax", "meanTz", "meanTp", "meanDirTp", "meanSST",
             "Evapotranspiration_mm",
             "Rain_mm",
             "PanEvaporation_mm",
             "MaximumTemperature_C",
             "MaxRelativeHumidity_pc",
             "MinRelativeHumidity_pc",
             "Avg10mWindSpeed_m_sec",
             "SolarRadiation_MJ_sqm")]

M <- as.matrix(M)
C <- cor(M)

corrplot(C)

X <- data[,inputCols]
Y <- data[,targetCols]

require(wavelets)
w1 <- modwt(data$Hmax, filter="d4", n.levels=3)
plot(w1)

c1 <- w1@W$W1
c2 <- w1@W$W2
c3 <- w1@W$W3

w2 <- modwt(data$meanHs, filter="d4", n.levels=3)
plot(w2)

c21 <- w2@W$W1
c22 <- w2@W$W2
c23 <- w2@W$W3

## We will add coefficients for use as lagged covariates
data$C1_Hmax <- c1
data$C2_Hmax <- c2
data$C3_Hmax <- c3
data$C1_meanHs <- c21
data$C2_meanHs <- c22
data$C3_meanHs <- c23

# Now we need to shift the coefficients up one timestep.
n <- nrow(data)
n1 <- n-1
data$C1_Hmax <- c(0,c1[1:n1])
data$C2_Hmax <- c(0,c2[1:n1])
data$C3_Hmax <- c(0,c3[1:n1])
data$C1_meanHs <- c(0,c21[1:n1])
data$C2_meanHs <- c(0,c22[1:n1])
data$C3_meanHs <- c(0,c23[1:n1])
# for the lagged series we discard the first row of data.
data2 <- data[2:n,]

# model2 will use the new lagged columns

inputCols2 <- c("Evapotranspiration_mm",
               "Rain_mm",
               "PanEvaporation_mm",
               "MaximumTemperature_C",
               "MaxRelativeHumidity_pc",
               "MinRelativeHumidity_pc",
               "Avg10mWindSpeed_m_sec",
               "SolarRadiation_MJ_sqm",
               "C1_Hmax",
               "C2_Hmax",
               "C3_Hmax",
               "C1_meanHs",
               "C2_meanHs",
               "C3_meanHs")
targetCols2 <- c("meanHs", "Hmax", "meanTz", "meanTp", "meanDirTp", "meanSST")

model2 <- model_dense(length(inputCols2), 
                     length(targetCols2), 
                     hiddenUnits=c(32), 
                     hiddenActivation="relu", 
                     outputActivation="linear") %>% 
  compile_model("nadam", "mse", 
                list(
                  "mean_squared_error",
                  "mean_absolute_error",
                  "accuracy"
                ))

summary(model2)

partitions1 <- partition_data(data)
trainX <- partitions1$trainX
trainY <- partitions1$trainY
validX <- partitions1$validX
validY <- partitions1$validY
testX <- partitions1$testX
testY <- partitions1$testY



checkpoint_path <- "checkpoints/model1.h5"
logdir <- "logs/model1"

callbacks <- list(
  callback_model_checkpoint(checkpoint_path),
  callback_tensorboard(logdir)
)

tensorboard("logs/model1")

history <- model %>% fit(
  as.matrix(trainX),
  as.matrix(trainY),
  epochs=500,
  validation_data = list(as.matrix(validX), as.matrix(validY)),
  callbacks = callbacks
) 

plot(history)

results <- model %>% evaluate(as.matrix(testX), as.matrix(testY), verbose=1)
results


predictY <- model %>% predict(as.matrix(testX))

predictY <- as.data.frame(predictY)
names(predictY) <- colnames(testY)

idx <- which(testY$Hmax >= 0)

png("obs_vs_predict.png", width=800, height=1024)
par.old <- par(mfrow=c(5,1))

plot(testY[idx,]$meanHs, col="blue", main="Mean Wave Height")
points(predictY[idx,]$meanHs, col="red")

plot(testY[idx,]$Hmax, col="blue", main="Maximum Wave Height")
points(predictY[idx,]$Hmax, col="red")

plot(testY[idx,]$meanTz, col="blue", main="Mean Zero Upcrossing Wave Period")
points(predictY[idx,]$meanTz, col="red")

plot(testY[idx,]$meanDirTp, col="blue", main="Mean Direction from true north")
points(predictY[idx,]$meanDirTp, col="red")

plot(testY[idx,]$meanSST, col="blue", main="Mean Sea Surface temperature")
points(predictY[idx,]$meanSST, col="red")

par(par.old)
dev.off()

par.old <- par(mfrow=c(2,1))
hist(testY[idx,]$Hmax)
hist(predictY[idx,]$Hmax)
par(par.old)

summary(testY)
summary(predictY)

metrics <- assess_metrics(testY, predictY, c("meanHs",
                                             "Hmax",
                                             "meanTz",
                                             "meanTp",
                                             "meanDirTp",
                                             "meanSST"))
metrics

write.csv(metrics, "metrics_model1.csv", row.names=FALSE)


partitions2 <- partition_data(data)
trainX <- partitions2$trainX
trainY <- partitions2$trainY
validX <- partitions2$validX
validY <- partitions2$validY
testX <- partitions2$testX
testY <- partitions2$testY



checkpoint_path <- "checkpoints/model2.h5"
logdir <- "logs/model2"

callbacks <- list(
  callback_model_checkpoint(checkpoint_path),
  callback_tensorboard(logdir)
)

tensorboard("logs/model1")

history <- model %>% fit(
  as.matrix(trainX),
  as.matrix(trainY),
  epochs=500,
  validation_data = list(as.matrix(validX), as.matrix(validY)),
  callbacks = callbacks
) 

plot(history)

results <- model %>% evaluate(as.matrix(testX), as.matrix(testY), verbose=1)
results


predictY <- model %>% predict(as.matrix(testX))

predictY <- as.data.frame(predictY)
names(predictY) <- colnames(testY)


png("obs_vs_predict2.png", width=800, height=1024)
par.old <- par(mfrow=c(5,1))

plot(testY[idx,]$meanHs, col="blue", main="Mean Wave Height")
points(predictY[idx,]$meanHs, col="red")

plot(testY[idx,]$Hmax, col="blue", main="Maximum Wave Height")
points(predictY[idx,]$Hmax, col="red")

plot(testY[idx,]$meanTz, col="blue", main="Mean Zero Upcrossing Wave Period")
points(predictY[idx,]$meanTz, col="red")

plot(testY[idx,]$meanDirTp, col="blue", main="Mean Direction from true north")
points(predictY[idx,]$meanDirTp, col="red")

plot(testY[idx,]$meanSST, col="blue", main="Mean Sea Surface temperature")
points(predictY[idx,]$meanSST, col="red")

par(par.old)
dev.off()


metrics2 <- assess_metrics(testY, predictY, c("meanHs",
                                             "Hmax",
                                             "meanTz",
                                             "meanTp",
                                             "meanDirTp",
                                             "meanSST"))
metrics2

write.csv(metrics2, "metrics_model2.csv", row.names=FALSE)
