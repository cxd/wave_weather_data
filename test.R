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
targetCols <- c("meanHs", "Hmax", "meanTz", "meanDirTp", "meanSST")

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


set.seed(42L)
n <- nrow(data)
pcTrain <- 0.6
pcValid <- 0.2
pcTest <- 0.2
nTrain <- nrow(data)*0.8
idx <- sample(1:nrow(data), nTrain)

inputX <- X[idx,]
testX <- X[-idx,]

inputY <- Y[idx,]
testY <- Y[-idx,]

nValid <- nrow(data)*0.2
idx <- sample(1:nrow(inputX), nValid)

trainX <- inputX[-idx,]
validX <- inputX[idx,]
trainY <- inputY[-idx,]
validY <- inputY[idx,]

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

data.frame(
  attribute=c("meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST",
             
              "meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST",
              
              "meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST",
              
              "meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST",
              
              "meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST",
              
              "meanHs",
              "Hmax",
              "meanTz",
              "meanDirTp",
              "meanSST"
              ),
  metric=c("Rsquared",
           "Rsquared",
           "Rsquared",
           "Rsquared",
           "Rsquared",
           
           "Agreement",
           "Agreement",
           "Agreement",
           "Agreement",
           "Agreement",
           
           "Efficiency",
           "Efficiency",
           "Efficiency",
           "Efficiency",
           "Efficiency",
           
           "PercentPeakDeviation",
           "PercentPeakDeviation",
           "PercentPeakDeviation",
           "PercentPeakDeviation",
           "PercentPeakDeviation",
           
           "RMSE",
           "RMSE",
           "RMSE",
           "RMSE",
           "RMSE",
           
           "MAE",
           "MAE",
           "MAE",
           "MAE",
           "MAE"
           ),
  value=c(
    rSquared(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    rSquared(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    rSquared(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    rSquared(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    rSquared(testY[idx,]$meanSST, predictY[idx,]$meanSST),
    
    agreement(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    agreement(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    agreement(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    agreement(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    agreement(testY[idx,]$meanSST, predictY[idx,]$meanSST),
    
    efficiency(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    efficiency(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    efficiency(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    efficiency(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    efficiency(testY[idx,]$meanSST, predictY[idx,]$meanSST),
    
    percentPeakDev(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    percentPeakDev(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    percentPeakDev(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    percentPeakDev(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    percentPeakDev(testY[idx,]$meanSST, predictY[idx,]$meanSST),
    
    rootMeanSquareError(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    rootMeanSquareError(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    rootMeanSquareError(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    rootMeanSquareError(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    rootMeanSquareError(testY[idx,]$meanSST, predictY[idx,]$meanSST),
    
    meanAbsoluteError(testY[idx,]$meanHs, predictY[idx,]$meanHs),
    meanAbsoluteError(testY[idx,]$Hmax, predictY[idx,]$Hmax),
    meanAbsoluteError(testY[idx,]$meanTz, predictY[idx,]$meanTz),
    meanAbsoluteError(testY[idx,]$meanDirTp, predictY[idx,]$meanDirTp),
    meanAbsoluteError(testY[idx,]$meanSST, predictY[idx,]$meanSST)
  )
)


