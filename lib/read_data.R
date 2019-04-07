require(dplyr)
require(tidyr)
source("lib/load_multi_files.R")

read_climate_files <- function(baseDir, stationName, columnNames, skip=13) {
  path <- file.path(baseDir, stationName)
  dataList <- processDir(path, skipLines=13)
  for(i in 1:length(dataList)) {
    colnames(dataList[[i]]) <- columnNames
  }
  data <- mergeFiles(dataList)
  colnames(data) <- columnNames
  idx <- which(data$Station %in% "Totals:") 
  if (length(idx) > 0) {
    data <- data[-idx,]
  }
  idx <- which(colnames(data) %in% "BLANK")
  if (length(idx) > 0) {
    data <- data[,-idx]
  }
  data$LOCAL_DATE <- as.POSIXct(strptime(data$Date, format="%d/%m/%Y"))
  data$EPOCHTIME <- as.double(data$LOCAL_DATE)
  data <- na.omit(data)
  data
}

read_wave_data <- function(baseDir, buoyName, columnNames) {
  path <- file.path(baseDir, buoyName)
  dataList <- processDir(path)
  for(i in 1:length(dataList)) {
    colnames(dataList[[i]]) <- columnNames
  }
  data <- mergeFiles(dataList)
  data$LOCAL_DATETIME <- strptime(data$DateTime, "%Y-%m-%dT%H:%M:%S")
  data$LOCAL_DATE <- strptime(strftime(data$LOCAL_DATETIME, "%Y-%m-%d"), "%Y-%m-%d")
  data$LOCAL_DATETIME <- as.POSIXct(data$LOCAL_DATETIME)
  data$LOCAL_DATE <- as.POSIXct(data$LOCAL_DATE)
  
  idx <- which(is.na(data$LOCAL_DATETIME))
  data[idx,]$LOCAL_DATETIME <- strptime(data[idx,]$DateTime, "%m/%e/%Y %H:%M")
  data[idx,]$LOCAL_DATE <- strptime(strftime(data[idx,]$LOCAL_DATETIME, "%Y-%m-%d"), "%Y-%m-%d")
  data[idx,]$LOCAL_DATETIME <- as.POSIXct(data[idx,]$LOCAL_DATETIME)
  data[idx,]$LOCAL_DATE <- as.POSIXct(data[idx,]$LOCAL_DATE)
  
  idx <- which(is.na(data$LOCAL_DATETIME))
  data[idx,]$LOCAL_DATETIME <- strptime(data[idx,]$DateTime, "%Y-%m-%d %H:%M:%S")
  data[idx,]$LOCAL_DATE <- strptime(strftime(data[idx,]$LOCAL_DATETIME, "%Y-%m-%d"), "%Y-%m-%d")
  data[idx,]$LOCAL_DATETIME <- as.POSIXct(data[idx,]$LOCAL_DATETIME)
  data[idx,]$LOCAL_DATE <- as.POSIXct(data[idx,]$LOCAL_DATE)
  
  data$EPOCHTIME <- as.double(data$LOCAL_DATETIME)
  
  data
}

## Aggregate the wave data per day.
aggregateWaveData <- function(data) {
  
  meanAboveZero <- function(series) {
    idx <- which(series > 0)
    if (length(idx) > 0) {
      mu <- mean(series[idx])
      mu  
    } else 0
  }
  
  data %>% group_by(LOCAL_DATE) %>%
    summarise(meanHs=meanAboveZero(Hs),
              Hmax=max(Hmax),
              meanTz=meanAboveZero(Tz),
              meanTp=meanAboveZero(Tp),
              meanDirTp=meanAboveZero(DirTpTRUE),
              meanSST=meanAboveZero(SST))
}


mergeWaveAndClimate <- function(waveData, climateData) {
  mergeData <- inner_join(waveData, climateData, by="LOCAL_DATE")
  mergeData
}

readAllSites <- function(waveDir, climateDir, mappings, climateColumns, waveColumns) {
  dataSet <- NA
  for(i in 1:nrow(mappings)) {
    
    print(paste("Wave Data", mappings[i,]$waveSite))
    print(paste("Climate Data", mappings[i,]$climateSite))
    
    data1 <- read_climate_files(conf$climateDir, mappings[i,]$climateSite, climateColumns)
    
    data2 <- read_wave_data(conf$waveData, mappings[i,]$waveSite, waveColumns)
    
    data3 <- data2 %>% aggregateWaveData()
    data4 <- mergeWaveAndClimate(data3, data1)
    
    
    data4$WaveSensor <- rep(mappings[i,]$waveSite, nrow(data4))
    
    if (is.na(dataSet)) {
      dataSet <- data4
    } else {
      dataSet <- rbind(dataSet, data4)
    }
  }
  dataSet$WaveSensor <- as.factor(dataSet$WaveSensor)
  dataSet
}
