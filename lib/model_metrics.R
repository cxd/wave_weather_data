
## Calculate Rsquared for observations vs simulated data.
rSquared <- function(obs, sim) {
  muO <- mean(obs)
  muS <- mean(sim)
  deltaO <- obs - muO
  deltaS <- sim - muS
  ssO2 <- t(deltaO)%*%deltaO
  ssS2 <- t(deltaS)%*%deltaS
  prod <- t(deltaO)%*%deltaS
  r <- prod/(sqrt(ssO2*ssS2))
  R2 <- r*r
  R2
}

# Wilmotts index of agreement
agreement <- function(obs, sim) {
  muO <- mean(obs)
  delta <- obs - sim
  MSE <- t(delta)%*% delta
  deltaS <- abs(sim - muO)
  deltaO <- abs(obs - muO)
  total <- deltaS + deltaO
  PE <- t(total)%*%total
  n <- length(obs)
  d <- 1.0 - (MSE/PE)
  d
}

## Nash Sutcliffe Efficiency
efficiency <- function(obs, sim) {
  muO <- mean(obs)
  delta <- obs - sim
  MSE <- t(delta)%*%delta
  deltaO <- obs - muO
  varO <- t(deltaO)%*%deltaO
  p <- MSE/varO
  E <- 1.0 - p
  E
}

percentPeakDev <- function(obs, sim) {
  maxObs <- max(obs)
  maxSim <- max(sim)
  pdv <- 100*(maxSim - maxObs)/maxObs
  pdv
}

rootMeanSquareError <- function(obs, sim) {
  n <- length(obs)
  delta <- sim - obs
  ss <- t(delta)%*%delta
  rmse <- sqrt(1/n * ss)
  rmse
}

meanAbsoluteError <- function(obs, sim) {
  n <- length(obs)
  delta <- abs(sim - obs)
  mae <- 1/n * sum(delta)
}

assess_metrics <- function(obsData, simData, columnNames) {
  obs <- obsData[,columnNames]
  sim <- simData[,columnNames]
  # there are 5 x length(columnNames) tests
  results <- NA
  for(i in 1:length(columnNames)) {
    obs1 <- obs[,columnNames[i]]
    sim1 <- sim[,columnNames[i]]
    result <- data.frame(
      attribute=rep(columnNames[i], 6),
      metric=c("RSquared",
               "Agreement",
               "Efficiency",
               "PercentPeakDeviation",
               "RMSE",
               "MAE"),
      value=c(
        rSquared(obs1, sim1),
        agreement(obs1, sim1),
        efficiency(obs1, sim1),
        percentPeakDev(obs1, sim1),
        rootMeanSquareError(obs1, sim1),
        meanAbsoluteError(obs1, sim1)
      )
    )
    if (is.na(results)) {
      results <- result
    } else {
      results <- rbind(results, result)
    }
  }
  results
}

