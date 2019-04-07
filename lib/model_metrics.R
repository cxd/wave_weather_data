
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

