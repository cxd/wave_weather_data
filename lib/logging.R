

openLogging <- function(logDir) {
  #Create and open log file
  #Get current system time
  execTime <- format(Sys.time(), "%Y%m%d_%H-%M-%S")
  
  #Title format: log_ + date period processed in YMD + _exec_ + time executed in YMD_H-M-S + .txt
  logFile <- file(paste0(logDir, "/log_","exec_", execTime, ".txt"))
  
  sink(file = logFile)
  sink(file = logFile, type = "message")
  
  logFile
}


closeLogging <- function(logFile) {
  #Close log connection and log file
  sink(type = "message")
  sink()
  close(logFile)
  
}