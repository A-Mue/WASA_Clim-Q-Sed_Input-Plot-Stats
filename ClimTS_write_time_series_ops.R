#' writes WASA input file H
#' @param long is a long data frame object whose points are the centroids of the subbasins
#' @param address is the path where the file should be written
#' @export
long2WASA_H <- function(long,address)
{
  try(system(paste0("mkdir ",address)))
  try(system(paste0("rm ",address,"/humidity.dat")))
  fileConn <- file(paste0(address,"/humidity.dat"),"a")
  cat("Daily average humidity [%]","Date\tNo. of days\tSubbasin-ID.", file = fileConn, sep = "\n")
  
  dfObj1 <- select(long,date) %>%
    distinct(date) %>%
    mutate(str=strftime(date,"%d%m%Y"),index=row_number()) %>%
    select(-date)
  
  dfObj2 <- select(long,location,date,value) %>%
    mutate(value=round(value)) %>%
    spread(location,value) %>%
    select(-date)
  dfObj <- bind_cols(dfObj1,dfObj2)
  
  HEADER <- c("0","0",colnames(dfObj2))
  cat(HEADER, file = fileConn, sep = "\t")
  cat("\n",file = fileConn, sep = "")
  write.table(dfObj,file = fileConn, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE, fileEncoding = "UTF-8")
  close(fileConn)            
}

#' writes WASA input file R
#' @param long is a long data frame object whose points are the centroids of the subbasins
#' @param address is the path where the file should be written
#' @export
long2WASA_R <- function(long,address)
{
  try(system(paste0("mkdir ",address)))
  try(system(paste0("rm ",address,"/radiation.dat")))
  fileConn <- file(paste0(address,"/radiation.dat"),"a")
  cat("Daily average radiation [W/m2]","Date\tNo. of days\tSubbasin-ID.", file = fileConn, sep = "\n")
  
  dfObj1 <- select(long,date) %>%
    distinct(date) %>%
    mutate(str=strftime(date,"%d%m%Y"),index=row_number()) %>%
    select(-date)
  
  dfObj2 <- select(long,location,date,value) %>%
    mutate(value=round(value)) %>%
    spread(location,value) %>%
    select(-date)
  dfObj <- bind_cols(dfObj1,dfObj2)
  
  HEADER <- c("0","0",colnames(dfObj2))
  cat(HEADER, file = fileConn, sep = "\t")
  cat("\n",file = fileConn, sep = "")
  write.table(dfObj,file = fileConn, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE, fileEncoding = "UTF-8")
  close(fileConn)            
}


#' writes WASA input file T
#' @param stObj is a spacetime object whose points are the centroids of the subbasins
#' @param address is the path where the file should be written
#' @export
long2WASA_T <- function(long,address)
{
  
  try(system(paste0("mkdir ",address)))
  try(system(paste0("rm ",address,"/temperature.dat")))
  fileConn <- file(paste0(address,"/temperature.dat"),"a")
  cat("Daily average temperature (in degree Celsius)","Date\tNo. of days\tSubbasin-ID.", file = fileConn, sep = "\n")
  
  dfObj1 <- select(long,date) %>%
    distinct(date) %>%
    mutate(str=strftime(date,"%d%m%Y"),index=row_number()) %>%
    select(-date)
  
  dfObj2 <- select(long,location,date,value) %>%
    mutate(value=round(value)) %>%
    spread(location,value) %>%
    select(-date)
  dfObj <- bind_cols(dfObj1,dfObj2)
  
  HEADER <- c("0","0",colnames(dfObj2))
  cat(HEADER, file = fileConn, sep = "\t")
  cat("\n",file = fileConn, sep = "")
  write.table(dfObj,file = fileConn, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE, fileEncoding = "UTF-8")
  close(fileConn)            
}


#' writes WASA input file P
#' @param long is a long data frame object whose points are the centroids of the subbasins
#' @param address is the path where the file should be written
#' @export
long2WASA_P <- function(long,address)
{
  try(system(paste0("mkdir ",address)))
  try(system(paste0("rm ",address,"/rain_daily.dat")))
  fileConn <- file(paste0(address,"/rain_daily.dat"),"a")
  cat("Daily total precipitation [mm] for each subasin, ordered according to Map-IDs","Date\t\tSubbasin-ID.", file = fileConn, sep = "\n")
  
  dfObj1 <- select(long,date) %>%
    distinct(date) %>%
    mutate(str=strftime(date,"%d%m%Y"),index=row_number()) %>%
    select(-date)
  
  dfObj2 <- select(long,location,date,value) %>%
    mutate(value=round(value)) %>%
    spread(location,value) %>%
    select(-date)
  dfObj <- bind_cols(dfObj1,dfObj2)
  
  HEADER <- c("0","0",colnames(dfObj2))
  cat(HEADER, file = fileConn, sep = "\t")
  cat("\n",file = fileConn, sep = "")
  write.table(dfObj,file = fileConn, quote = FALSE, sep = "\t", row.names = FALSE, col.names=FALSE, fileEncoding = "UTF-8")
  close(fileConn)            
}