complete <- function(directory, id = 1:332) {
  ## initialize nobs data.frame
  nobs <- data.frame(id = numeric() , nobs = numeric() )
  
  ## cycle through number of id's
  for ( i in id ) {
    ## create name of file from directory and id
    file <- if ( i < 10 ) { paste("00", i, ".csv", sep = "" ) }
      else if ( i >= 10 & i < 100 ) { paste("0", i, ".csv", sep = "" ) }
      else { paste(i, ".csv", sep = "" ) }  
    string <- paste(directory, file, sep = "/")
    data = read.csv(string)
    
    ## initiize count to zero for each id
    count <- 0
    
    ## cycle through rows and calculte nobs that are complete cases
    for ( f in 1:nrow(data) ) {
      row <- data[f,]
      if ( (!is.na(row[2])) & (!is.na(row[3])) ) {
        count <- count + 1
      }
    }
    
    ## add nob information to data.frame for each id
    id_nob <- data.frame(id = i, nobs = count)
    nobs <- rbind(nobs, id_nob)  
  }  
  
  ## Return results
  nobs
}