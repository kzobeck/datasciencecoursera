pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## set counters to 0
  sum_pollutant <- 0
  num_reads <- 0
  
  ## determine pollutant
  column <- if ( pollutant == "sulfate" ) { 2 }
    else if ( pollutant == "nitrate" ) { 3 }
  
  ## calculate pollutant in id range, and number of reads
  for ( i in id ) {
    ## create name of file from directory and id
    file <- if ( i < 10 ) { paste("00", i, ".csv", sep = "" ) }
      else if ( i >= 10 & i < 100 ) { paste("0", i, ".csv", sep = "" ) }
      else { paste(i, ".csv", sep = "" ) }  
    string <- paste(directory, file, sep = "/")
    data = read.csv(string)
    
    ## Identify valid reads perform calculations for sum and total number of reads
    pos_reads <- t(data[column])[!is.na(t(data[column]))]
    sum_pollutant <- sum_pollutant + sum(pos_reads)
    num_reads <- num_reads + length(pos_reads)
  }
  
  #calculate mean (sum / reads) rounded to thousanths.
  round(sum_pollutant / num_reads, 3)
}