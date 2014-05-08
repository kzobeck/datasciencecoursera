corr <- function(directory, threshold = 0) {  
  ## Run complete.R on the directory for all files, using default settings
  comp_res <- complete(directory)
  
  ## Initialize vectors
  id_at_thresh <- vector()
  correlations <- vector()
  
  ## Calculate which rows meet the threshold
  for ( i in 1:nrow(comp_res) ) {
    row = comp_res[i,]
    if ( row[2] > threshold ) {
      id_at_thresh <- append(id_at_thresh, i)
    }
  }
  
  ## itterate through id_at_thresh to identify files and perform correlations
  if (length(id_at_thresh) > 0) { 
    for ( f in 1:length(id_at_thresh) ) {
      ## create name of file from directory and id_at_thresh
      i = id_at_thresh[f]
      file <- if ( i < 10 ) { paste("00", i, ".csv", sep = "" ) }
        else if ( i >= 10 & i < 100 ) { paste("0", i, ".csv", sep = "" ) }
        else { paste(i, ".csv", sep = "" ) }    
      string <- paste(directory, file, sep = "/")
      data = read.csv(string)
    
      ## Perform Correlations on file and append to vector
      x <- as.vector(t(data[2]))
      y <- as.vector(t(data[3]))
      new_cor = cor(x, y, use = "complete.obs")
      correlations = append(correlations, new_cor)
    }
  }
  
  ## Return results
  correlations
}
