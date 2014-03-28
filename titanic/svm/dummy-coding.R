df <- data.frame(X1 = c("a","a","b","b"), X2 = c("a","b","c","a"), X3 = c(1.3, 2.5, -1.8, 0.2), Y = c(1,0,1,0))

dummy_code_df <- function(df, type="n:n") {
  shiftie <- ifelse(type == "n:n-1", 1, 0)
  
  levels_size <- sapply(unlist(lapply(df,nlevels)), function(n) ifelse(n==0,1,n - ifelse(type == "n:n-1",1,0)))
  num_levels <- sum(levels_size)
  offsets <- rep(1,ncol(df)) + c(0,cumsum(levels_size)[-length(levels_size)])
  
  is_factor <- sapply(df[1,],is.factor)
  
  mdf <- matrix(rep(0,num_levels*nrow(df)),ncol=num_levels)
  
  for(i in 1:nrow(df)) {
    for(j in 1:ncol(df)) {
      if(is_factor[j]) {
        levelID <- which(df[i,j] == levels(df[i,j]))
        mdf[i, offsets[j] + levelID - ifelse(type == "n:n-1",2,1)] <- ifelse(type == "n:n-1" & levelID == 1, 0, 1)
      } else {
        mdf[i, offsets[j]] <- df[i,j]
      }
    }
  }
  
  return(mdf)
}