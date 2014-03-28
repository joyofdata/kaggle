kernlab_grid_search <- function(df, G, formula, k, cores=6, maxIter=N.) {
  library(doSNOW, quietly=TRUE)
  
  nIter <- ifelse(is.na(maxIter), nrow(G), maxIter)
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  res <- foreach(i = 1:nIter, .packages="kernlab", .combine="rbind") %dopar% {
    g <- G[i,]
    
    res <- c(
      "kernel"= as.character(g$kernel),
      "C"     = g$C,
      "p1"    = g$p1,
      "p2"    = g$p2,
      "error" = N.,
      "cross" = N.,
      "numSV" = N.,
      "obj_f" = N.,
      "time"  = N.
    )
    
    try({
      t <- system.time(
        if(g$kernel == "rbfdot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(sigma=g$p1), cross=k)
        } else if (g$kernel == "tanhdot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(scale=g$p1, offset=g$p2), cross=k)
        } else if (g$kernel == "polydot") {
          m <- ksvm(formula, data=df, kernel=as.character(g$kernel), C=g$C, kpar=list(degree=g$p1, offset=g$p2), cross=k)
        }
      )
    
      res <- c(
        "kernel"= as.character(g$kernel),
        "C"     = g$C,
        "p1"    = g$p1,
        "p2"    = g$p2,
        "error" = m@error,
        "cross" = m@cross,
        "numSV" = m@nSV,
        "obj_f" = m@obj,
        "time"  = t["elapsed"]
      )
    })
    
    return(res)
  }
  
  stopCluster(cl)
  
  return(res)
}