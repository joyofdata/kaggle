
# > kfolds(23,4)
# [1] 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4
kfolds <- function(N, k) {
  return(
    sort(
      rep(1:k, ceiling(N / k))[1:N]
    )
  )
}

hypergrid <- function(k, tt, minc, mins, minb, maxd, shuffle = FALSE) {
  G <- expand.grid(tt=tt, minc=minc, mins=mins, minb=minb, maxd=maxd, k=1:k,
                   stringsAsFactors=FALSE)
  G <- G[G$mins > G$minb,]
  
  if(shuffle) {
    G <- G[sample(nrow(G)),]
  }
  
  return(G)
}

gridsearch_ctree <- function(K, G, data, formula, cores=8, maxIter=NA) {
  library(doSNOW, quietly=TRUE)
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  if(is.na(maxIter)) {
    nIter <- nrow(G)
  } else {
    nIter <- maxIter
  }
  
  res <- foreach(i = 1:nIter, .packages="party", .combine="rbind") %dopar% {
    if(i %% 100 == 0) cat(sprintf("%d of %d",i,nrow(G)), file="state.txt")
    
    cfg <- G[i,]
    ctrl <- ctree_control(
      testtype = cfg$tt,
      mincriterion = cfg$minc,
      minsplit = cfg$mins,
      minbucket = cfg$minb,
      maxdepth = cfg$maxd
    )
    
    data_train <- data[K != cfg$k,]
    data_test <- data[K == cfg$k,]
    
    res <- data.frame(
      testtype = cfg$tt,
      mincriterion = cfg$minc,
      minsplit = cfg$mins,
      minbucket = cfg$minb,
      maxdepth = cfg$maxd,
      correct = NA,
      incorrect = NA,
      elapsed = NA,
      terminals = NA
    )
    
    try({
      time <- system.time(
        t <- ctree(formula, data_train, controls = ctrl)
      )
      predictions <- Predict(t, data_test)
      
      response <- all.vars(formula)[1]
      
      nc <- sum(predictions == data_test[,response])
      
      res <- data.frame(
        testtype = cfg$tt,
        mincriterion = cfg$minc,
        minsplit = cfg$mins,
        minbucket = cfg$minb,
        maxdepth = cfg$maxd,
        correct = nc,
        incorrect = length(predictions) - nc,
        elapsed = time["elapsed"],
        terminals = length(unique(where(t)))
      )
    })
    
    return(res)
  }
  
  stopCluster(cl)
  
  return(res)
}