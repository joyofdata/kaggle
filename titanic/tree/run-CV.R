setwd("/media/Volume/git-repos/kaggle/titanic/tree/")
source("../prepare-data.R")
source("cv.R")



df_train <- read.table("../data/train.csv", sep=",", header=TRUE)
df_test <- read.table("../data/test.csv", sep=",", header=TRUE)

data <- read_data(df_train, df_test, shuffle=TRUE)

###################################################
# prepare data
###################################################

k <- 100

K <- kfolds(nrow(data$train), k)
G <- hypergrid(k, 
       tt = c("Bonferroni","Univariate","Teststatistic"), 
       minc = c(0.9, 0.95, 0.99), 
       mins = c(60,40,20, 10), 
       minb = c(30,20,10,5), 
       maxd = c(0,4,5,6),
       shuffle = TRUE
     )

###################################################
# perform CV
###################################################

result <- NA

# 57600 = 300 * 192
# 39600 = 900 * 44

library(party)

for(i in 1:44) {
  subG <- G[((i-1)*900+1):(i*900),]
  print(sprintf("G from %d to %d",((i-1)*900+1),i*900))
  
  subResult <- gridsearch_ctree(K, subG, data$train,
     survived ~ class + title + parch + ticket + cabin + ageCat + embarked + sibsp + sex,
     cores = 6, maxIter = NA
  )
  
  if(!is.na(result)) {
    result <- rbind(result, subResult)
  } else {
    result <- subResult
  }
  
  save.image("../data/ctree.grid.search.RData")
}

###################################################
# analyze results
###################################################

library(sqldf)
r <- sqldf("select testtype, mincriterion, minsplit, minbucket, maxdepth, sum(correct) as N1, sum(incorrect) as N0, sum(elapsed) as Time, sum(terminals) as Term, count(correct) as N from result group by testtype, mincriterion, minsplit, minbucket, maxdepth")
r$err <- r$N0 / (r$N0 + r$N1)
r <- r[order(r$err),]
r$idx <- 1:nrow(r)
head(r)

r$testtype <- as.character(r$testtype)

v <- rep(NA,nrow(r))

for(i in 1:nrow(r)) {
  t <- ctree(survived ~ class + title + parch + ticket + cabin + ageCat + embarked + sibsp + sex, data$train,
     controls = ctree_control(
       teststat="quad",
       testtype=r[i,"testtype"],
       mincriterion=r[i,"mincriterion"],
       minsplit=r[i,"minsplit"], 
       minbucket=r[i,"minbucket"],
       maxdepth=r[i,"maxdepth"]
     )
  )
  
  v[i] <- sum(t@predict_response() == data$train$survived)/nrow(data$train)
}
r$err_all <- v

r_unique_trees <- sqldf("select min(idx) as idx from r group by err_all")