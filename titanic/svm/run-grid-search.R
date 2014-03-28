setwd("/media/Volume/git-repos/kaggle/titanic/svm/")
source("./grid-search.r")
source("../prepare-data.R")

df_train <- read.table("../data/train.csv", sep=",", header=TRUE)
df_test <- read.table("../data/test.csv", sep=",", header=TRUE)

data <- read_data(df_train, df_test, shuffle=TRUE)


# http://www.csie.ntu.edu.tw/~cjlin/papers/tanh.pdf
# gaussian:  C x sigma     = {2^-3, ..., 2^12} x {2^-12, ..., 2^2}
# tanh:      C x sigma x r = {2^-3, ..., 2^12} x {2^-12, ..., 2^2} x {-2.4, -1.8, ..., 2.4}
# poly:      C x deg x off = {2^-3, ..., 2^12} x {1, ..., 5}       x {0,1}

G <- rbind(
  #expand.grid(kernel = "rbfdot",  C = 10^(-2:5), p1 = 10^(-5:1), p2 = N.),
  #expand.grid(kernel = "tanhdot", C = 10^(-2:5), p1 = 10^(-5:1), p2 = seq(-2.4,2.4,0.6))
  expand.grid(kernel = "polydot", C = 10^(-2:2), p1 = c(1,2,3,4,5), p2 = c(0,1))
)

G <- G[sample(nrow(G)),]

formula <- (survived ~ class + title + parch + ticket + cabin + ageCat + embarked + sibsp + sex)


fullResult <- N.
step_size <- 40
num_of_iterations <- floor(nrow(G) / step_size) + ifelse(nrow(G)%%step_size==0,0,1)

for(i in 1:num_of_iterations) {
  subG <- G[((i-1)*step_size+1):min((i*step_size), nrow(G)),]
  print(sprintf("G from %d to %d",((i-1)*step_size+1),min((i*step_size), nrow(G))))
  
  subResult <- kernlab_grid_search(data$train, subG, formula, k=100)
  
  if(!is.na(fullResult)) {
    fullResult <- rbind(fullResult, subResult)
  } else {
    fullResult <- subResult
  }
  
  save.image("../data/svm.grid.search.RData")
}

fullResult_P <- fullResult[order(fullResult[,"cross"]),]
save.image("../data/svm.grid.search.RData")

##################################################################################################################

data <- read_data(df_train, df_test, shuffle=FALSE)

m <- ksvm(formula, data=data$train, kernel="tanhdot", C=100, kpar=list(scale=0.01, offset=-1.2))

res <- data.frame(PassengerId = df_test$PassengerId, Survived = as.numeric(as.character(predict(m, data$test))))
write.table(res, "result.txt", row.names=FALSE, sep=",")