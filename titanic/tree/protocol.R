setwd("/media/Volume/git-repos/kaggle/titanic/tree")
source("../prepare-data.R")

library(party)

df_train <- read.table("../data/train.csv", sep=",", header=TRUE)
df_test <- read.table("../data/test.csv", sep=",", header=TRUE)

data <- read_data(df_train, df_test)

t <- ctree(survived ~ class + title + parch + ticket + cabin + ageCat + embarked + sibsp, data$train,
           controls = ctree_control(
             teststat="quad",
             testtype="Univariate",
             mincriterion=.95,
             minsplit=10, 
             minbucket=5
           )
)


sum(t@predict_response() == data$train$survived)/nrow(data$train)

result <- data.frame(PassengerId = df_test$PassengerId, Survived = as.numeric(as.character(Predict(t, data$test))))
write.table(result, "result.txt", row.names=FALSE, sep=",")