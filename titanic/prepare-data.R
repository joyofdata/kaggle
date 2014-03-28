read_data <- function(df_train, df_test,shuffle=FALSE) {
  df_train_prepared <- prepare_data(df_train)
  df_test_prepared <- prepare_data(df_test)
  
  df <- rbind(df_train_prepared, df_test_prepared)
  
  # homogenize levels
  df_train_prepared <- df[1:nrow(df_train_prepared),]
  df_test_prepared <- df[(nrow(df_train_prepared)+1):nrow(df),]
  
  if(shuffle) {
    df_train_prepared <- df_train_prepared[sample(nrow(df_train_prepared)),]
    df_test_prepared <- df_test_prepared[sample(nrow(df_test_prepared)),]
  }
  
  list(train = df_train_prepared, test = df_test_prepared)
}

prepare_data <- function(data) {
  if("Survived" %in% names(data)) {
    d <- data.frame(survived = factor(data$Survived))
  } else {
    d <- data.frame(survived = rep(NA,nrow(data)))
  }

  d$class <- factor(data$Pclass)
  d$sex <- factor(data$Sex)
  
  d$title <- rep(NA, nrow(data))
  for(i in 1:nrow(data)) {
    if(length(grep("Mr\\.|Don\\.|Jonkheer\\.|Sir\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Mister"
    } else if(length(grep("Dr\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Doctor"
    } else if(length(grep("Major\\.|Col\\.|Capt\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Soldier"
    } else if(length(grep("Rev\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Reverend"
    } else if(length(grep("Mrs\\.|Mme\\.|Countess\\.|Dona\\.|Lady\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Missis"
    } else if(length(grep("Miss\\.|Mlle\\.|Ms\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Miss"
    } else if(length(grep("Master\\.",data[i,"Name"]))==1) {
      d[i,"title"] <- "Master"
    }
  }
  
  d$title <- factor(d$title)
  
  d$sibsp <- data$SibSp
  d$parch <- data$Parch
  
  d$age <- as.numeric(data$Age)

  d$ageCat <- apply(d, 1, function(row) {
    age <- as.numeric(row["age"])
    title <- row["title"]

    if(is.na(age)) {
      if(title == "Master") {
        return("[0,5)x")
      } else {
        return("(15,60)")
      }
    } else if(age < 5) {
      return("[0,5)")
    } else if(age >= 5 && age <= 10) {
      return("[5,10]")
    } else if(age > 10 && age <= 15) {
      return("(10,15]")
    } else if(age > 15 && age < 60) {
      return("(15,60)")
    } else if(age >= 60) {
      return("[60,.)")
    } else {
      return(NA)
    }
  })
  
  d$ageCat <- factor(d$ageCat)
  
  d$ticket <- gsub("[^A-Z]","",data$Ticket)
  d$ticket <- factor(ifelse(d$ticket=="","123",d$ticket))
  
  d$fare <- data$Fare
  
  d$cabin <- factor(ifelse(data$Cabin == "", "unknown", regmatches(data$Cabin,regexpr("[A-Z]",data$Cabin))))
  d$embarked <- factor(data$Embarked)
  
  return(d)
}