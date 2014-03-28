setwd("F:\\git-repos\\kaggle\\digit-recognizer\\")

load("data/ready.to.go.RData")

source("scripts/transform-raw-to-features.r")
source("scripts/helpers.r")

test.data.file <- "data\\test.csv"

test.data.init <- read.digits(test.data.file)
test.clipped <- clip.matrices(test.data.init)

test.data.a <- feature.extraction.asis(test.clipped,10,10)
test.data.a[["features"]] <- scale.features(test.data.a[["features"]])

test.data.m <- feature.extraction.measurements(test.clipped)
test.data.m[["features"]] <- scale.features(test.data.m[["features"]])

save.image("data/ready.to.go.RData")

test.labels.a <- apply.svms.to.data.set(svms.polydot.a, test.data.a[["features"]])
test.conflicts <- apply.svms.to.data.set.identify.conflicts(svms.polydot.a, test.data.a[["features"]], cores=6)

save.image("data/ready.to.go.RData")

svms.polydot.a.recursive <- train.models(rbind(polydot.a.1st,data.a[["features"]],test.data.a),c(data.a[["labels"]],test.labels.a),"polydot")