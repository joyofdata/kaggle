#######################################################
# set up environment
#######################################################

setwd("F:\\git-repos\\kaggle\\digit-recognizer\\")

source("scripts/transform-raw-to-features.r")
source("scripts/helpers.r")
source("scripts/grid-search.r")

#######################################################
# prepare data: turn data into scaled feature vectors
#######################################################

data.file <- "data\\train.csv"

data.init <- read.digits(data.file)
clipped <- clip.matrices(data.init)

data.a <- feature.extraction.asis(clipped,10,10)
data.a[["features"]] <- scale.features(data.a[["features"]])

data.m <- feature.extraction.measurements(clipped)
data.m[["features"]] <- scale.features(data.m[["features"]])

data.n <- feature.extraction.neighbours(clipped,th=90)
data.n[["features"]] <- scale.features(data.n[["features"]])

#######################################################
# perform a custom grid search
#######################################################

ab1 <- matrix(as.character(c(0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,1,2,1,3)),ncol=2,byrow=TRUE)
ab2 <- matrix(as.character(c(1,4,1,5,1,6,1,7,1,8,1,9,2,3,2,4,2,5,2,6,2,7)),ncol=2,byrow=TRUE)
ab3 <- matrix(as.character(c(2,8,2,9,3,4,3,5,3,6,3,7,3,8,3,9,4,5,4,6,4,7)),ncol=2,byrow=TRUE)
ab4 <- matrix(as.character(c(4,8,4,9,5,6,5,7,5,8,5,9,6,7,6,8,6,9,7,8,7,9,8,9)),ncol=2,byrow=TRUE)

vC <- c(1,10,100,1000,10000)
vS <- c(1,0.1,0.01,0.001,0.0001)

vC <- c(1,10,100,1000,10000)
vD <- 1:5

# rbfdot

result.ab1.rbfdot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab2.rbfdot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab2, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab3.rbfdot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab3, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab4.rbfdot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab4, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")

result.ab1.rbfdot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab2.rbfdot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab2, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab3.rbfdot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab3, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
result.ab4.rbfdot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab4, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")

result.ab1.rbfdot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
# performance very low while computationally very expensive for above cases - hence stopped
#result.ab2.rbfdot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab2, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab3.rbfdot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab3, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab4.rbfdot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "rbfdot", vC, vS, criss=10, cross=5, deg=NA, nsvm=NA, ab4, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")

# polydot

result.ab1.polydot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
#result.ab2.polydot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab2, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab3.polydot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab3, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab4.polydot.a <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab4, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")

result.ab1.polydot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
#result.ab2.polydot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab2, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab3.polydot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab3, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab4.polydot.m <- perform.grid.search(data.m[["features"]], data.m[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab4, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")

result.ab1.polydot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab1, sample.size=1000, cores=8)
save.image("data/ready.to.go.RData")
#result.ab2.polydot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab2, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab3.polydot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab3, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")
#result.ab4.polydot.n <- perform.grid.search(data.n[["features"]], data.n[["labels"]], "polydot", vC, sigma=NA, criss=10, cross=5, deg=vD, nsvm=NA, ab4, sample.size=1000, cores=8)
#save.image("data/ready.to.go.RData")


# collect the result sets more conveniently

result.rbfdot.a <- result.sets.to.matrix(list(result.ab1.rbfdot.a,result.ab2.rbfdot.a,result.ab3.rbfdot.a,result.ab4.rbfdot.a),"results/rbfdot-a.csv")
result.rbfdot.m <- result.sets.to.matrix(list(result.ab1.rbfdot.m,result.ab2.rbfdot.m,result.ab3.rbfdot.m,result.ab4.rbfdot.m),"results/rbfdot-m.csv")
result.rbfdot.n <- result.sets.to.matrix(list(result.ab1.rbfdot.n),"results/rbfdot-n.csv")

result.polydot.a <- result.sets.to.matrix(list(result.ab1.polydot.a,result.ab2.polydot.a,result.ab3.polydot.a,result.ab4.polydot.a),"results/polydot-a.csv")
result.polydot.m <- result.sets.to.matrix(list(result.ab1.polydot.m),"results/polydot-m.csv")
result.polydot.n <- result.sets.to.matrix(list(result.ab1.polydot.n),"results/polydot-n.csv")

save.image("data/ready.to.go.RData")