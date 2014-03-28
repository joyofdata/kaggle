setwd("F:\\git-repos\\kaggle\\digit-recognizer\\")

load("data/ready.to.go.RData")

source("scripts/helpers.r")
source("scripts/grid-search.r")

polydot.a.1st <- best.of.classifiers(result.polydot.a,2,"C","degree")

result.polydot.a.1st <- list()
for(i in 1:nrow(polydot.a.1st)) {

	C <- polydot.a.1st[i,"C"]
	D <- polydot.a.1st[i,"degree"]

	vC <- c(max(1,C/2),C,C*5)

	AB <- matrix(as.character(polydot.a.1st[i,c("A","B")]),ncol=2)
	result.polydot.a.1st[[i]] <- perform.grid.search(data.a[["features"]], data.a[["labels"]], "polydot", vC, sigma = NA, criss=1, cross=10, deg = D, NA, ab=AB, sample.size=0, cores=6)
	print(i)
}
save.image("data/ready.to.go.RData")

result.m.polydot.a.2nd.round <- result.sets.to.matrix(result.polydot.a.1st,"results/polydot-a-2nd-round.csv")
polydot.a.1st <- best.of.classifiers(result.m.polydot.a.2nd.round,100,"C","degree")

svms.polydot.a <- train.models(polydot.a.1st,data.a[["features"]],data.a[["labels"]],"polydot")

save.image("data/ready.to.go.RData")