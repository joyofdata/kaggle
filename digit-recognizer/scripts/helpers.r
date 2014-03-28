library(raster)
library(doSNOW)
library(kernlab)

pic <- function(digit, width=NA, th=NA, byrow=FALSE, grid=TRUE) {

	if(!is.na(th) & is.numeric(th)) {
		digit <- ifelse(digit >= th, 256, 0)
	}
	
	if(is.vector(digit)) {
		Ncol <- width
		Nrow <- length(digit)/width
		
		digit <- raster(matrix((unlist(digit)), ncol=width, byrow=byrow))
	} else if(is.matrix(digit)) {
		Nrow <- dim(digit)[1]
		Ncol <- dim(digit)[2]
		
		digit <- raster(digit)
	}
	
	plot(digit, col = grey(seq(1, 0, length = 256)))
	
	if(grid) {
		abline(v=(0:Ncol)/Ncol,col="red")
		abline(h=(0:Nrow)/Nrow,col="red")
	}
}

scale.features <- function(features) {
	return(apply(features,2,function(e)(e-mean(e))/ifelse(sd(e)==0,1,sd(e))))
}

result.sets.to.matrix <- function(L, file.name) {
	all.results <- 0
	for(result.set in L) {
		results <- 0
		for(l in result.set) {
			if(!is.null(l[["a"]]) & !is.null(l[["b"]]) & !is.null(l[["result"]])) {
				num.of.results <- nrow(l[["result"]])
				result <- cbind(
						rep(l[["a"]],num.of.results),
						rep(l[["b"]],num.of.results),
						l[["result"]]
				)
				
				if(!is.matrix(results)) {
						results <- result
				} else {
						results <- rbind(results,result)
				}
			}
		}
		
		if(!is.matrix(all.results)) {
				all.results <- results
		} else {
				all.results <- rbind(all.results,results)
		}
	}
	m <- apply(all.results,2,function(c)round(as.numeric(c),4))
	colnames(m) <- c(c("A","B"),colnames(m)[3:ncol(m)])
	
	if(!is.na(file.name)) {
		write.table(m,file.name, sep=",", row.names=FALSE)
	}
	
	return(m)
}

performance.overview <- function(L, v = "cross", th = 0.01, v2=NA, th2 = NA) {
	
	ab <- expand.grid(0:9,0:9)
	ab <- ab[ab[,1] < ab[,2],]
	ab.names <- apply(ab,1,function(r)paste(r,collapse=":"))
	
	m <- matrix(0, ncol = nrow(ab), nrow = length(L))
	colnames(m) <- ab.names
	rownames(m) <- names(L)
	
	for(n in names(L)) {
		if(is.na(v2)) {
			l <- L[[n]][L[[n]][,v] <= th,c("A","B")]
		} else {
			l <- L[[n]][L[[n]][,v] <= th & L[[n]][,v2] <= th2,c("A","B")]
		}
		
		for(i in 1:nrow(l)) {
			m[n,paste(l[i,"A"],l[i,"B"],sep=":")] <- m[n,paste(l[i,"A"],l[i,"B"],sep=":")] + 1
		}
	}
	
	return(m)
}

best.of.classifiers <- function(result, th.time = 2, p1 = "C", p2 = "sigma") {
	ab <- expand.grid(0:9,0:9,NA,NA)
	ab <- ab[ab[,1] < ab[,2],]
	colnames(ab) <- c("A","B",p1,p2)
	
	result <- result[result[,"t.elapsed"] <= th.time,]
	result <- result[order(result[,"cross"],result[,"SVs"],result[,"t.elapsed"]),]
	
	for(i in 1:nrow(ab)) {
		r <- result[result[,"A"] == ab[i,"A"] & result[,"B"] == ab[i,"B"],]
		ab[i,p1] <- r[1,p1]
		ab[i,p2] <- r[1,p2]
	}
	
	return(ab)
}

train.models <- function(chosen.ones, features, labels, kernel.type="rbfdot") {
	if(kernel.type == "rbfdot") {
		kpar <- list("sigma" = NA)
		second.param.name <- "sigma"
	} else if(kernel.type == "polydot") {
		kpar <- list("degree" = NA)
		second.param.name <- "degree"
	}	
	
	result <- list()

	for(i in 1:nrow(chosen.ones)) {
		result[[i]] <- list()
		
		kpar[[second.param.name]] <- chosen.ones[i,second.param.name]
		
		A <- as.character(min(chosen.ones[i,c("A","B")]))
		B <- as.character(max(chosen.ones[i,c("A","B")]))
		
		result[[i]][["A"]] <- A
		result[[i]][["B"]] <- B
		
		features.i <- rbind(features[labels == A,],features[labels == B,])
		labels.i <- as.factor(c(labels[labels == A], labels[labels == B]))
		
		result[[i]][["model"]] <- ksvm(features.i, labels.i,type="C-svc",C=chosen.ones[i,"C"], kpar = kpar, kernel=kernel.type, cache = 1000, scaled=FALSE)
		
		print(i)
	}
	
	return(result)
}

apply.svms.to.data.set <- function(svms, data.set, cores=8) {
	apply.svms.to.vector <- function(svms, feature.vector) {
		v <- c("0"=0,"1"=0,"2"=0,"3"=0,"4"=0,"5"=0,"6"=0,"7"=0,"8"=0,"9"=0)
		for(svm in svms) {
			x <- as.character(predict(svm[["model"]],matrix(feature.vector,nrow=1)))
			v[x] <- v[x] + 1
		}
		v <- names(v[v==max(v)])
		v <- v[sample(length(v),1)]
		return(v)
	}
	
	cl <- makeCluster(cores)
	registerDoSNOW(cl)
	
	labels <- foreach(i = 1:nrow(data.set), .combine="c", .packages="kernlab") %dopar% {
		apply.svms.to.vector(svms, data.set[i,])
	}
	
	stopCluster(cl)
	
	return(labels)
}

apply.svms.to.data.set.identify.conflicts <- function(svms, data.set, cores=8) {
	apply.svms.to.vector <- function(svms, feature.vector) {
		v <- c("0"=0,"1"=0,"2"=0,"3"=0,"4"=0,"5"=0,"6"=0,"7"=0,"8"=0,"9"=0)
		w <- rep(0,10)
		
		for(svm in svms) {
			x <- as.character(predict(svm[["model"]],matrix(feature.vector,nrow=1)))
			v[x] <- v[x] + 1
		}
		w[v==max(v)] <- 1
		return(w)
	}
	
	cl <- makeCluster(cores)
	registerDoSNOW(cl)
	
	labels <- foreach(i = 1:nrow(data.set), .combine="rbind", .packages="kernlab") %dopar% {
		apply.svms.to.vector(svms, data.set[i,])
	}
	
	stopCluster(cl)
	
	return(labels)
}