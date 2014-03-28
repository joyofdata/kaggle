library(doSNOW)
library(kernlab)

perform.grid.search <- function(features, labels, kernel.type, C, sigma, criss=5, cross=10, deg = NA, nsvm, ab=NA, sample.size=0, cores=8) {

  if(!is.matrix(ab)) {
		classes <- unique(labels)
		ab <- expand.grid(classes,classes)
		ab <- ab[ab[,1]<ab[,2],]
		ab <- ab[sample(nrow(ab), nsvm),]
	}
	
	if(kernel.type == "rbfdot") {
		g <- expand.grid(C,sigma)
		kpar <- list("sigma" = NA)
		second.param.name <- "sigma"
	} else if(kernel.type == "polydot") {
		g <- expand.grid(C,deg)
		kpar <- list("degree" = NA)
		second.param.name <- "degree"
	}	
	grid.size <- nrow(g)

	cl	<- makeCluster(cores)
	registerDoSNOW(cl)

	result <- list()
	result[["settings"]] <- list("kernel" = kernel.type, "criss" = criss, "cross" = cross, "size" = sample.size, "cores" = cores)

	for(i in 1:nrow(ab)) {
		print(i)

		A <- ab[i,1]
		B <- ab[i,2]

		mA.all <- features[labels == A,]
		mB.all <- features[labels == B,]
		
		result[[paste(ab[i,1],ab[i,2],sep=".")]] <- list()
		result[[paste(ab[i,1],ab[i,2],sep=".")]][["a"]] <- A
		result[[paste(ab[i,1],ab[i,2],sep=".")]][["b"]] <- B

		result[[paste(ab[i,1],ab[i,2],sep=".")]][["result"]] <- foreach(k = 1:grid.size, .combine="rbind", .packages="kernlab") %dopar% {
		
			kpar[[1]] <- g[k,2]
			
			performance <- rep(0,7)
			for(l in 1:criss) {
				if(is.numeric(sample.size) & sample.size > 0) {
					mA <- mA.all[sample(nrow(mA.all),sample.size),]
					mB <- mB.all[sample(nrow(mB.all),sample.size),]
				} else {
					mA <- mA.all
					mB <- mB.all
				}
				mAB <- rbind(mA,mB)
				
				mAB_target <- as.factor(c(rep("a",nrow(mA)),rep("b",nrow(mB))))

				t <- system.time({
					modAB <- ksvm(mAB, mAB_target,type="C-svc",C=g[k,1], kpar = kpar, kernel=kernel.type, cross=cross, cache = 1000, scaled=FALSE)
				})
				performance <- performance + c(modAB@error,modAB@cross,modAB@nSV,modAB@obj,t["user.self"],t["sys.self"],t["elapsed"])
			}
			c(g[k,1],g[k,2], performance / criss)
		}
		colnames(result[[paste(ab[i,1],ab[i,2],sep=".")]][["result"]]) <- c("C",second.param.name,"error","cross","SVs","obj.fun","t.user","t.sys","t.elapsed")
	}

	stopCluster(cl)	
	
	return(result)
}
