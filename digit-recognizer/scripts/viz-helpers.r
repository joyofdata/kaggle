library(reshape2)
library(rasterVis)
library(stringr)
library(msm)
library(doSNOW)

# are grid search results similar for different SVMs?

compare.performance <- function(m, value.var="cross", normalize = FALSE, pot=1.5, max.top.diff=0.001) {
	if("sigma" %in% colnames(m)) {
		m <- as.matrix(dcast(as.data.frame(m),C+sigma~A+B,value.var=value.var))
		rownames(m) <- paste("C = ",m[,1]," / sig = ",m[,2],sep="")
	} else if ("degree" %in% colnames(m)) {
		m <- as.matrix(dcast(as.data.frame(m),C+degree~A+B,value.var=value.var))
		rownames(m) <- paste("C = ",m[,1]," / deg = ",m[,2],sep="")
	}
	
	colnames(m) <- str_replace_all(colnames(m),"_",":")
	
	m <- m[,3:ncol(m)]
	
	if(normalize) {
		m.plot <- apply(m,2,function(c)(c-min(c))/(max(c)-min(c)))
	} else {
		m.plot <- m
	}

	plot(levelplot(t(m.plot), col.regions=colorRampPalette(c("red", "green","blue"), space = "Lab")(120), at=(seq(0,max(m.plot),length.out=100))^pot))

	print(sapply(colnames(m),function(ab){
    m.err.1 <- m[which(abs(min(m[,ab])-m[,ab])<=max.top.diff),ab]
    m.err.1[order(m.err.1)]
	},simplify=F))
	
	m <- apply(apply(m,1,function(c)c-min(c)),2,mean)
	print("ranking:")
  print(m[order(m)][1:10])
}

simulate.accuracy.of.classifier.set <- function(N,csd=0.1,cores=8,const.noise=FALSE) {
    result <- rep(NA, N)
    
    ab <- expand.grid(0:9,0:9)
    ab <- ab[ab[,1] < ab[,2],]
    rownames(ab) <- NULL
    ab <- ab[order(ab[,1],ab[,2]),]
    
    if(const.noise) ab[,3] <- c(rep(p,9),rtnorm(36,0.5,csd,0.1,0.9))
    
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
    
    result <- foreach(i = 51:100, .packages="msm", .combine="rbind") %dopar% {
        p <- i/100
        
        for(i in 1:N) {
            if(!const.noise) ab[,3] <- c(rep(p,9),rtnorm(36,0.5,csd,0.1,0.9))
            
            v <- apply(ab,1,function(r) sample(r[1:2],1,prob=c(r[3],1-r[3])))
            freq <- sort(table(v),decreasing=TRUE)
            w <- freq[freq == max(freq)]
            
            if(length(w) == 1 & names(w[1]) == "0") {
                result[i] <- 1
            } else if (length(w) == 1 & names(w[1]) != "0") {
                result[i] <- 0
            } else {
                result[i] <- ifelse(sample(names(w),1) == "0", 1, 0)
            }
        }
        
        c(p, sum(result==1)/N)
    }
    
    stopCluster(cl)
    
    return(result)
}