library(plyr)
library(raster)

read.digits <- function(file.path) {
	df <- read.table(file.path, header=T, sep=",")
	
	if(!"label" %in% colnames(df)) {
		df <- cbind(rep("?",nrow(df)),df)
		colnames(df) <- c("label",colnames(df)[2:ncol(df)])
	}

	digits <- dlply(df,.(label),function(x)as.matrix(x[,-1]))
	
	return(digits)
}

clip.matrix <- function(digit.vec, width) {
	m <- matrix((unlist(digit.vec)), ncol=width, byrow=TRUE)
	
	rows <- apply(m,1,function(row)sum(row)>0)
	cols <- apply(m,2,function(col)sum(col)>0)
	
	return(m[rows,cols])
}

clip.matrices <- function(digits) {
	digits_cut_out <- list()
	
	for(digit in names(digits)) {
		digits_cut_out[[digit]] <- apply(digits[[digit]],1,function(d)clip.matrix(d,28))
	}

	return(digits_cut_out)
}

# feature extraction

feature.extraction.measurements <- function(digits) {
	symmetry.vertical.axis <- function(digit, th=90) {
		nc <- dim(digit)[2]
		
		matching <- sum((digit[,1:floor(nc/2)]>=th) & (digit[,nc:ceiling(nc/2+1)]>=th))
		pot.matching <- sum((digit[,1:floor(nc/2)]>=th) | (digit[,nc:ceiling(nc/2+1)]>=th))
		
		matching / pot.matching
	}

	symmetry.horizontal.axis <- function(digit, th=90) {
		nr <- dim(digit)[1]
		
		matching <- sum((digit[1:floor(nr/2),]>=th) & (digit[nr:ceiling(nr/2+1),]>=th))
		pot.matching <- sum((digit[1:floor(nr/2),]>=th) | (digit[nr:ceiling(nr/2+1),]>=th))
		
		matching / pot.matching
	}

	mean.point.value <- function(digit) {
		mean(digit) / max(digit)
	}

	width.per.height <- function(digit) {
		dim(digit)[2] / dim(digit)[1]
	}

	quarter.top.left <- function(digit, th=90) {
		nr <- dim(digit)[1]
		nc <- dim(digit)[2]
		
		sum(digit[1:floor(nr/2),1:floor(nc/2)]>=th) / sum(digit >= th)
	}

	quarter.top.right <- function(digit, th=90) {
		nr <- dim(digit)[1]
		nc <- dim(digit)[2]
		
		sum(digit[1:floor(nr/2),nc:ceiling(nc/2+1)]>=th) / sum(digit >= th)
	}

	quarter.bottom.left <- function(digit, th=90) {
		nr <- dim(digit)[1]
		nc <- dim(digit)[2]
		
		sum(digit[nr:ceiling(nr/2+1),1:floor(nc/2)]>=th) / sum(digit >= th)
	}

	quarter.bottom.right <- function(digit, th=90) {
		nr <- dim(digit)[1]
		nc <- dim(digit)[2]
		
		sum(digit[nr:ceiling(nr/2+1),nc:ceiling(nc/2+1)]>=th) / sum(digit >= th)
	}
	
	feature.funs <- list(
		symmetry.vertical.axis, 
		symmetry.horizontal.axis, 
		mean.point.value, 
		width.per.height,
		quarter.top.left,
		quarter.top.right,
		quarter.bottom.left,
		quarter.bottom.right
	)
	
	apply.feature.funs <- function(digit, feature.funs) {
		fv <- rep(NA,length(feature.funs))
		
		i <- 1
		for(fun in feature.funs) {
			fv[i] <- fun(digit)
			i <- i + 1
		}
		
		fv
	}
	
	num.of.features <- length(feature.funs)
	features <- matrix(NA, nrow=sum(unlist(lapply(digits,length))), ncol=num.of.features)
	labels <- rep(NA, sum(unlist(lapply(digits,length))))
	
	k <- 1
	for(digit in names(digits)) {
		for(m in digits[[digit]]) {
			labels[k] <- digit
			features[k,] <- apply.feature.funs(m, feature.funs)
			k <- k + 1
		}
	}
	
	return(list("labels" = labels, "features" = features))
}

feature.extraction.asis <- function(digits, w,h) {
	rescale.digit <- function(digit, final.height, final.width, width=NA, plot.it=FALSE) {
		if(is.vector(digit)) {
			digit <- raster(matrix((unlist(digit)), ncol=width, byrow=byrow))
		}

		Nrow2 <- final.height
		Ncol2 <- final.width

		r <- digit
		r2 <- matrix(rep(0,Nrow2 * Ncol2),ncol=Ncol2)

		Nrow <- dim(r)[1]
		Ncol <- dim(r)[2]

		row.unit <- Nrow/Nrow2
		col.unit <- Ncol/Ncol2

		f <- function(Nrow,Nrow2,Ncol,Ncol2) {
			tR <-  cbind(floor(1+(0:(Nrow2-1))*Nrow/Nrow2),floor(1+(1:Nrow2)*Nrow/Nrow2))
			tR[Nrow2,2] <- tR[Nrow2,2] - 1
			tC <- cbind(floor(1+(0:(Ncol2-1))*Ncol/Ncol2),floor(1+(1:Ncol2)*Ncol/Ncol2))
			tC[Ncol2,2] <- tC[Ncol2,2] - 1
			
			return(
				function(col,row) mean(
					r[tR[row,1]:tR[row,2],
						tC[col,1]:tC[col,2]
					])
			)
		}

		positions <- expand.grid(1:Nrow2,1:Ncol2)
		digit <- mapply(f(Nrow,Nrow2,Ncol,Ncol2),positions[,1],positions[,2])

		if(plot.it) {
			plot(raster(matrix((unlist(digit)), ncol=Ncol2)), col = grey(seq(1, 0, length = 256)))
		}
		
		return(digit)
	}
	
	num.of.features <- w*h
	features <- matrix(NA, nrow=sum(unlist(lapply(digits,length))), ncol=num.of.features)
	labels <- rep(NA, sum(unlist(lapply(digits,length))))
	
	k <- 1
	for(digit in names(digits)) {
		for(m in digits[[digit]]) {
			labels[k] <- digit
			features[k,] <- rescale.digit(m, w,h)
			k <- k + 1
		}
	}
	
	return(list("labels" = labels, "features" = features))
}

feature.extraction.neighbours <- function(digits, th=90) {

	neighbourfication <- function(m, th) {
		nc <- ncol(m)
		nr <- nrow(m)
		
		m <- ifelse(m >= th, 1, 0)
		M <- matrix(0,ncol=nc+2,nrow=nr+2)
		
		M[2:(nr+1),2:(nc+1)] <- m
		
		v <- rep(0,2^8)
		
		ones <- cbind((which(m==1)-1)%%nr+2,ceiling(which(m==1)/nr)+1)
		pos <- apply(ones,1,function(rc)sum(c(M[rc[1]-1,rc[2]-1],M[rc[1]-1,rc[2]],M[rc[1]-1,rc[2]+1],M[rc[1],rc[2]+1],M[rc[1]+1,rc[2]+1],M[rc[1]+1,rc[2]],M[rc[1]+1,rc[2]-1],M[rc[1],rc[2]-1])*2^(0:7)))
		v[pos] <- 1
		
		return(v)
	}
	
	num.of.features <- 2^8
	features <- matrix(NA, nrow=sum(unlist(lapply(digits,length))), ncol=num.of.features)
	labels <- rep(NA, sum(unlist(lapply(digits,length))))
	
	k <- 1
	for(digit in names(digits)) {
		for(m in digits[[digit]]) {
			labels[k] <- digit
			features[k,] <- neighbourfication(m, th)
			k <- k + 1
		}
	}
	
	return(list("labels" = labels, "features" = features))
}