# /usr/bin/r
#
# Copyright 2015-2015 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav
#
# This file is part of madness.
#
# madness is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# madness is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with madness.  If not, see <http://www.gnu.org/licenses/>.
#
# Created: 2015.11.22
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

library(expm)

set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}

apx_deriv <- function(xval,thefun,eps=1e-8,type=c('forward','central')) {
	type <- match.arg(type)
	yval <- thefun(xval)
	dapx <- matrix(0,length(yval),length(xval))
	for (iii in seq_len(length(xval))) {
		xalt <- xval
		xalt[iii] <- xalt[iii] + eps
		yplus <- thefun(xalt)
		dydx <- switch(type,
			forward={ (yplus - yval) / eps },
			central={
				xalt <- xval
				xalt[iii] <- xalt[iii] - eps
				yneg <- thefun(xalt)
				(yplus - yneg) / (2*eps)
		})
		dapx[,iii] <- as.numeric(dydx)
	}
	dapx
}

# evaluate the error between a numerical approximation and a computed
# value, both of which may be erroneous?
errit <- function(apx,cmp,eps) {
	merror <- abs(apx - cmp)
	rerror <- merror / pmax(eps^0.333,0.5 * (abs(apx) + abs(cmp)))
	rerror[(abs(apx) < eps^2) & (abs(cmp) < eps^2)] <- 0
	rerror
}

# now the harness
test_harness <- function(xval,thefun,scalfun=thefun,eps=1e-8) {
	xobj <- madness(val=xval,ytag='x',xtag='x')
	yobj <- thefun(xobj)
	# compute the error between the function applied to madness
	# and the function on the scalar value.
	ynum <- scalfun(xval)
	f_err <- errit(ynum,val(yobj),eps)

	# now the derivatives
	xval <- val(xobj)
  dapx <- apx_deriv(xval,scalfun,eps=eps,type='central')
	# compute error:
	dcmp <- dvdx(yobj)
	dim(dcmp) <- dim(dapx)

	d_err <- errit(dapx,dcmp,eps)
	
	retv <- max(max(abs(d_err)),max(abs(f_err)))
}

context("Basic Operations")#FOLDUP

test_that("arith functions",{#FOLDUP
	set.char.seed("dee9af9b-cb59-474f-ac3b-acd60faa8ba2")
	xval <- matrix(1 + runif(4*4),nrow=4)
	yval <- matrix(1 + runif(length(xval)),nrow=nrow(xval))
	expect_less_than(test_harness(xval,function(x) { + x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { - x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x + x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x + yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval + x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x - x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x - yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval - x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x * x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x * yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval * x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x / x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x / yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval / x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x ^ x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x ^ yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval ^ x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { x %*% x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x %*% yval }),1e-6)
	expect_less_than(test_harness(xval,function(x) { yval %*% x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { crossprod(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { crossprod(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { crossprod(x,yval) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { crossprod(yval,x) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { tcrossprod(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { tcrossprod(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { tcrossprod(x,yval) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { tcrossprod(yval,x) }),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("bind functions",{#FOLDUP
	set.char.seed("f459e4a4-2b1f-4902-9f5c-a78ee3302e96")
	xval <- matrix(1 + runif(4*4),nrow=4)
	yval <- matrix(1 + runif(length(xval)),nrow=nrow(xval))
	
	acol <- matrix(1 + runif(nrow(xval)),ncol=1)
	arow <- matrix(1 + runif(ncol(xval)),nrow=1)

	expect_less_than(test_harness(xval,function(x) { cbind(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,yval) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,acol) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,acol,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(yval,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(x,yval,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cbind(yval,x,yval) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { rbind(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,yval) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,arow) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,arow,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(yval,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(x,yval,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { rbind(yval,x,yval) }),1e-6)

	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("elwise functions",{#FOLDUP
	set.char.seed("05ffaa40-902d-430a-a47f-63938b921306")
	xval <- matrix(1 + runif(4*4),nrow=4)

	expect_less_than(test_harness(xval,function(x) { abs(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { exp(x) }),1e-6)
	expect_less_than(test_harness(abs(xval),function(x) { log(x) }),1e-6)
	expect_less_than(test_harness(abs(xval),function(x) { log10(x) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { sqrt(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { sin(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { cos(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { tan(x) },eps=1e-10),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("matwise functions",{#FOLDUP
	set.char.seed("1860a172-ba3e-4873-950a-75c9b771134d")
	zval <- matrix(0.01 + runif(4*100,min=0,max=0.05),nrow=4)
	xval <- tcrossprod(zval)

	expect_less_than(test_harness(xval,function(x) { sqrtm(x) }),1e-6)
	#expect_less_than(test_harness(xval,function(x) { logm(x) }),1e-6)
	#expect_less_than(test_harness(xval,function(x) { expm(x) }),1e-6)

	# the 'wrong way to test'
	expect_gt(test_harness(xval,function(x) { chol(x) }),1e-6)

	# chol has hidden symmetry:
	fsym <- function(x) { 0.5 * (x + t(x)) }
	expect_less_than(test_harness(xval,function(x) { chol(fsym(x)) }),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("sums functions",{#FOLDUP
	set.char.seed("25e43832-3030-40cf-acf8-fa43bd56dc09")
	xval <- matrix(1 + runif(4*4),nrow=4)

	expect_less_than(test_harness(xval,function(x) { matrix.trace(x) },function(matx) { sum(diag(matx)) }),1e-6)

	for (na.rm in c(FALSE,TRUE)) {
		expect_less_than(test_harness(xval,function(x) { colSums(x,na.rm=na.rm) }),1e-6)
		expect_less_than(test_harness(xval,function(x) { colMeans(x,na.rm=na.rm) }),1e-6)
		expect_less_than(test_harness(xval,function(x) { rowSums(x,na.rm=na.rm) }),1e-6)
		expect_less_than(test_harness(xval,function(x) { rowMeans(x,na.rm=na.rm) }),1e-6)
	}
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("outer functions",{#FOLDUP
	set.char.seed("354455b9-2b3f-40fb-b8e7-21a1302b48de")
	xval <- array(1+runif(2*3*4),dim=c(2,3,4))
	yval <- array(1+runif(3*2),dim=c(3,2))

	expect_less_than(test_harness(xval,function(x) { outer(x,x,FUN='*') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,x,FUN='+') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,x,FUN='-') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,x,FUN='/') }),1e-6)

	expect_less_than(test_harness(xval,function(x) { outer(x,yval,FUN='*') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,yval,FUN='+') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,yval,FUN='-') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(x,yval,FUN='/') }),1e-6)

	expect_less_than(test_harness(xval,function(x) { outer(yval,x,FUN='*') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(yval,x,FUN='+') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(yval,x,FUN='-') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { outer(yval,x,FUN='/') }),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("determinants",{#FOLDUP
	set.char.seed("081849a0-ab28-42ac-8d18-1963cb8a9a0a")
	xval <- matrix(1 + runif(4*4),nrow=4)

	# fuck det.
	#expect_less_than(test_harness(xval,function(x) { det(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { 
																dt <- determinant(x,logarithm=FALSE) 
																dt$modulus }),1e-6)

	expect_less_than(test_harness(xval,function(x) { 
																dt <- determinant(x,logarithm=TRUE) 
																dt$modulus }),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("reshape functions",{#FOLDUP
	set.char.seed("3d06aea4-c339-4630-a8db-3d56d6b6b687")
	xval <- matrix(1 + runif(4*4),nrow=4)
	xvec <- array(1 + runif(4*4),dim=c(16,1))

	expect_less_than(test_harness(xval,function(x) { t(x) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { vec(x) },
																function(x) { dim(x) <- c(length(x),1); x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { vech(x) },
																function(x) { x <- x[row(x) >= col(x)]; 
																dim(x) <- c(length(x),1); 
																x }),1e-6)

	expect_less_than(test_harness(xval,function(x) { diag(x) }),1e-6)
	
	expect_less_than(test_harness(xval,function(x) { dim(x) <- c(prod(dim(x)),1); x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { x[1,1,drop=FALSE] }),1e-6)
	expect_less_than(test_harness(xvec,function(x) { dim(x) <- c(prod(dim(x)),1); x }),1e-6)
	expect_less_than(test_harness(xvec,function(x) { x[1,1,drop=FALSE] }),1e-6)

	expect_less_than(test_harness(xval,function(x) { todiag(x) },
																function(x) { diag(as.numeric(x)) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { tril(x) },
																function(x) { x[row(x) < col(x)] <- 0; x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { triu(x) },
																function(x) { x[row(x) > col(x)] <- 0; x }),1e-6)
	

	xval <- array(1 + runif(2*3*4*5),dim=c(2,3,4,5))
	expect_less_than(test_harness(xval,function(x) { aperm(x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { aperm(x,c(2,1,3,4)) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { aperm(x,c(4,3,2,1)) }),1e-6)

	# need better tests of these!
	xval <- array(1 + runif(2*3*4*5),dim=c(2,3,4,5))
	xdim <- dim(xval)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(1)) },function(x) { x }),1e-6)
	expect_less_than(test_harness(xval,function(x) { repto(x,xdim) },function(x) { x }),1e-6)

	xval <- array(1 + runif(3*7),dim=c(3,7))
	xdim <- dim(xval)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(2,1)) },function(x) { rbind(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(3,1)) },function(x) { rbind(x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(5,1)) },function(x) { rbind(x,x,x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(1,2)) },function(x) { cbind(x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(1,4)) },function(x) { cbind(x,x,x,x) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(2,3)) },function(x) { x2 <- rbind(x,x) ; cbind(x2,x2,x2) }),1e-6)
	expect_less_than(test_harness(xval,function(x) { blockrep(x,c(5,2)) },function(x) { x5 <- rbind(x,x,x,x,x) ; cbind(x5,x5) }),1e-6)
	# now alternate dimensions?

	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("solve functions",{#FOLDUP
	set.char.seed("232ba1a1-7751-40be-866f-a8e2122c2ace")
	prex <- matrix(1 + runif(100*8),ncol=8)
	xval <- crossprod(prex)
	yval <- array(1 + runif(nrow(xval)),dim=c(nrow(xval),1))

	expect_less_than(test_harness(xval,function(x) { solve(x) },eps=1e-6),1e-6)
	expect_less_than(test_harness(xval,function(x) { solve(x,yval) },eps=1e-6),1e-6)
	expect_less_than(test_harness(xval,function(x) { solve(x,x[,1,drop=FALSE]) },eps=1e-6),1e-5)
	expect_less_than(test_harness(xval,function(x) { solve(xval,x[,1,drop=FALSE]) },eps=1e-6),1e-5)
	expect_less_than(test_harness(xval,function(x) { solve(x,as.numeric(yval)) },eps=1e-6),1e-6)

	# numeric left only works in 1d case. so be degenerate
	xval <- array(rnorm(1),dim=c(1,1))
	yval <- runif(1)
	expect_less_than(test_harness(xval,function(x) { solve(yval,x) },eps=1e-6),1e-6)

	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("norm functions",{#FOLDUP
	set.char.seed("e74da7ce-92f1-41ee-96cd-fe8201da753f")
	xval <- matrix(1 + runif(4*4),nrow=4)

	expect_less_than(test_harness(xval,function(x) { norm(x) }),1e-6)

	expect_less_than(test_harness(xval,function(x) { norm(x,'O') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'o') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'1') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'I') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'i') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'M') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'m') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'F') }),1e-6)
	expect_less_than(test_harness(xval,function(x) { norm(x,'f') }),1e-6)
# Matrix::norm does not support type '2'
	expect_less_than(test_harness(xval,function(x) { norm(x,'2') },
		function(x) { base::norm(x,'2') }),1e-6)

	expect_less_than(test_harness(xval,function(x) { maxeig(x) },
																function(x) { 
																	usv <- svd(x,1,1)
																	as.numeric(usv$d[1])
																}),1e-6)
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD

#UNFOLD

context("Requiring Chain Rule")#FOLDUP

test_that("round one",{#FOLDUP
	set.char.seed("dee9af9b-cb59-474f-ac3b-acd60faa8ba2")
	xval <- matrix(1 + runif(4*4),nrow=4)
	yval <- matrix(1 + runif(length(xval)),nrow=nrow(xval))
	expect_less_than(test_harness(xval,function(x) { norm(crossprod(x),'O') },eps=1e-7),1e-5)
	expect_less_than(test_harness(xval,function(x) { norm(crossprod(x^x),'M') },eps=1e-7),1e-5)
	expect_less_than(test_harness(xval,function(x) { norm(abs(x) %*% t(x),'I') },eps=1e-7),1e-5)

	expect_less_than(test_harness(xval,function(x) { tcrossprod(sin(x)) },eps=1e-07),1e-5)
	expect_less_than(test_harness(xval,function(x) { cos(crossprod(sin(x))) },eps=1e-07),1e-5)

	# sentinel:
	expect_true(TRUE)
})#UNFOLD

#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
