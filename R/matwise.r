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
# Created: 2015.11.23
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @include AllClass.r
#' @include utils.r
NULL

#' @title Matrix-wise Multivariate Operations
#'
#' @description 
#'
#' Element-wise multivariate operations. 
#'
#' @details
#'
#' These operations are scalar-to-scalar operations applied to each element of a 
#' multidimensional array. 
#'
#' @include AllClass.r
#' @inheritParams expm::sqrtm
#' @inheritParams base::norm
#' @param x \code{madness} object.
#' @param ... further arguments passed to or from other methods.
#' @name matwise
#' @template etc
NULL

# 2FIX: add logm, expm

#' @name matwise
#' @rdname matwise
#' @aliases sqrtm
#' @exportMethod sqrtm
setGeneric('sqrtm', signature="x", function(x) standardGeneric('sqrtm'))
#' @rdname matwise
#' @aliases sqrtm,madness-method
setMethod("sqrtm", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- expm::sqrtm(x@val)
						scalby <- (t(val) %x% diag(dim(val)[2])) + (diag(dim(val)[2]) %x% val)
						dvdx <- solve(scalby,x@dvdx)
						ytag <- paste0('sqrtm(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @name matwise
#' @rdname matwise
#' @aliases maxeig
#' @exportMethod maxeig
setGeneric('maxeig', signature="x", function(x) standardGeneric('maxeig'))
#' @rdname matwise
#' @aliases maxeig,madness-method
setMethod("maxeig", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						vvals <- svd(x@val,nu=1,nv=1)
						val <- matrix(vvals$d[1])
						dvdx <- (t(vvals$v) %x% t(vvals$u)) %*% x@dvdx
						ytag <- paste0('maxeig(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

.normit <- function(x,type='One') {
	type <- tolower(substr(type,0,1))
	stopifnot(type %in% c('o','1','2','i','f','m'))
	xtag <- x@xtag
	#val <- norm(x@val,type=type)
	nr <- nrow(x@val)
	nc <- ncol(x@val)
	if (type %in% c('o','1')) {
# DRY: colsums
		cs <- colSums(abs(x@val))
		midx <- which.max(cs)
		val <- cs[midx]
		dvdx <- array(as.numeric(sign(x@val[,midx])),dim=c(1,nr)) %*% x@dvdx[which(col(x@val)==midx),,drop=FALSE]
	} else if (type == 'i') {
# DRY: rowsums
		rs <- rowSums(abs(x@val))
		midx <- which.max(rs)
		val <- rs[midx]
		dvdx <- array(as.numeric(sign(x@val[midx,])),dim=c(1,nc)) %*% x@dvdx[which(row(x@val)==midx),,drop=FALSE]
	} else if (type == 'f') {
# DRY:
		val <- sqrt(sum(x@val^2))
		dvdx <- (1/val) * (array(x@val,dim=c(1,length(x@val))) %*% x@dvdx)
	} else if (type == 'm') {
		val <- max(abs(x@val))
		midx <- which.max(abs(x@val))
		dvdx <- sign(x@val[midx]) * x@dvdx[midx,,drop=FALSE]
	} else {
		# DRY: farm this off to maxeig?
		vvals <- svd(x@val,nu=1,nv=1)
		val <- matrix(vvals$d[1])
		dvdx <- (t(vvals$v) %x% t(vvals$u)) %*% x@dvdx
	}

	ytag <- paste0('norm(',x@ytag,", '",type,"')")
	varx <- x@varx
	val <- array(val,dim=c(1,1))
	dvdx <- array(dvdx,dim=c(1,length(dvdx)))

	new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
}

#' @name matwise
#' @rdname matwise
#' @aliases norm
#' @exportMethod norm
setGeneric('norm', function(x,type) standardGeneric('norm'))
#' @rdname matwise
#' @aliases maxeig,madness-method
setMethod("norm", signature(x="madness",type='missing'), function(x) .normit(x))
#' @rdname matwise
#' @aliases maxeig,madness-method
setMethod("norm", signature(x="madness",type='ANY'), .normit)

#' @rdname matwise
#' @export 
#' @method chol madness
#' @usage chol(x,...)
#' @aliases chol
chol.madness <- function(x,...) {
 	xtag <- x@xtag
 	val <- chol(x@val)
	scalby <- t(val) %x% diag(dim(val)[2]) 
	scalby <- scalby + .do_commutator(t(val),scalby)
	Lm <- matrixcalc::L.matrix(dim(val)[2])
	scalby <- Lm %*% (scalby %*% t(Lm))
	dvdx <- .do_commutator(val,t(Lm) %*% solve(scalby,Lm %*% x@dvdx))
	ytag <- paste0('chol(',x@ytag,')')
	varx <- x@varx

	new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
