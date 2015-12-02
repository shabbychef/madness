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
# Created: 2015.11.18
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @include AllClass.r
#' @include utils.r
NULL

#' Basic Reshape Operations
#'
#' @include AllClass.r
#' @param x \code{madness} object.
#' @param value an array of the new dimensions of the object value.
#' @inheritParams Matrix::tril
#' @param k the index of the diagonal number from which to extract.\code{madness} object.
#' @name reshapes
#' @template etc
NULL

# transpose#FOLDUP

#' @rdname reshapes
#' @aliases t
#' @exportMethod t
setGeneric('t', function(x) standardGeneric('t'))
#' @rdname reshapes
#' @aliases t,madness-method
setMethod("t", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- t(x@val)
						# could use the commutation matrix, but faster
						# probably to reorder...
						newidx <- (row(val) - 1) * ncol(val) + col(val)
						dvdx <- x@dvdx[newidx,,drop=FALSE] 
						ytag <- paste0('t(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases vec
#' @exportMethod vec
setGeneric('vec', function(x) standardGeneric('vec'))
#' @rdname reshapes
#' @aliases vec,madness-method
setMethod("vec", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- x@val
						dim(val) <- c(prod(dim(val)),1)
						dvdx <- x@dvdx
						ytag <- paste0('vec(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases vech
#' @exportMethod vech
setGeneric('vech', function(x) standardGeneric('vech'))
#' @rdname reshapes
#' @aliases vech,madness-method
setMethod("vech", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- x@val
						takeus <- row(val) >= col(val)
						val <- val[takeus]
						dim(val) <- c(length(val),1)
						dvdx <- x@dvdx[which(takeus),]
						ytag <- paste0('vech(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases diag,madness-method
setMethod("diag", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- x@val
						takeus <- row(val) == col(val)
						val <- val[takeus]
						dim(val) <- c(length(val),1)
						dvdx <- x@dvdx[which(takeus),]
						ytag <- paste0('diag(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})


#' @rdname reshapes
#' @aliases tril
#' @exportMethod tril
setGeneric('tril', function(x,k=0,...) standardGeneric('tril'))
# 2FIX: must I check if this has already been defined as a generic?
#' @rdname reshapes
#' @aliases tril,madness-method
setMethod("tril", signature(x="madness",k='ANY'),
					function(x,k=0) {
						xtag <- x@xtag
						val <- x@val
						takeus <- row(val) >= col(val) - k
						val[!takeus] <- 0
						dvdx <- x@dvdx
						dvdx[which(!takeus),] <- 0
						ytag <- paste0('tril(',x@ytag,', ',k,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases triu
#' @exportMethod triu
setGeneric('triu', function(x,k=0,...) standardGeneric('triu'))
#' @rdname reshapes
#' @aliases triu,madness-method
setMethod("triu", signature(x="madness",k='ANY'),
					function(x,k=0) {
						xtag <- x@xtag
						val <- x@val
						takeus <- row(val) <= col(val) - k
						val[!takeus] <- 0
						dvdx <- x@dvdx
						dvdx[which(!takeus),] <- 0
						ytag <- paste0('triu(',x@ytag,', ',k,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases todiag
#' @exportMethod todiag
setGeneric('todiag', function(x) standardGeneric('todiag'))
#' @rdname reshapes
#' @aliases todiag,madness-method
setMethod("todiag", signature(x="madness"),
					function(x) {
						xtag <- x@xtag
						val <- diag(as.numeric(x@val))
						takeus <- row(val) == col(val)
						dvdx <- matrix(0,nrow=length(takeus),ncol=ncol(x@dvdx))
						dvdx[which(takeus),] <- x@dvdx
						ytag <- paste0('todiag(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname reshapes
#' @aliases dim<-,madness,ANY-method
setMethod("dim<-", signature(x="madness",value="ANY"),
					function(x,value) {
						xtag <- x@xtag
						val <- x@val
						dim(val) <- value
						dvdx <- x@dvdx
						ytag <- paste0('reshape(',x@ytag,', ',
													 as.character(enquote(value))[2],
													 ')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

# see http://stackoverflow.com/a/8057007/164611
#' Extract parts of a \code{madness} value.
#'
#' @param x a \code{madness} object.
#' @param j,...  further indices specifying elements to extract or 
#' replace.  
#' @inheritParams base::`[`
#' @name [
#' @aliases [,madness,ANY,ANY,ANY-method
#' @docType methods
#' @rdname extract-methods
#' @template etc
setMethod("[", signature(x="madness",i="ANY",j="ANY"),
					function(x,i,j,...,drop) {
						getidx <- vector(length=length(x@val))
						dim(getidx) <- dim(x@val)
						getidx[i,j,...] <- TRUE
						val <- x@val[i,j,...,drop=FALSE]
						dvdx <- x@dvdx[which(getidx),,drop=FALSE]

						retv <- new("madness", val=val, dvdx=dvdx,
												xtag=x@xtag,
												y=paste0(x@ytag,'[...]'),
												varx=x@varx)
					})

# 2FIX: define the setter method? but, wait we don't want to be
# able to poke elements willy nilly, right?

#UNFOLD

# 2FIX: add aperm.

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
