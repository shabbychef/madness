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
# Created: 2015.11.17
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @include AllClass.r
#' @include utils.r
NULL

#' @title Basic Arithmetic Operations.
#'
#' @description
#'
#' These perform basic arithmetic operations on \code{madness} objects: unary
#' plus and minus, addition, subtraction, multiplication, division and power.
#'
#' @examples 
#' set.seed(123)
#' y <- array(rnorm(3*3),dim=c(3,3))
#' dy <- matrix(rnorm(length(y)*2),ncol=2)
#' dx <- crossprod(matrix(rnorm(ncol(dy)*100),nrow=100))
#' obj0 <- madness(val=y,ytag='y',xtag='x',dvdx=dy,varx=dx)
#'
#' anobj <- + obj0
#' anobj <- - obj0
#' anobj <- 6 - obj0
#' anobj <- 1 + obj0
#' anobj <- obj0 - 3
#'
#' obj1 <- obj0 ^ 2
#' anobj <- (0.3 * obj0) + (5.1 * obj1)
#'
#' anobj <- 2 ^ obj0
#' anobj <- obj1 ^ obj0
#'
#' @include AllClass.r
#' @param e1,e2 \code{madness} or numeric values
#' @template etc
#' @rdname arithops
#' @name arithops
NULL

# unary plus#FOLDUP

#' @rdname arithops
#' @aliases +,madness-class
setMethod("+", signature(e1="madness",e2="missing"),
					function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val
						dvdx <- e1@dvdx 
						ytag <- paste0('+',e1@ytag)
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})
#UNFOLD

# unary minus#FOLDUP
#' @rdname arithops
#' @aliases -,madness-class
setMethod("-", signature(e1="madness",e2="missing"),
					function(e1,e2) {
						xtag <- e1@xtag
						val <- - e1@val
						dvdx <- - e1@dvdx 
						ytag <- paste0('-',e1@ytag)
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})
#UNFOLD

# addition#FOLDUP
#' @rdname arithops
#' @aliases +,madness,madness-class
setMethod("+", signature(e1="madness",e2="madness"),
					function(e1,e2) {
						xtag <- .check_common_xtag(e1,e2)
						val <- e1@val + e2@val
						dvdx <- e1@dvdx + e2@dvdx
						ytag <- paste0('(',e1@ytag,' + ',e2@ytag,')')
						varx <- .get_a_varx(e1,e2)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

mplusn <- function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val + e2
						dvdx <- e1@dvdx 
						ytag <- paste0('(',e1@ytag,' + numeric)')
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}
#' @rdname arithops
#' @aliases +,madness,numeric-class
setMethod("+", signature(e1="madness",e2="numeric"),mplusn)
#' @rdname arithops
#' @aliases +,madness,array-class
setMethod("+", signature(e1="madness",e2="array"),mplusn)

nplusm <- function(e1,e2) {
						xtag <- e2@xtag
						val <- e1 + e2@val
						dvdx <- e2@dvdx 
						ytag <- paste0('(numeric + ',e2@ytag,')')
						varx <- e2@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases +,numeric,madness-class
setMethod("+", signature(e1="numeric",e2="madness"),nplusm)
#' @rdname arithops
#' @aliases +,array,madness-class
setMethod("+", signature(e1="array",e2="madness"),nplusm)
#UNFOLD

# subtraction#FOLDUP
#' @rdname arithops
#' @aliases -,madness,madness-class
setMethod("-", signature(e1="madness",e2="madness"),
					function(e1,e2) {
						xtag <- .check_common_xtag(e1,e2)
						val <- e1@val - e2@val
						dvdx <- e1@dvdx - e2@dvdx
						ytag <- paste0('(',e1@ytag,' - ',e2@ytag,')')
						varx <- .get_a_varx(e1,e2)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

mminusn <- function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val - e2
						dvdx <- e1@dvdx 
						ytag <- paste0('(',e1@ytag,' - numeric)')
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases -,madness,numeric-class
setMethod("-", signature(e1="madness",e2="numeric"), mminusn)
#' @rdname arithops
#' @aliases -,madness,array-class
setMethod("-", signature(e1="madness",e2="array"), mminusn)

nminusm <- function(e1,e2) {
						xtag <- e2@xtag
						val <- e1 - e2@val
						dvdx <- - e2@dvdx 
						ytag <- paste0('(numeric - ',e2@ytag,')')
						varx <- e2@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases -,numeric,madness-class
setMethod("-", signature(e1="numeric",e2="madness"),nminusm)
#' @rdname arithops
#' @aliases -,array,madness-class
setMethod("-", signature(e1="array",e2="madness"),nminusm)
#UNFOLD

# scalar and recycled multiplication#FOLDUP
#' @rdname arithops
#' @aliases *,madness,madness-class
setMethod("*", signature(e1="madness",e2="madness"),
					function(e1,e2) {
						xtag <- .check_common_xtag(e1,e2)
						val <- e1@val * e2@val
						dvdx <- e1@dvdx * as.numeric(e2@val) + as.numeric(e1@val) * e2@dvdx
						ytag <- paste0('(',e1@ytag,' * ',e2@ytag,')')
						varx <- .get_a_varx(e1,e2)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

mtimesn <- function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val * e2
						dvdx <- e1@dvdx * as.numeric(e2)
						ytag <- paste0('(',e1@ytag,' * numeric)')
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases *,madness,numeric-class
setMethod("*", signature(e1="madness",e2="numeric"), mtimesn)
#' @rdname arithops
#' @aliases *,madness,array-class
setMethod("*", signature(e1="madness",e2="array"), mtimesn)

ntimesm <- function(e1,e2) {
						xtag <- e2@xtag
						val <- e1 * e2@val
						dvdx <- as.numeric(e1) * e2@dvdx 
						ytag <- paste0('(numeric * ',e2@ytag,')')
						varx <- e2@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases *,numeric,madness-class
setMethod("*", signature(e1="numeric",e2="madness"), ntimesm)
#' @rdname arithops
#' @aliases *,array,madness-class
setMethod("*", signature(e1="array",e2="madness"), ntimesm)
#UNFOLD

# scalar and recycled division#FOLDUP
#' @rdname arithops
#' @aliases /,madness,madness-class
setMethod("/", signature(e1="madness",e2="madness"),
					function(e1,e2) {
						xtag <- .check_common_xtag(e1,e2)
						val <- e1@val / e2@val
						# lo d hi minus hi d lo over the square of whats below
						# or (d hi / lo) - (hi/lo^2) dlo
						dvdx <- (e1@dvdx / as.numeric(e2@val)) - as.numeric((val / e2@val)) * e2@dvdx
						ytag <- paste0('(',e1@ytag,' / ',e2@ytag,')')
						varx <- .get_a_varx(e1,e2)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

mbyn <- function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val / e2
						dvdx <- e1@dvdx / as.numeric(e2)
						ytag <- paste0('(',e1@ytag,' / numeric)')
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases /,madness,numeric-class
setMethod("/", signature(e1="madness",e2="numeric"), mbyn)
#' @rdname arithops
#' @aliases /,madness,array-class
setMethod("/", signature(e1="madness",e2="array"), mbyn)

nbym <- function(e1,e2) {
						xtag <- e2@xtag
						val <- e1 / e2@val
						dvdx <- - as.numeric((val / e2@val)) * e2@dvdx 
						ytag <- paste0('(numeric / ',e2@ytag,')')
						varx <- e2@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}
	
#' @rdname arithops
#' @aliases /,numeric,madness-class
setMethod("/", signature(e1="numeric",e2="madness"),nbym)
#' @rdname arithops
#' @aliases /,array,madness-class
setMethod("/", signature(e1="array",e2="madness"),nbym)
#UNFOLD

# scalar and recycled power#FOLDUP
#' @rdname arithops
#' @aliases ^,madness,madness-class
setMethod("^", signature(e1="madness",e2="madness"),
					function(e1,e2) {
						xtag <- .check_common_xtag(e1,e2)
						val <- e1@val ^ e2@val
						dvdx <- as.numeric((e1@val ^ (e2@val - 1)) * e2@val) * e1@dvdx +
							as.numeric(val * log(e1@val)) * e2@dvdx
						ytag <- paste0('(',e1@ytag,' ^ ',e2@ytag,')')
						varx <- .get_a_varx(e1,e2)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

mtothen <- function(e1,e2) {
						xtag <- e1@xtag
						val <- e1@val ^ e2
						dvdx <- as.numeric(as.numeric(e2) * (e1@val ^ (e2-1))) * e1@dvdx 
						ytag <- paste0('(',e1@ytag,' ^ numeric)')
						varx <- e1@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases ^,madness,numeric-class
setMethod("^", signature(e1="madness",e2="numeric"),mtothen)
#' @rdname arithops
#' @aliases ^,madness,array-class
setMethod("^", signature(e1="madness",e2="array"),mtothen)

ntothem <- function(e1,e2) {
						xtag <- e2@xtag
						val <- e1 ^ e2@val
						dvdx <- as.numeric(val * log(e1)) * e2@dvdx
						ytag <- paste0('(numeric ^ ',e2@ytag,')')
						varx <- e2@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					}

#' @rdname arithops
#' @aliases ^,numeric,madness-class
setMethod("^", signature(e1="numeric",e2="madness"),ntothem)
#' @rdname arithops
#' @aliases ^,array,madness-class
setMethod("^", signature(e1="array",e2="madness"),ntothem)
#UNFOLD

#' @title Basic Matrix Arithmetic Operations.
#'
#' @description
#'
#' These perform basic matrix arithmetic on \code{madness} objects: matrix
#' multiplication, cross product, Kronecker product.
#'
#' @examples 
#' set.seed(123)
#' y <- array(rnorm(3*3),dim=c(3,3))
#' dy <- matrix(rnorm(length(y)*2),ncol=2)
#' dx <- crossprod(matrix(rnorm(ncol(dy)*100),nrow=100))
#' obj0 <- madness(val=y,ytag='y',xtag='x',dvdx=dy,varx=dx)
#'
#' anobj <- obj0 %*% obj0
#' anobj <- crossprod(obj0,obj0)
#' anobj <- tcrossprod(obj0,obj0)
#' # NYI: 
#' # anobj <- obj0 %x% obj0
#'
#' @include AllClass.r
#' @param x,y \code{madness} or numeric matrix values.
#' @template etc
#' @rdname marithops
#' @name marithops
NULL

# matrix multiplication!#FOLDUP
#' @rdname marithops
#' @aliases %*%,madness,madness-class
setMethod("%*%", signature(x="madness",y="madness"),
					function(x,y) {
						xtag <- .check_common_xtag(x,y)
						val <- x@val %*% y@val
						dvdx <- (diag(dim(val)[2]) %x% x@val) %*% y@dvdx +
							(t(y@val) %x% diag(dim(val)[1])) %*% x@dvdx

						ytag <- paste0('(',x@ytag,' %*% ',y@ytag,')')
						varx <- .get_a_varx(x,y)

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname marithops
#' @aliases %*%,madness,array-class
setMethod("%*%", signature(x="madness",y="array"),
					function(x,y) {
						xtag <- x@xtag
						val <- x@val %*% y
						dvdx <- (t(y) %x% diag(dim(val)[1])) %*% x@dvdx

						ytag <- paste0('(',x@ytag,' %*% numeric)')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#' @rdname marithops
#' @aliases %*%,array,madness-class
setMethod("%*%", signature(x="array",y="madness"),
					function(x,y) {
						xtag <- y@xtag
						val <- x %*% y@val
						dvdx <- (diag(dim(val)[2]) %x% x) %*% y@dvdx 

						ytag <- paste0('(numeric  %*% ',y@ytag,')')
						varx <- y@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})
#UNFOLD

# cross product!#FOLDUP

#' @param ... ignored here.
#' @name marithops
#' @rdname marithops
#' @aliases crossprod
#' @exportMethod crossprod
setGeneric('crossprod', function(x,y) standardGeneric('crossprod'))
#' @rdname marithops
#' @aliases crossprod,madness,madness-method
setMethod("crossprod", signature(x="madness",y="madness"),
					function(x,y) { t(x) %*% y })

#' @rdname marithops
#' @aliases crossprod,madness,ANY-method
setMethod("crossprod", signature(x="madness",y="ANY"),
					function(x,y) { t(x) %*% y })

#' @rdname marithops
#' @aliases crossprod,madness,missing-method
setMethod("crossprod", signature(x="madness",y="missing"),
					function(x,y) { t(x) %*% x })

#' @rdname marithops
#' @aliases crossprod,ANY,madness-method
setMethod("crossprod", signature(x="ANY",y="madness"),
					function(x,y) { t(x) %*% y })

#' @name marithops
#' @rdname marithops
#' @aliases tcrossprod
#' @exportMethod tcrossprod
setGeneric('tcrossprod', function(x,y) standardGeneric('tcrossprod'))
#' @rdname marithops
#' @aliases crossprod,madness,madness-method
setMethod("tcrossprod", signature(x="madness",y="madness"),
					function(x,y) { x %*% t(y) })

#' @rdname marithops
#' @aliases tcrossprod,madness,ANY-method
setMethod("tcrossprod", signature(x="madness",y="ANY"),
					function(x,y) { x %*% t(y) })

#' @rdname marithops
#' @aliases tcrossprod,madness,missing-method
setMethod("tcrossprod", signature(x="madness",y="missing"),
					function(x,y) { x %*% t(x) })

#' @rdname marithops
#' @aliases tcrossprod,ANY,madness-method
setMethod("tcrossprod", signature(x="ANY",y="madness"),
					function(x,y) { x %*% t(y) })
#UNFOLD

# 2FIX: 
# add kron?
# outer product?
# kronecker sum?
# svd!

## pattern match off this:
## http://stackoverflow.com/q/33548341/164611

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r