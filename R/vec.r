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

#' @title vectorize a multidimensional array.
#'
#' @description 
#'
#' Turn a multidimensional array into a (column) vector. 
#' Turn a (typically symmetric) matrix into a (column) vector of
#' the lower triangular part. Or reverse these
#' operations.
#'
#' @param x a \code{madness} object or multidimensional array or matrix.
#' @param k the diagonal from which to subselect. 
#' @return a \code{madness} object or an array, of the vectorized array
#' or the subselected part. For the inverse operations, promotes to a
#' \code{madness} of a matrix, or a matrix.
#' @seealso \code{\link{reshapes}}, in particular \code{tril}.
#' @template etc
#' @rdname vec
#' @name vec
#' @examples 
#' y <- matrix(rnorm(16),ncol=4)
#' sy <- y + t(y)
#' vy <- vec(sy)
#' vmy <- vec(madness(sy))
#' vhy <- vech(sy)
#' vmhy <- vech(madness(sy))
NULL

#' @rdname vec
#' @aliases vec
#' @exportMethod vec
setGeneric('vec', function(x) standardGeneric('vec'))
#' @rdname vec
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
#' @rdname vec
#' @aliases vec,array-method
setMethod("vec", signature(x="array"),
					function(x) {
						dim(x) <- c(length(x),1)
						x
					})

#' @rdname vec
#' @aliases vech
#' @exportMethod vech
setGeneric('vech', function(x,k=0) standardGeneric('vech'))
#' @rdname vec
#' @aliases vech,array-method
setMethod("vech", signature(x="array"),
					function(x,k=0) {
						takeus <- row(x) >= col(x) - k
						x <- x[takeus]
						dim(x) <- c(length(x),1)
						x
					})

#' @rdname vec
#' @aliases vech,madness-method
setMethod("vech", signature(x="madness"),
					function(x,k=0) {
						xtag <- x@xtag
						val <- x@val
						takeus <- row(val) >= col(val) - k
						val <- val[takeus]
						dim(val) <- c(length(val),1)
						dvdx <- x@dvdx[which(takeus),]
						ytag <- paste0('vech(',x@ytag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, ytag=ytag, xtag=xtag, varx=varx)
					})

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
