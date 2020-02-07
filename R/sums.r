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

#' @include AllClass.r
#' @include utils.r
NULL

## MM: do _NOT_  setGeneric() on existing functions! (==> conflict other pkg methods!)
# setGeneric('colSums', function(x,na.rm,dims) standardGeneric('colSums'))
# setGeneric('colMeans', function(x,na.rm=FALSE,dims=1) standardGeneric('colMeans'))
# setGeneric('rowSums', function(x,na.rm,dims) standardGeneric('rowSums'))
# setGeneric('rowMeans', function(x,na.rm,dims) standardGeneric('rowMeans'))


#' @title Form Row and Column Sums and Means
#'
#' @description
#'
#' Form Row and Column Sums and Means for \code{madness} objects.
#'
#' @include AllClass.r
#' @param x \code{madness} object.
#' @inheritParams base::colSums 
#' @inheritParams base::rowSums 
#' @inheritParams base::colMeans
#' @inheritParams base::rowMeans
#' @inheritParams Matrix::colSums 
#' @return a \code{madness} object. Note that the sums are flattened to a
#' column vector.
#' @name colsums
#' @exportMethod colSums
#' @aliases colSums colSums,madness-method
#' @template etc
#' @rdname colsums
setMethod("colSums", signature(x="madness"),
					function(x,na.rm=FALSE,dims=1) {
						xtag <- x@xtag
						val <- base::colSums(x@val,na.rm=na.rm,dims=dims)
						val <- array(val,dim=c(length(val),1))
						dvdx <- array(x@dvdx,dim=c(dim(x@val),ncol(x@dvdx))) 
						if (na.rm) {
							# so colSums do not puke later
							badiii <- which(is.na(x@val) | is.nan(x@val))
							odim <- dim(dvdx)
							dim(dvdx) <- c(length(x@val),odim[length(odim)])
							dvdx[badiii,] <- 0
							dim(dvdx) <- odim
						}
						dvdx <- matrix(base::colSums(dvdx,dims=dims),ncol=ncol(x@dvdx))
						vtag <- paste0('colSums(',x@vtag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, vtag=vtag, xtag=xtag, varx=varx)
					})
#' @exportMethod colMeans
#' @aliases colMeans colMeans,madness-method
#' @rdname colsums
setMethod("colMeans", signature(x="madness"),
					function(x,na.rm=FALSE,dims=1) {
						xtag <- x@xtag
						val <- colMeans(x@val,na.rm=na.rm,dims=dims)
						val <- array(val,dim=c(length(val),1))
						dvdx <- array(x@dvdx,dim=c(dim(x@val),ncol(x@dvdx))) 
						if (na.rm) {
							isok <- (!is.na(x@val) & !is.nan(x@val))
							deno <- colSums(isok,dims=dims)
							odim <- dim(dvdx)
							dim(dvdx) <- c(length(x@val),odim[length(odim)])
							dvdx[which(!isok),] <- 0
							dim(dvdx) <- odim
							dvdx <- as.numeric(1/deno) * matrix(colSums(dvdx,dims=dims),ncol=ncol(x@dvdx))
						} else {
							dvdx <- matrix(colMeans(dvdx,dims=dims),ncol=ncol(x@dvdx))
						}
						vtag <- paste0('colMeans(',x@vtag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, vtag=vtag, xtag=xtag, varx=varx)
					})

#' @exportMethod rowSums
#' @aliases rowSums rowSums,madness-method
#' @rdname colsums
setMethod("rowSums", signature(x="madness"),
					function(x,na.rm=FALSE,dims=1) {
						xtag <- x@xtag
						val <- base::rowSums(x@val,na.rm=na.rm,dims=dims)
						val <- array(val,dim=c(length(val),1))

						dvdx <- array(t(x@dvdx),dim=c(ncol(x@dvdx),dim(x@val)))
						if (na.rm) {
							# so rowSums do not puke later
							badiii <- which(is.na(x@val) | is.nan(x@val))
							odim <- dim(dvdx)
							dim(dvdx) <- c(odim[1],length(x@val))
							dvdx[,badiii] <- 0
							dim(dvdx) <- odim
						}
						dvdx <- t(matrix(base::rowSums(dvdx,dims=dims+1),nrow=ncol(x@dvdx)))
						vtag <- paste0('rowSums(',x@vtag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, vtag=vtag, xtag=xtag, varx=varx)
					})
#' @exportMethod rowMeans
#' @aliases rowMeans rowMeans,madness-method
#' @rdname colsums
setMethod("rowMeans", signature(x="madness"),
					function(x,na.rm=FALSE,dims=1) {
						xtag <- x@xtag
						val <- rowMeans(x@val,na.rm=na.rm,dims=dims)
						val <- array(val,dim=c(length(val),1))
						dvdx <- array(t(x@dvdx),dim=c(ncol(x@dvdx),dim(x@val)))
						if (na.rm) {
							isok <- (!is.na(x@val) & !is.nan(x@val))
							deno <- rowSums(isok,dims=dims)
							odim <- dim(dvdx)
							dim(dvdx) <- c(odim[1],length(x@val))
							dvdx[,which(!isok)] <- 0
							dim(dvdx) <- odim
							dvdx <- as.numeric(1/deno) * t(matrix(base::rowSums(dvdx,dims=dims+1),nrow=ncol(x@dvdx)))
						} else {
							dvdx <- t(matrix(base::rowMeans(dvdx,dims=dims+1),nrow=ncol(x@dvdx)))
						}
						vtag <- paste0('rowMeans(',x@vtag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, vtag=vtag, xtag=xtag, varx=varx)
					})


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
