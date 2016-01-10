# /usr/bin/r
#
# Copyright 2015-2016 Steven E. Pav. All Rights Reserved.
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
# Created: 2016.01.09
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

#' @include AllClass.r
#' @include utils.r
NULL

#' @title Singular value decomposition.
#'
#' @description 
#'
#' Compute the singular value decomposition of a matrix.
#'
#' @details
#'
#' The singular value decomposition of the matrix \eqn{X}{X} is
#' \deqn{X = U D V',}{X = U D V',}
#' where \eqn{U} and \eqn{V} are orthogonal, \eqn{V'} is \eqn{V}
#' transposed, and \eqn{D} is a diagonal matrix with the singular
#' values on the diagonal.
#'
#' @include AllClass.r
#' @inheritParams base::svd
#' @param x \code{madness} object.
#' @param nd the number of singular values to compute, and return in
#' \code{d}. This affects how many singular vectors are computed, and
#' is included for efficiency.
#' @return a list with components
#' \describe{
#'  \item{d}{a \code{madness} object of a vector
#'  containing the singular values of \code{x}, of length \code{min(n,p)}.}
#'  \item{u}{a \code{madness} object of a matrix whose
#'  columns contain the left singular vectors of \code{x},
#'  present if \code{nu > 0}. Dimension \code{c(n,nu)}.}
#'  \item{v}{a \code{madness} object of a matrix whose
#'  columns contain the right singular vectors of \code{x},
#'  present if \code{nv > 0}. Dimension \code{c(p,nv)}.}
#' }
#' @references
#'
#' Papadopoulo, Th\'{e}odore and Lourakis, Manolis I. A. "Estimating the
#' Jacobian of the Singular Value Decomposition: Theory and Applications."
#' ECCV 2000, LNCS 1842, pp 554-570 (2000).
#' \url{https://www.ics.forth.gr/_publications/2000_eccv_SVD_jacobian.pdf}
#'
#' Kato, Tosio. "Perturbation Theory for Linear Operators."
#' Springer (1995).
#' \url{http://www.maths.ed.ac.uk/~aar/papers/kato1.pdf}
#'
#' @name svd
#' @template etc
NULL

#' @name svd
#' @rdname svd
#' @exportMethod svd
setGeneric('svd', function(x,nu,nv,nd,...) standardGeneric('svd'))
#' @rdname svd
#' @aliases svd,madness-method
setMethod("svd", signature(x="madness"),
					function(x,nu=min(n,p),nv=min(n,p),nd=min(n,p),...) {
						xtag <- x@xtag
						# unfortunately, to compute the derivative of the singular values,
						# you have to compute _all_ the singular vectors?
						dms <- dim(x@val)
						n <- dms[1]
						p <- dms[2]
						reqnu <- max(nu,nd)
						reqnv <- max(nv,nd)
						vvals <- svd(x@val,nu=reqnu,nv=reqnv)
						# compute d.
						val <- matrix(vvals$d)
						# do an lapply and do.call rbind  here ... 
						dvdx <- (t(vvals$v) %x% t(vvals$u)) %*% x@dvdx
						vtag <- paste0('maxeig(',x@vtag,')')
						varx <- x@varx

						new("madness", val=val, dvdx=dvdx, vtag=vtag, xtag=xtag, varx=varx)
					})


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
