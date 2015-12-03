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
# Created: 2015.11.16
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

# documenting S4 object:
# http://r-pkgs.had.co.nz/man.html#man-classes

#' @title madness class
#'
#' @description 
#'
#' An S4 class to enable forward differentiation of multivariate computations.
#' Think of \sQuote{madness} as \sQuote{multivariate automatic differentiation -ness.}
#' There is also a constructor method for \code{madness} objects, and a 
#' wrapper method.
#'
#' @details
#'
#' A \code{madness} object contains a (multidimensional) 
#' value, and the derivative of that with respect to some independent
#' variable. The purpose is to simplify computation of multivariate 
#' derivatives, especially for use in the Delta method. Towards this
#' usage, one may store the covariance of the independent variable
#' in the object as well, from which the approximate variance-covariance
#' matrix can easily be computed. See \code{\link{vcov}}.
#'
#' Note that derivatives are all implicitly 'flattened'. That is,
#' when we talk of the derivative of \eqn{i \times j}{i x j} 
#' matrix \eqn{Y}{Y} with respect to \eqn{m \times n}{m x n}
#' matrix \eqn{X}{X}, we mean the derivative of the \eqn{ij}{ij}
#' vector \eqn{\mathrm{vec}\left(Y\right)}{vec(Y)} with
#' respect to the \eqn{mn}{mn} vector
#' \eqn{\mathrm{vec}\left(X\right)}{vec(X)}. Moreover,
#' derivatives follow the 'numerator layout' convention: 
#' this derivative is a \eqn{ij \times mn}{ij x mn} matrix
#' whose first column is the derivative of 
#' \eqn{\mathrm{vec}\left(Y\right)}{vec(Y)} with respect
#' to \eqn{X_{1,1}}{X_11}. Numerator layout feels unnatural
#' because it makes a gradient vector of a scalar-valued function
#' into a row vector. Despite this deficiency, it makes 
#' the product rule feel more natural. (2FIX: is this so?)
#'
#' @slot val an \code{array} of some numeric value. (Note that
#' \code{array} includes \code{matrix} as a subclass.) The numeric
#' value can have arbitrary dimension.
#' @slot dvdx a \code{matrix} of the derivative of 
#' (the vector of) \code{val} with respect to some independent 
#' variable, \eqn{X}{X}. 
#' A Derivative is indeed a 2-dimensional matrix.
#' Derivatives have all been 'flattened'. See the details. 
#' If not given, defaults
#' to the identity matrix, in which case \eqn{val=X}{val = X},
#' which is useful to initialization. Note that the derivative
#' is with respect to an 'unrestricted' \eqn{X}.
#' @slot xtag an optional name for the \eqn{X} variable. 
#' Operations between two objects of the class with distinct
#' \code{xtag} data will result in an error, since they are 
#' considered to have different independent variables.
#' @slot ytag an optional name for the \eqn{val} variable. 
#' This will be propagated forward.
#' @slot varx an optional variance-covariance matrix of
#' the independent variable, \eqn{X}{X}.
#'
#' @return An object of class \code{madness}.
#' @keywords differentiation
#' @keywords multivariate
#' @references
#'
#' Petersen, Kaare Brandt and Pedersen, Michael Syskind. "The Matrix Cookbook."
#' Technical University of Denmark (2012). 
#' \url{http://www2.imm.dtu.dk/pubdb/p.php?3274}
#'
#' Magnus, Jan R. and Neudecker, H. "Matrix Differential Calculus with Applications in Statistics and Econometrics."
#' 3rd Edition. Wiley Series in Probability and Statistics: Texts and References Section (2007).
#' \url{http://www.janmagnus.nl/misc/mdc2007-3rdedition}
#'
#' @examples 
#' obj <- new("madness", val=matrix(rnorm(10*10),nrow=10), dvdx=diag(100), xtag="foo", ytag="foo")
#' obj2 <- madness(val=matrix(rnorm(10*10),nrow=10), xtag="foo", ytag="foo^2")
#'
#' @template etc
#' @name madness-class
#' @rdname madness-class
#' @exportClass madness
#' @export
setClass("madness", 
				 representation(val="array", dvdx="matrix", xtag="character", ytag="character", varx="matrix"),
				 prototype(val=matrix(nrow=0,ncol=0),
									 dvdx=matrix(nrow=0,ncol=0),
									 xtag=NA_character_,
									 ytag=NA_character_,
									 varx=matrix(nrow=0,ncol=0)),
				 validity=function(object) {
					 # ... 
					 # http://www.cyclismo.org/tutorial/R/s4Classes.html
					 if (length(object@val) != dim(object@dvdx)[1]) { return("bad dimensionality or derivative not in numerator layout.") }
					 if (dim(object@varx)[1] != dim(object@varx)[2]) { return("must give empty or square varx variance covariance.") }
					 if ((dim(object@varx)[1] != 0) && (dim(object@varx)[1] != dim(object@dvdx)[2])) {
						 return("must give empty or conformable varx variance covariance.") 
					 }
					 return(TRUE)
				 }
				 )

# constructor method documentation
#  
#' @param .Object a \code{madness} object, or proto-object.
#' @rdname madness-class
#' @aliases initialize,madness-class
setMethod('initialize',
					signature('madness'),
					function(.Object,val,dvdx,xtag=NA_character_,ytag=NA_character_,varx=matrix(nrow=0,ncol=0)) {
					 	if (length(val) != dim(dvdx)[1]) { stop("bad dimensionality or derivative not in numerator layout.") }
					 	if (dim(varx)[1] != dim(varx)[2]) { stop("must give empty or square varx variance covariance.") }
					 	if ((dim(varx)[1] != 0) && (dim(varx)[1] != dim(dvdx)[2])) {
						 stop("must give empty or conformable varx variance covariance.") 
						}
						if (is.null(dim(val))) {
							if (length(val) > 1) { warning('no dimension given, turning val into a column') }
							dim(val) <- c(length(val),1)
						}
						if (is.null(dim(dvdx))) {
							if (length(dvdx) > length(val)) { warning('no dimension given, turning independent variable into a column') }
							taild <- length(dvdx) / length(val)
							dim(dvdx) <- c(length(val),taild)
						}
					 	.Object@val <- val
					 	.Object@dvdx <- dvdx
					 	.Object@xtag <- xtag
					 	.Object@ytag <- ytag
					 	.Object@varx <- varx

						.Object
					})

#'
#' @param val an \code{array} of some numeric value, of arbitrary
#' dimension.
#' @param dvdx a \code{matrix} of the derivative of 
#' (the vector of) \code{val} with respect to some independent 
#' variable, \eqn{X}{X}. 
#' @param xtag an optional name for the \eqn{X} variable. 
#' @param ytag an optional name for the \eqn{val} variable. 
#' @param varx an optional variance-covariance matrix of
#' the independent variable, \eqn{X}{X}.
#' @name madness
#' @rdname madness-class
#' @export
madness <- function(val,dvdx=NULL,xtag=NULL,ytag=NULL,varx=NULL) {
	if (missing(ytag)) { 
		ytag <- deparse(substitute(val))
	}
	if (missing(dvdx)) { 
		dvdx <- diag(1,nrow=length(val))
		if (missing(xtag)) {
			xtag <- ytag
		}
	} else if (missing(xtag)) {
		xtag <- 'x'
	}
	if (is.null(dim(val))) {
		if (length(val) > 1) { warning('no dimension given, turning val into a column') }
		dim(val) <- c(length(val),1)
	}
	if (is.null(dim(dvdx))) {
		if (length(dvdx) > length(val)) { warning('no dimension given, turning independent variable into a column') }
		taild <- length(dvdx) / length(val)
		dim(dvdx) <- c(length(val),taild)
	}
	if (is.null(varx)) { varx <- matrix(nrow=0,ncol=0) }
	retv <- new("madness", val=val, dvdx=dvdx, xtag=xtag, ytag=ytag, varx=varx)
	invisible(retv)
}

# http://www.bioconductor.org/help/course-materials/2010/AdvancedR/S4InBioconductor.pdf
# accessor #FOLDUP
#' @title Accessor methods.
#'
#' @description
#'
#' Access slot data from a \code{madness} object.
#'
#' @param x a \code{madness} object.
#' @template etc
#' @name accessor
#' @rdname accessor-methods
#' @aliases val
#' @exportMethod val
setGeneric('val', signature="x", function(x) standardGeneric('val'))
#' @rdname accessor-methods
#' @aliases val,madness-method
setMethod('val', 'madness', function(x) x@val )

#' @rdname accessor-methods
#' @aliases dim,madness-method
#' @exportMethod dim
setMethod('dim', 'madness', function(x) dim(val(x)) )

#' @name accessor
#' @rdname accessor-methods
#' @aliases dvdx
#' @exportMethod dvdx
setGeneric('dvdx', signature="x", function(x) standardGeneric('dvdx'))
#' @rdname accessor-methods
#' @aliases dvdx,madness-method
setMethod('dvdx', 'madness', function(x) x@dvdx )

#' @name accessor
#' @rdname accessor-methods
#' @aliases xtag
#' @exportMethod xtag
setGeneric('xtag', signature="x", function(x) standardGeneric('xtag'))
#' @rdname accessor-methods
#' @aliases xtag,madness-method
setMethod('xtag', 'madness', function(x) x@xtag )

#' @name accessor
#' @rdname accessor-methods
#' @aliases ytag
#' @exportMethod ytag
setGeneric('ytag', signature="x", function(x) standardGeneric('ytag'))
#' @rdname accessor-methods
#' @aliases ytag,madness-method
setMethod('ytag', 'madness', function(x) x@ytag )

#' @name accessor
#' @rdname accessor-methods
#' @aliases varx
#' @exportMethod varx
setGeneric('varx', signature="x", function(x) standardGeneric('varx'))
#' @rdname accessor-methods
#' @aliases varx,madness-method
setMethod('varx', 'madness', function(x) x@varx )
#UNFOLD

# replacer#FOLDUP
#' @title Setter methods.
#'
#' @description
#'
#' Modify slot data of a \code{madness} object. Note that the value and the
#' derivative cannot easily be changed, as allowing this form of access would
#' likely result in badly computed derivatives.
#'
#' @template etc
#' @param x a \code{madness} object.
#' @param value the new value of the tag or derivative.
#' @name setter
#' @rdname setter-methods
#' @aliases xtag<-
#' @exportMethod xtag<-
setGeneric('xtag<-', signature="x", function(x,value) standardGeneric('xtag<-'))
#' @rdname setter-methods
#' @aliases xtag<-,madness-method
setReplaceMethod('xtag', 'madness', function(x,value) initialize(x, val=x@val, dvdx=x@dvdx, xtag=value, ytag=x@ytag, varx=x@varx))

#' @name setter
#' @rdname setter-methods
#' @aliases ytag<-
#' @exportMethod ytag<-
setGeneric('ytag<-', signature="x", function(x,value) standardGeneric('ytag<-'))
#' @rdname setter-methods
#' @aliases ytag<-,madness-method
setReplaceMethod('ytag', 'madness', function(x,value) initialize(x, val=x@val, dvdx=x@dvdx, xtag=x@xtag, ytag=value, varx=x@varx))

#' @name setter
#' @rdname setter-methods
#' @aliases varx<-
#' @exportMethod varx<-
setGeneric('varx<-', signature="x", function(x,value) standardGeneric('varx<-'))
#' @rdname setter-methods
#' @aliases varx<-,madness-method
setReplaceMethod('varx', 'madness', function(x,value) initialize(x, val=x@val, dvdx=x@dvdx, xtag=x@xtag, ytag=x@xtag, varx=value))
#UNFOLD

# show#FOLDUP
# 2FIX: add documentation and export
#' @title show a madness object.
#'
#' @description 
#'
#' Displays the madness object.
#'
#' @usage
#'
#' show(object)
#'
#' @param object a \code{madness} object.
#' @examples 
#' obj <- madness(val=matrix(rnorm(10*10),nrow=10), xtag="foo", ytag="foo^2")
#' obj
#' @template etc
#' @name show
#' @rdname show-methods
#' @exportMethod show
#' @aliases show
NULL
#' @rdname show-methods
#' @aliases show,madness-method
setMethod('show', signature('madness'), 
					function(object) {
						rchar <- function(achr,alen) { paste0(rep(achr,ceiling(alen)),collapse='') }
						cat('class:', class(object), '\n')
						xlen <- nchar(object@xtag) + 4
						ylen <- nchar(object@ytag) + 4
						mlen <- max(xlen,ylen)
						repr <- sprintf(paste0('      %',ceiling((mlen + ylen)/2),'s\n',
																	 ' calc: ',rchar('-',mlen), ' \n',
																	 '      %',ceiling((mlen + xlen)/2),'s\n'),
														paste0(rchar(' ',(mlen-ylen)/2),'d ',object@ytag),
														paste0(rchar(' ',(mlen-xlen)/2),'d ',object@xtag))
						cat(repr)
						cat('  val:', head(object@val,1L),  '...\n')
						cat(' dvdx:', head(object@dvdx,1L), '...\n')
						cat(' varx:', head(object@varx,1L), '...\n')
					})
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
