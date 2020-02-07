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

# Created: 2015.11.16
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav
# Comments: Steven E. Pav

#' Automatic Differentiation of Matrix Operations.
#'
#' @section Legal Mumbo Jumbo:
#'
#' madness is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU Lesser General Public License for more details.
#'
#' @template etc
#'
#' @references
#'
#' Griewank, Andreas and Walther, Andrea. "Evaluating Derivatives: principles and techniques of algorithmic differentiation."
#' SIAM (2008).
#'
#' Petersen, Kaare Brandt and Pedersen, Michael Syskind. "The Matrix Cookbook."
#' Technical University of Denmark (2012). 
#' \url{http://www2.imm.dtu.dk/pubdb/p.php?3274}
#'
#' Magnus, Jan R. and Neudecker, H. "Matrix Differential Calculus with Applications in Statistics and Econometrics."
#' 3rd Edition. Wiley Series in Probability and Statistics: Texts and References Section (2007).
#'
#' Magnus, Jan R. and Neudecker, H. "The elimination matrix: some lemmas and applications," 
#' SIAM Journal on Algebraic Discrete Methods 1, no. 4 (1980): 422-449.
#' \url{http://www.janmagnus.nl/papers/JRM008.pdf}
#'
#' Magnus, Jan R. and Neudecker, H. "Symmetry, 0-1 Matrices and Jacobians,"
#' Econometric Theory 2 (1986): 157-190.
#' \url{http://www.janmagnus.nl/papers/JRM014.pdf},
#'
#' Fackler, Paul L. "Notes on Matrix Calculus." (2005).
#' \url{http://www4.ncsu.edu/~pfackler/MatCalc.pdf}
#'
#' MM: DO use  @importFrom (whimps use full imports ..) !
#' @import matrixcalc methods
#' @importFrom methods cbind2
#' @importFrom methods rbind2
#' @importFrom methods coerce
#' @importFrom methods show
#' @importFrom methods kronecker
#' @importFrom stats coef
#' @importFrom stats lm
#' @importFrom stats na.omit
#' @importFrom stats vcov
#' @importFrom utils head
#'
#' @name madness-pkg
#' @rdname madness-pkg
#' @docType package
#' @title Multivariate Automatic Differentiation.
#' @keywords package
#' @template etc
#' @note
#' 
#' This package is maintained as a hobby. 
#'
NULL

#' @title News for package \sQuote{madness}:
#'
#' @description
#'
#' News for package \sQuote{madness}.
#'
#' \newcommand{\pkg}{#1}
#' \newcommand{\CRANpkg}{\href{https://cran.r-project.org/package=#1}{\pkg{#1}}}
#' \newcommand{\madness}{\CRANpkg{madness}}
#'
#' @section \madness{} Version 0.2.7 (2020-02-07) :
#' \itemize{
#' \item emergency CRAN release to deal with ellipsis in documentation.
#' }
#'
#' @section \madness{} Version 0.2.6 (2019-04-19) :
#' \itemize{
#' \item emergency CRAN release to deal with change in generic
#' signature for \code{colSums}, \code{colMeans},
#' \code{rowSums}, \code{rowMeans}.
#' }
#'
#' @section \madness{} Version 0.2.5 (2018-08-27) :
#' \itemize{
#' \item emergency CRAN release to deal with failing vignette on alternative
#' BLAS.
#' }
#'
#' @section \madness{} Version 0.2.4 (2018-08-26) :
#' \itemize{
#' \item adding to unit tests.
#' \item fix scalar to array promotion.
#' \item fix broken vtag in aperm.
#' \item add FF3 and stock returns data to build vignette.
#' }
#'
#' @section \madness{} Version 0.2.3 (2018-02-14) :
#' \itemize{
#' \item emergency CRAN release to deal with failing tests
#' under alternative BLAS/LAPACK libraries.
#' }
#'
#' @section \madness{} Version 0.2.2 (2017-04-26) :
#' \itemize{
#' \item emergency CRAN release for upstream changes to \code{diag}.
#' thanks to Martin Maechler for the patch.
#' }
#'
#' @section \madness{} Version 0.2.1 (2017-04-13) :
#' \itemize{
#' \item emergency CRAN release for failed build.
#' \item no new functionality.
#' }
#'
#' @section \madness{} Version 0.2.0 (2016-01-19) :
#' \itemize{
#' \item add static vignette.
#' \item modify \code{twomoments}.
#' \item release to CRAN.
#' }
#'
#' @section \madness{} Version 0.1.0.400 (2016-01-12) :
#' \itemize{
#' \item adding \code{max} and \code{min}.
#' }
#'
#' @section \madness{} Version 0.1.0.300 (2016-01-10) :
#' \itemize{
#' \item adding \code{eigen}.
#' }
#'
#' @section \madness{} Version 0.1.0.200 (2016-01-07) :
#' \itemize{
#' \item exporting \code{diag}.
#' }
#'
#' @section \madness{} Version 0.1.0 (2015-12-15) :
#' \itemize{
#' \item first CRAN release.
#' }
#'
#' @section \madness{} Initial Version 0.0.0.5000 (2015-12-01) :
#' \itemize{
#' \item first github release.
#' }
#'
#' @name madness-NEWS
#' @rdname NEWS
NULL


# 2FIX
#
# expm logm (these will require Matrix package ...)
# eigs?
#
# %o%   (note this is a *huge* outer product potentially)
#

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
