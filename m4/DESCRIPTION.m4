dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", role=c("aut","cre"),
    email="shabbychef@gmail.com"))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Automatic Differentiation of Multivariate Operations
BugReports: https://github.com/shabbychef/PKG_NAME()/issues
Description: An object that supports automatic differentiation
    of matrix- and multidimensional-valued functions with 
    respect to multidimensional independent variables. 
    Automatic differentiation is via 'forward accumulation'.
Depends: 
    R (>= 3.2.0)
Imports:
		Matrix,
    matrixcalc,
    expm,
    methods
Suggests: 
    testthat, 
    R.rsp,
    knitr
URL: https://github.com/shabbychef/PKG_NAME()
dnl VignetteBuilder: knitr
VignetteBuilder: R.rsp
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4
