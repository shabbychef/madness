dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", 
    role=c("aut","cre"),
    email="shabbychef@gmail.com",
    comment = c(ORCID = "0000-0002-4197-6195")))
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
dnl need these for the vignette:
    dplyr,
    tidyr,
    lubridate,
    SharpeR,
    sandwich,
dnl quantmod,
dnl R.rsp,
    formatR,
    knitr
URL: https://github.com/shabbychef/PKG_NAME()
VignetteBuilder: knitr
dnl VignetteBuilder: R.rsp
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4:et
