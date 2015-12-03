

# madness

[![Build Status](https://travis-ci.org/shabbychef/madness.png)](https://travis-ci.org/shabbychef/madness)
[![codecov.io](http://codecov.io/github/shabbychef/madness/coverage.svg?branch=master)](http://codecov.io/github/shabbychef/madness?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/madness)](http://cran.rstudio.com/package=madness) 
[![Downloads](http://cranlogs.r-pkg.org/badges/madness?color=brightgreen)](http://www.r-pkg.org/pkg/madness)

'madness' is a 'Multivariate Automatic Differentiation' package for R. It allows one to compute and track the derivative of 
multivariate to multivariate functions applied to concrete data via forward differentiation and the chain rule. 
The obvious use cases are for computing approximate standard errors via the Delta method, possibly for optimization
of objective functions over vectors of parameters, and party tricks. 

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package is not yet on CRAN; the latest version may be
found on [github](https://www.github.com/shabbychef/madness "madness")
via devtools, or installed via [drat](https://github.com/eddelbuettel/drat "drat"):


```r
# via CRAN (not yet): install.packages('madness')
# via drat:
if (require(drat)) {
    drat:::add("shabbychef")
    install.packages("madness")
}
# via devtools
if (require(devtools)) {
    install_github("shabbychef/madness")
}
```

# Basic Usage

You can store an initial value in a madness object, 
its derivative with respect to the independent variable, along with the 
'name' of the dependent and independent variables, and an optional 
variance-covariance matrix of the independent variable. These are mostly
filled in with sane defaults (the derivative defaults to the identity
matrix, and so on):


```r
require(madness)
set.seed(1234)
x <- matrix(rnorm(16), ncol = 2)
madx <- madness(x, ytag = "y", xtag = "x")
# the show method could use some work
print(madx)
```

```
## class: madness 
##         d y
##  calc: ----- 
##         d x
##   val: -1.2 -0.56 ...
##  dvdx: 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...
##  varx:  ...
```

You can then perform operations on objects and the derivative will be propagated
forward via the chain rule:


```r
madx2 <- cbind(crossprod(2 * madx), diag(2))
print(madx2)
```

```
## class: madness 
##         d cbind((t((numeric * y)) %*% (numeric * y)), numeric)
##  calc: -------------------------------------------------------- 
##                                   d x
##   val: 37 5.9 1 0 ...
##  dvdx: -9.7 2.2 8.7 -19 3.4 4 -4.6 -4.4 0 0 0 0 0 0 0 0 ...
##  varx:  ...
```

```r
madx3 <- colSums(madx2)
print(madx3)
```

```
## class: madness 
##         d colSums(cbind((t((numeric * y)) %*% (numeric * y)), numeric))
##  calc: ----------------------------------------------------------------- 
##                                       d x
##   val: 43 ...
##  dvdx: -12 -1.3 6.8 -23 0.33 4.3 -0.76 -4.8 -4.8 1.1 4.3 -9.4 1.7 2 -2.3 -2.2 ...
##  varx:  ...
```

```r
madx4 <- norm(log(abs(1 + madx2^2)), "F")
print(madx4)
```

```
## class: madness 
##         d norm(log(abs((numeric + (cbind((t((numeric * y)) %*% (numeric * y)), numeric) ^ numeric)))), 'f')
##  calc: ----------------------------------------------------------------------------------------------------- 
##                                                         d x
##   val: 10 ...
##  dvdx: -0.87 -0.72 -0.11 -1.6 -0.58 0.21 0.7 -0.26 -1.4 -0.23 0.72 -2.7 -0.031 0.49 -0.0021 -0.56 ...
##  varx:  ...
```

## Correctness

The following are cribbed from the unit tests (of which there are never enough). First
we define the function which computes approximate derivatives numerically, then test
it on some functions:


```r
require(madness)
require(testthat)

apx_deriv <- function(xval, thefun, eps = 1e-09, type = c("forward", 
    "central")) {
    type <- match.arg(type)
    yval <- thefun(xval)
    dapx <- matrix(0, length(yval), length(xval))
    for (iii in seq_len(length(xval))) {
        xalt <- xval
        xalt[iii] <- xalt[iii] + eps
        yplus <- thefun(xalt)
        dydx <- switch(type, forward = {
            (yplus - yval)/eps
        }, central = {
            xalt <- xval
            xalt[iii] <- xalt[iii] - eps
            yneg <- thefun(xalt)
            (yplus - yneg)/(2 * eps)
        })
        dapx[, iii] <- as.numeric(dydx)
    }
    dapx
}

test_harness <- function(xval, thefun, scalfun = thefun, 
    eps = 1e-07) {
    xobj <- madness(val = xval, ytag = "x", xtag = "x")
    yobj <- thefun(xobj)
    xval <- val(xobj)
    dapx <- apx_deriv(xval, scalfun, eps = eps, type = "central")
    # compute error:
    dcmp <- dvdx(yobj)
    dim(dcmp) <- dim(dapx)
    
    merror <- abs(dapx - dcmp)
    rerror <- merror/(0.5 * pmax(sqrt(eps), (abs(dapx) + 
        abs(dcmp))))
    rerror[dapx == 0 & dcmp == 0] <- 0
    max(abs(rerror))
}

# now test a bunch:
set.seed(2015)

xval <- matrix(1 + runif(4 * 4), nrow = 4)
yval <- matrix(1 + runif(length(xval)), nrow = nrow(xval))
expect_less_than(test_harness(xval, function(x) {
    x + x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    x * yval
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    yval/x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    x^x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    x %*% x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    x %*% yval
}), 1e-06)

expect_less_than(test_harness(abs(xval), function(x) {
    log(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    exp(x)
}), 1e-06)
expect_less_than(test_harness(abs(xval), function(x) {
    log10(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    sqrt(x)
}), 1e-06)

expect_less_than(test_harness(xval, function(x) {
    matrix.trace(x)
}, function(matx) {
    sum(diag(matx))
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    colSums(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    colMeans(x)
}), 1e-06)

expect_less_than(test_harness(xval, function(x) {
    vec(x)
}, function(x) {
    dim(x) <- c(length(x), 1)
    x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    vech(x)
}, function(x) {
    x <- x[row(x) >= col(x)]
    dim(x) <- c(length(x), 1)
    x
}), 1e-06)

expect_less_than(test_harness(xval, function(x) {
    tril(x)
}, function(x) {
    x[row(x) < col(x)] <- 0
    x
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    triu(x)
}, function(x) {
    x[row(x) > col(x)] <- 0
    x
}), 1e-06)

# for now, `det` is basically hosed. sorry.
expect_less_than(test_harness(xval, function(x) {
    det(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    determinant(x, logarithm = TRUE)$modulus
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    determinant(x, logarithm = FALSE)$modulus
}), 1e-06)

expect_less_than(test_harness(xval, function(x) {
    colSums(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    colMeans(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    rowSums(x)
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    rowMeans(x)
}), 1e-06)

set.seed(2015)
zval <- matrix(0.01 + runif(4 * 100, min = 0, max = 0.05), 
    nrow = 4)
xval <- tcrossprod(zval)

expect_less_than(test_harness(xval, function(x) {
    norm(x, "O")
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    norm(x, "I")
}), 1e-06)
# Matrix::norm does not support type '2'
expect_less_than(test_harness(xval, function(x) {
    norm(x, "2")
}, function(x) {
    base::norm(x, "2")
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    norm(x, "M")
}), 1e-06)
expect_less_than(test_harness(xval, function(x) {
    norm(x, "F")
}), 1e-06)

expect_less_than(test_harness(xval, function(x) {
    sqrtm(x)
}), 1e-06)
```

## Symmetry

Some functions implicitly break symmetry, which could cause the differentiation
process to fail. For example, the 'chol' function is to be applied to a symmetric
matrix, but only looks at the upper triangular part, ignoring the lower triangular
part. This is demonstrated below. For the moment, the only 'solution' is to enforce
symmetry of the input. Eventually some native functionality around symmetry may
be implemented.


```r
set.seed(2015)
zval <- matrix(0.01 + runif(4 * 100, min = 0, max = 0.05), 
    nrow = 4)
xval <- tcrossprod(zval)

fsym <- function(x) {
    0.5 * (x + t(x))
}
# this will fail:
expect_less_than(test_harness(xval, function(x) {
    chol(x)
}), 1e-06)
```

```
## Error: test_harness(xval, function(x) {
##     chol(x)
## }) not less than 1e-06. Difference: -2
```

```r
# this will not:
expect_less_than(test_harness(xval, function(x) {
    chol(fsym(x))
}), 1e-06)
```

# Warnings

This code is a proof of concept. The methods used to compute derivatives are not (yet)
space-efficient or necessarily numerically stable. User assumes all risk.

Derivatives are stored as a matrix in 'numerator layout'. That is the independent and
dependent variable are vectorized, then the derivative matrix has the same number
of columns as elements in the dependent variable. Thus the derivative of a 
3 by 2 by 5 y with respect to a 1 by 2 by 2 x is a 30 by 4 matrix.

