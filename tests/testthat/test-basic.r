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
# Created: 2015.12.02
# Copyright: Steven E. Pav, 2015
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

library(expm)

set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}

context("Basic Operations")#FOLDUP

test_that("initialize errors",{#FOLDUP
	set.char.seed("ea2ca251-7b91-4835-a030-610f1835e995")

	yt <- 'any'
	xt <- 'anx'

	xval <- matrix(1 + runif(4*4),nrow=4)
	# make this the wrong size:
	ddd <- matrix(rnorm((1+length(xval))*5),ncol=5)
	# make this the wrong size
	vvv <- crossprod(matrix(rnorm(100*(1+ncol(ddd))),ncol=(1+ncol(ddd))))

	expect_error(dumb <- madness(xval,ddd,ytag=yt,xtag=xt,varx=vvv))
	expect_error(dumb <- madness(xval,ddd,ytag=yt,xtag=xt))
	expect_error(dumb <- madness(xval,diag(length(xval)),ytag=yt,xtag=xt,varx=vvv))
	expect_error(dumb <- madness(xval,'bogus',ytag=yt,xtag=xt))
	expect_error(dumb <- madness(xval,diag(length(xval)),ytag=23))
	expect_error(dumb <- madness(xval,diag(length(xval)),ytag=yt,xtag=17))
	expect_error(dumb <- madness(xval,diag(length(xval)),ytag=yt,xtag=xt,varx='dumby'))
# make this error out:
	#expect_error(dumb <- madness('not a numeric'))

	vvv <- matrix(rnorm(5*4),ncol=5)
	expect_error(dumb <- madness(xval,diag(length(xval)),ytag=yt,xtag=xt,varx=vvv))

# these are warnings
	xval <- rnorm(5)
	expect_warning(dumb <- madness(xval,ytag=yt,xtag=xt))
# fix this: should be a warning...
	xval <- matrix(1 + runif(4*4),nrow=4)
	nover <- 2
	ddd <- rnorm(nover*length(xval))
	expect_warning(dumb <- madness(xval,ddd,ytag=yt,xtag=xt))
	expect_warning(dumb <- madness(xval,ddd))
	expect_warning(dumb <- madness(xval,ddd,ytag=yt))
	expect_warning(dumb <- madness(xval,ddd,varx=diag(nover)))
	expect_error(dumb <- madness(xval,ddd,varx=diag(nover+1)))


	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD
test_that("basic getters and setters",{#FOLDUP
	set.char.seed("dee9af9b-cb59-474f-ac3b-acd60faa8ba2")
	xval <- matrix(1 + runif(4*4),nrow=4)
	xmad <- madness(xval)

	expect_equal(xval,val(xmad))
	expect_equal(dim(xval),dim(xmad))

	# do not error out
	show(xmad)

	set.char.seed("d35d4e4a-af3a-4491-a759-377fca599ec5")
	ddd <- matrix(rnorm(length(xval)*5),ncol=5)
	yt <- 'any'
	xt <- 'anx'
	vvv <- crossprod(matrix(rnorm(100*ncol(ddd)),ncol=ncol(ddd)))
	xmad <- madness(xval,ddd,ytag=yt,xtag=xt,varx=vvv)

	expect_equal(xval,val(xmad))
	expect_equal(ddd,dvdx(xmad))
	expect_equal(vvv,varx(xmad))
	expect_equal(xt,xtag(xmad))
	expect_equal(yt,ytag(xmad))

	# as.foo
	#expect_equal(as.numeric(xval),as.numeric(xmad))
	expect_equal(as.matrix(xval),as.matrix(xmad))
	expect_equal(as.array(xval),as.array(xmad))

	ddd <- matrix(rnorm(length(xval)*5),ncol=5)
	yt <- 'anewy'
	xt <- 'anewx'
	vvv <- crossprod(matrix(rnorm(100*ncol(ddd)),ncol=ncol(ddd)))
	xtag(xmad) <- xt
	ytag(xmad) <- yt
	varx(xmad) <- vvv
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD

#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
