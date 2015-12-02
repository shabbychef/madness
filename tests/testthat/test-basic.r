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

test_that("arith functions",{#FOLDUP
	set.char.seed("dee9af9b-cb59-474f-ac3b-acd60faa8ba2")
	xval <- matrix(1 + runif(4*4),nrow=4)
	xmad <- madness(xval)

	expect_equal(xval,val(xmad))
	expect_equal(dim(xval),dim(xmad))
	
	# sentinel:
	expect_true(TRUE)
})#UNFOLD

#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
