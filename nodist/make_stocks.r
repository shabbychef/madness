# /usr/bin/r
#
# Copyright 2018-2018 Steven E. Pav. All Rights Reserved.
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
# Created: 2018.08.25
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

library(docopt)

doc <- "Usage: make_stocks.r [-v] INFILES...

-v --verbose                     Be more verbose
-h --help                        show this help text"

opt <- docopt(doc)

suppressMessages({
	library(readr)
	library(dplyr)
	library(tidyr)
	library(lubridate)
	library(devtools)
})

readone <- function(fname) {
	readr::read_csv(fname,col_types=cols(Date=col_date(format=''),
																			 .default=col_double())) %>%
		dplyr::select(Date,Adj_Close) %>%
		arrange(Date) %>%
		mutate(stockn=gsub('^(.+/)?([^/]+).csv$','\\2',fname))
}

allem <- lapply(opt$INFILES,readone) %>%
	bind_rows()

lr2rr <- function(lll) { exp(lll) - 1 }

dodiff <- function(x,win=1) { x - dplyr::lag(x,win) }
logret <- function(x,win=1) { dodiff(log(x),win=win) }
relret <- function(x,win=1) { lr2rr(logret(x,win=win)) }

stock_returns <- allem %>%
	group_by(stockn) %>%
		mutate(lret=logret(Adj_Close)) %>%
	ungroup() %>%
	dplyr::select(-Adj_Close) %>%
	tidyr::spread(key=stockn,value=lret) %>%
	filter(!is.na(AAPL)) %>%
	arrange(Date) %>% 
	mutate(Date=Date %m-% days(2))

devtools::use_data(stock_returns,overwrite=TRUE)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
