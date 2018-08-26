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

suppressMessages(library(docopt))       # we need docopt (>= 0.3) as on CRAN

doc <- "Usage: make_wff3.R [-v] [-i <INFILE>]

-i infile --infile=INFILE        Give the input CSV file [default: data-raw/F-F_Research_Data_Factors_weekly.CSV]
-v --verbose                     Be more verbose
-h --help                        show this help text"

opt <- docopt(doc)

suppressMessages({
	library(readr)
	library(dplyr)
	library(tidyr)
	library(magrittr)
})

wff3 <- readr::read_csv(opt$infile,skip=4,col_types=cols(X1=col_date(format='%Y%m%d'),.default=col_double())) %>%
	rename(Date=X1) %>%
	mutate(Mkt=`Mkt-RF` + RF) %>%
	select(-`Mkt-RF`) %>%
	select(Date,Mkt,SMB,HML,RF) %>%
	filter(!is.na(Date)) %>%
	arrange(Date)

devtools::use_data(wff3,overwrite=TRUE)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
