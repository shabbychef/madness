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

#' @title Stock Returns Data
#' @description Historical weekly relative returns of common shares of IBM and AAPL, 
#' downloaded from Quandl.
#' @usage data(stock_returns)
#' @format A \code{data.frame} object with 1930 observations and 3 columns
#' The columns are defined as follows:
#' \describe{
#'  \item{\code{Date}}{The closing date at which the return was observed, as a \code{Date} object.
#'  These are Friday dates, ranging from January 1981 through December 2017.}
#'  \item{\code{AAPL}}{The simple returns of AAPL common shares, based on
#'  weekly (adjusted) close prices.  A value of \code{0.01} corresponds to a one percent return.
#'  Close prices are adjusted for splits and dividends by Quandl.}
#'  \item{\code{IBM}}{The simple returns of IBM common shares, based on
#'  weekly (adjusted) close prices.  A value of \code{0.01} corresponds to a one percent return.
#'  Close prices are adjusted for splits and dividends by Quandl.}
#' }
#' @source 
#' Data were collated from Quandl on August 25, 2018, from
#' \url{https://www.quandl.com/data/EOD/AAPL-Apple-Inc-AAPL-Stock-Prices-Dividends-and-Splits}
#' and 
#' \url{https://www.quandl.com/data/EOD/IBM-International-Business-Machines-Corporation-IBM-Stock-Prices-Dividends-and-Splits}.
#' @template etc
#' @name stock_returns 
#' @rdname stock_returns 
#' @docType data
#' @keywords data
#' @examples
#' data(stock_returns)
#' str(stock_returns)
"stock_returns"

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
