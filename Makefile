######################
# 
# Created: 2014.01.04
# Copyright: Steven E. Pav, 2014-2018
# Author: Steven E. Pav
######################

############### FLAGS ###############

VMAJOR 						 = 0
VMINOR 						 = 2
VPATCH  					 = 4
VDEV 							 = 
#VDEV 							 = .003
PKG_NAME 					:= madness

RPKG_USES_RCPP 		:= 0

include ./rpkg_make/Makefile

rpkg_make :  ## initialize the Makefile in rpkg_make
	git submodule add https://github.com/shabbychef/rpkg_make.git rpkg_make
	git submodule init
	git submodule update

STOCK_CSV 				= data-raw/AAPL.csv data-raw/IBM.csv

data-raw/%.csv :
	wget -O $@ "https://www.quandl.com/api/v3/datasets/EOD/$*.csv?api_key=$(QUANDL_AUTH)&collapse=weekly&start_date=1981-01-01&end_date=2017-12-31"

stock_csv : $(STOCK_CSV)   # download csv of stock data to data-raw

stock_returns_data : data/stock_returns.rda  ## make the aggregate raw stocks data csv file

data/stock_returns.rda  : nodist/make_stocks.r $(STOCK_CSV)  
	r $(filter %.r,$^) $(filter-out %.r,$^)

wff3_data :  data/wff3.rda ## make the weekly FF4 returns data

data-raw/F-F_Research_Data_Factors_weekly_CSV.zip :
	wget -O $@ 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly_CSV.zip'

data-raw/F-F_Research_Data_Factors_weekly.CSV : data-raw/F-F_Research_Data_Factors_weekly_CSV.zip 
	cd data-raw && unzip $^ && cd -

data/wff3.rda : nodist/make_wff3.r data-raw/F-F_Research_Data_Factors_weekly.CSV
	r $(filter %.r,$^) --infile=$(filter-out %.r,$^)

all_data : data/stock_returns.rda data/wff3.rda ## make all package data

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
