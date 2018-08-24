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

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
