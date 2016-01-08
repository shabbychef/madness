# 
# * Fri Dec 28 2012 04:15:55 PM Steven E. Pav <steven@cerebellumcapital.com>
#
# Makefile 'remixed' from RTikZDevice and optmatch packages. HT to Sharpie and
# markmfredrickson.
#
# you may have too
# sudo apt-get install -y texinfo
#
# forked from SharpeR
#
#
# Created: 2014.01.04
#

SHELL 						 = /bin/bash

BIN_TIME          ?= $(shell which time)

R_DEV_FILES 			?= $(wildcard ./R/*.[rR])
R_FILES 					?= $(R_DEV_FILES)
R_FILES 					+= $(wildcard ./inst/tests/*.[rR])
R_FILES 					+= $(wildcard ./tests/testthat/*.[rR])
R_FILES 					+= $(wildcard ./man-roxygen/*.R)
R_FILES 					+= $(wildcard ./tests/*.[rR])

R_QPDF 						?= $(shell which qpdf)
R_GSCMD						?= $(shell which gs)
GS_QUALITY 				?= 'ebook'

M4_FILES					?= $(wildcard m4/*.m4)

VMAJOR 						 = 0
VMINOR 						 = 1
VPATCH  					 = 0
VDEV 							 = .2000
#VERSION 					 = 0.1402
VERSION 					 = $(VMAJOR).$(VMINOR).$(VPATCH)$(VDEV)
TODAY 						:= $(shell date +%Y-%m-%d)

PKG_NAME 					:= madness
PKG_VERSION				:= $(VERSION)
PKG_SRC 					:= $(shell basename $(PWD))

PKG_TGZ 					 = $(PKG_NAME)_$(PKG_VERSION).tar.gz

LOCAL 						:= .local
RCHECK 						 = $(PKG_NAME).Rcheck
RCHECK_SENTINEL 	 = $(RCHECK)/$(PKG_NAME)/DESCRIPTION
DRAT_SENTINEL   	 = .drat_$(PKG_TGZ)

# WTF! 5.0.1, 5.0.0, 4.1.1, 4.1.0 mangle marithops
# 4.0.x series and 3.1.0 has some other error about
# U_REGEX_MISSING_CLOSING_BRACKET. so fuck. that.
ROXYGEN_TARGZ 		 = $(LOCAL)/roxygen2_5.0.0.tar.gz 

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN 							?= $(shell dirname "`which R`")
R_LIBS_SITE 			?= /usr/local/lib/R/site-library:/usr/lib/R/site-library::/usr/lib/R/library
R         				 = $(RBIN)/R
RSCRIPT   				 = $(RBIN)/Rscript
#R_FLAGS 					?= --vanilla --verbose -q
#R_FLAGS 					?= --vanilla -q
R_FLAGS 					?= -q --no-save --no-restore --no-init-file

# packages I need to test this one
DEV_DEPS           = testthat roxygen2 knitr covr curl
PACKAGE_DEPS       = matrixcalc expm 
PACKAGE_SUGGESTS   = sandwich 
TEST_DEPS  				 = $(DEV_DEPS) $(PACKAGE_DEPS) $(PACKAGE_SUGGESTS)
INSTALLED_DEPS 		 = $(patsubst %,$(LOCAL)/%/DESCRIPTION,$(TEST_DEPS)) 
PKG_TESTR 				 = tests/run-all.R

# see http://stackoverflow.com/a/7531247/164611
null  						:=
space 						:= $(null) #
comma 						:= ,
# turn space list to comma list:
COMMA_IT 					 = $(subst $(space),$(comma),$(strip $(1)))

TEST_DEPS_LIST  	 = $(call COMMA_IT,$(TEST_DEPS))

RD_DUMMY 					 = man/$(PKG_NAME)-class.Rd

# do not distribute these!
NODIST_R_DIR			 = nodist
NODIST_FILES			 = ./Makefile $(M4_FILES) .gitignore .gitattributes 
NODIST_FILES			+= rebuildTags.sh .tags .R_tags 
NODIST_FILES			+= Makefile
NODIST_DIRS				 = .git man-roxygen m4 $(NODIST_R_DIR)
NODIST_DIRS				+= $(VIGNETTE_D)/figure 
NODIST_DIRS				+= $(VIGNETTE_D)

# extradata
EXTDATA_D 				 = inst/extdata
PREMAKE_R 				 = $(wildcard ./$(NODIST_R_DIR)/make_*.R)
PREMAKE_RDA 			 = $(patsubst ./$(NODIST_R_DIR)/make_%.R,$(EXTDATA_D)/%.rda,$(PREMAKE_R))
EXTDATA_FILES	 		 = $(PREMAKE_RDA)

# vignette stuff
VIGNETTE_D 				 = vignettes
VIGNETTE_CACHE 		 = $(VIGNETTE_D)/cache
VIGNETTE_EXTRAS		 = $(VIGNETTE_D)/$(PKG_NAME).Rnw $(VIGNETTE_D)/$(PKG_NAME).sty
VIGNETTE_SRCS  		 = $(VIGNETTE_D)/$(PKG_NAME).Rnw $(VIGNETTE_D)/$(PKG_NAME).bib
VIGNETTE_PDF   		 = $(VIGNETTE_D)/$(PKG_NAME).pdf
VIGNETTE_HTML  		 = $(VIGNETTE_D)/index.html
VIGNETTE_CACHE_SENTINEL = $(VIGNETTE_CACHE)/__$(PKG_NAME).etc


SUPPORT_FILES 		 = ./DESCRIPTION ./NAMESPACE ./ChangeLog $(RD_DUMMY) ./inst/CITATION ./.Rbuildignore

# for building the package.tgz
#BUILD_FLAGS 			?= --compact-vignettes
BUILD_FLAGS 			?= --compact-vignettes="gs+qpdf" --resave-data=best
BUILD_ENV 				 = R_QPDF=$(R_QPDF) R_GSCMD=$(R_GSCMD) \
									 GS_QUALITY=$(GS_QUALITY)

NODIST_FILES 			+= $(VIGNETTE_PDF) $(VIGNETTE_HTML)
SUPPORT_FILES 		+= $(VIGNETTE_SRCS) $(VIGNETTE_EXTRAS)
EXTRA_PKG_DEPS 		 = 
#EXTRA_PKG_DEPS 	+= $(VIGNETTE_CACHE_SENTINEL)

EXTRA_PKG_DEPS 		+= $(EXTDATA_FILES)

#INSTALL_FLAGS 		?= --preclean --no-multiarch --library=$(LOCAL) 
INSTALL_FLAGS 		?= --preclean --library=$(LOCAL) 

TEST_PRAGMA     	?= release

GIT_BRANCH 				?= master
#GIT_BRANCH 				?= dev1401

# for R CMD build
ifeq ($(TEST_PRAGMA),thorough)
	# noop
else 
	SLOW_TESTS 			 = $(wildcard inst/tests/test-slow*.r)
	NODIST_FILES 		+= $(SLOW_TESTS)
endif

define \n


endef

fooz :
	echo $(PREMAKE_RDA)

STAGING 				?= .staging
STAGED_PKG 			 = $(STAGING)/$(PKG_NAME)

# latex bother. bleah.
#TEXINPADD    = .:$(HOME)/sys/etc/tex:$(HOME)/sys/etc/tex/SEPtex:$(HOME)/work/math/TEX
TEXINPADD    = .:./$(VIGNETTE_D)
PRETEX       = TEXINPUTS=$(TEXINPADD):$$TEXINPUTS
PREBIB       = BSTINPUTS=$(TEXINPADD):$$BSTINPUTS \
               BIBINPUTS=$(TEXINPADD):$$BIBINPUTS 
BIBTEX      := $(shell which bibtex)
LATEX       := $(shell which latex)

BASE_DEF_PACKAGES   = "utils,graphics,grDevices,methods,stats,$(PKG_NAME)"
PDF_NEED_PACKAGES   = "$(BASE_DEF_PACKAGES),knitr" 

#FAST_

#########################################################################
# MACROS
#########################################################################

# install locally
INSTALLPKG = $(R_LOCALLY) -e "install.packages('$(1)', repos = 'http://cran.cnr.Berkeley.edu')" 
	
# make a directory
MKDIR = mkdir -p $(1)

# warn new deps
#WARN_DEPS = $(warning newer deps are $(?))
WARN_DEPS = $(warning will build $@ ; newer deps are $(?))

#########################################################################
# TARGETS
#########################################################################

# these are phony targets
.PHONY: help tags all \
	gitpull gitpush staged \
	news docs build install \
	testthat tests loctest \
	staging_d local_d \
	clean realclean \
	vignette_cache \
	the_vignette \
	static_vignette \
	the_paper \
	R coverage 

help:
	@echo "\nTasks for $(PKG_NAME)\n"
	@echo "Usage: \`make <task>\` where <task> is one of:"
	@echo ""
	@echo "Development Tasks"
	@echo "-----------------"
	@echo "  tags       Build the ctags, for dev purposes"
	@echo "  deps       Install dependencies for package development"
	@echo "  docs       Invoke roxygen to generate Rd files in man/"
	@echo "  testthat   Run unit tests."
	@echo '  tests       "   "     "   '
	@echo '  loctest    local unit tests.'
	@echo "  staged     Create a staging version of this package."
	@echo "  build      Make docs and then R CMD build the package.tgz"
	@echo "  install    Make build and then install the result."
	@echo "  R          Make install, then invoke R in the local context w/ the package."
	@echo "  vignette_cache    fuck. this is broken. 2FIX"
	@echo "  the_vignette   Build the vignette in the local context."
	@echo "  clean      Do some cleanup."
	@echo "  realclean  Do lots of cleanup."
	@echo ""
	@echo "  subadvice  CRAN submission advice."
	@echo "  submit     Submit to CRAN."
	@echo ""
	@echo "Packaging Tasks"
	@echo "---------------"
	@echo "  check      Make build, then R CMD check the package as CRAN."
	@echo "  gitpush    Yes, I am lazy"
	@echo "  dratit     Make build, then upload package to my drat repo."
	@echo ""
	@echo "Using: "
	@echo "         RBIN: $(RBIN) "
	@echo "            R: $(R) "
	@echo "  R_LIBS_SITE: $(R_LIBS_SITE) "
	@echo "Set the RBIN environment variable to change this."
	@echo ""

# dev stuff
~/.ctags :
	@-echo -E '--langdef=R' >> $@
	@-echo -E '--langmap=R:.s.S.R.r.q' >> $@
	@-echo -E '--regex-R=/^[ \t]+"?([.A-Za-z][.A-Za-z0-9_]*)"?[\t]*<-[\t]*function/\1/' >> $@
	@-echo -E '--regex-R=/^"?([.A-Za-z][.A-Za-z0-9_]*)"?[ \t]*<-/\1/' >> $@

.R_tags: $(R_FILES)
	./rebuildTags.sh

tags: .R_tags

.Renviron : 
	echo "R_LIBS=$(LOCAL)" >> $@

# if you use emacs (shudder)
TAGS: 
	$(R) --slave CMD rtags

% : m4/%.m4 Makefile
	m4 -I ./m4 -DVERSION=$(VERSION) -DDATE=$(TODAY) -DPKG_NAME=$(PKG_NAME) $< > $@

%.md : %.Rmd
	$(call WARN_DEPS)
	$(call MKDIR,$(EXTDATA_D))
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(BASE_DEF_PACKAGES),knitr,quantmod" \
				 $(R) $(R_FLAGS) --slave -e \
				 "setwd(dirname('$@'));knitr::knit(basename('$<'));"

cached_data : $(PREMAKE_RDA)

README.md : $(NODIST_R_DIR)/README.md
	mv $< $@
	rsync -av --delete $(NODIST_R_DIR)/github_extra/ ./github_extra/

# macro for local R
R_LOCALLY  						= R_LIBS=$(LOCAL) $(R) $(R_FLAGS)
R_TIME_LOCALLY  			= R_LIBS=$(LOCAL) $(BIN_TIME) --verbose $(R) $(R_FLAGS)

# make directories
local_d :
	$(call MKDIR,$(LOCAL))

staging_d :
	$(call MKDIR,$(STAGING))

# debugging
echo :
	@-echo $(R_FILES)

# install an R package in the 'LOCAL' directory.
$(LOCAL)/%/DESCRIPTION : 
	$(call MKDIR,$(LOCAL))
	$(R_LOCALLY) -e "install.packages('$*', repos = 'http://cran.cnr.Berkeley.edu')" 

$(ROXYGEN_TARGZ) :
	wget -O $@ https://cran.r-project.org/src/contrib/Archive/roxygen2/$(@F)

.roxygen2 : $(ROXYGEN_TARGZ)
	echo roxygen2 5.0.1 is borked!
	$(R_LOCALLY) CMD INSTALL $<
	touch $@

deps: $(INSTALLED_DEPS) .roxygen2

# roxygen it.
$(RD_DUMMY) NAMESPACE: $(R_FILES)
	$(call WARN_DEPS)
	$(R_LOCALLY) --slave -e "require(roxygen2); roxygenize('.', clean=TRUE)"
	touch $@

docs: README.md DESCRIPTION man/$(PKG_NAME)-class.Rd 

#RSYNC_FLAGS     = -av
#RSYNC_FLAGS     = -vrlpgoD --delete
RSYNC_FLAGS     = -av --delete 

# a parallel version of this package, but without the support structure
$(STAGED_PKG)/DESCRIPTION : $(R_FILES) $(SUPPORT_FILES) $(EXTRA_PKG_DEPS)
	$(call WARN_DEPS)
	@-echo clean up first
	@-rm -rf $(STAGED_PKG)
	$(call MKDIR,$(STAGED_PKG))
	@-echo sync over
	rsync $(RSYNC_FLAGS) \
  --include=man/ --include=man/* \
  --include=NAMESPACE --include=DESCRIPTION \
  --include=$(EXTDATA_D)/ \
  --exclude-from=.gitignore \
 $(patsubst %, % \${\n},$(patsubst %,--exclude=%,$(NODIST_FILES)))  --exclude=$(LOCAL) \
 $(patsubst %, % \${\n},$(patsubst %,--exclude=%,$(NODIST_DIRS)))  --exclude=$(basename $(STAGING)) \
  --exclude=$(RCHECK) \
  . $(@D)
	touch $@

staged : $(STAGED_PKG)/DESCRIPTION 

# make the 'package', which is a tar.gz
$(PKG_TGZ) : $(STAGED_PKG)/DESCRIPTION $(INSTALLED_DEPS) $(EXTRA_PKG_DEPS) 
	$(call WARN_DEPS)
	# check values
	@$(BUILD_ENV) $(R_LOCALLY) --slave -e 'print(Sys.getenv("R_QPDF"));print(Sys.getenv("R_GSCMD"));print(Sys.getenv("GS_QUALITY"));'
	$(BUILD_ENV) $(R_LOCALLY) CMD build $(BUILD_FLAGS) $(<D)

#package : $(PKG_TGZ)

build : $(PKG_TGZ)

build_list : $(PKG_TGZ)
	tar -tzvf $<

# an 'install'
$(LOCAL)/$(PKG_NAME)/INDEX : $(PKG_TGZ) 
	$(call WARN_DEPS)
	$(call MKDIR,$(LOCAL))
	$(R_LOCALLY) CMD INSTALL $(INSTALL_FLAGS) $<
	touch $@

install: $(LOCAL)/$(PKG_NAME)/INDEX

# rely on the 'install' target above.
$(LOCAL)/doc/$(PKG_NAME).pdf : $(LOCAL)/$(PKG_NAME)/INDEX

# check and install
$(RCHECK_SENTINEL) : $(PKG_TGZ)
	$(call WARN_DEPS)
	$(R_TIME_LOCALLY) CMD check --as-cran --timings $^ 

#$(R_LOCALLY) CMD check --as-cran --outdir=$(RCHECK) $^ 
	
check: $(RCHECK_SENTINEL)

checksee : $(RCHECK_SENTINEL)
	okular $(RCHECK)/$(PKG_NAME)-manual.pdf

$(DRAT_SENTINEL) : $(PKG_TGZ)
	$(call WARN_DEPS)
	$(R) --slave -e "drat:::insertPackage('$<',repodir='~/github/drat',commit=TRUE)"

dratit : $(DRAT_SENTINEL)

#$(RCHECK)/$(PKG_NAME)/doc/$(PKG_NAME).pdf : $(VIGNETTE_SRCS) $(RCHECK_SENTINEL)

#slow_vignette : $(RCHECK)/$(PKG_NAME)/doc/$(PKG_NAME).pdf

################################
# UNIT TESTING
################################

#$(R_LOCALLY) --slave -e "if (require(testthat) && require($(PKG_NAME))) testthat::test_dir('./inst/tests')" | tee $@

# 2FIX:
unit_test.log : $(LOCAL)/$(PKG_NAME)/INDEX $(LOCAL)/testthat/DESCRIPTION $(PKG_TESTR)
	$(call WARN_DEPS)
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES=$(BASE_DEF_PACKAGES) $(R) $(R_FLAGS) \
				 --slave < $(PKG_TESTR) | tee $@

testthat : unit_test.log

tests    : unit_test.log

loctest : deps $(LOCAL)/$(PKG_NAME)/INDEX
	$(call WARN_DEPS)
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES=$(BASE_DEF_PACKAGES),testthat $(R) $(R_FLAGS) \
				 --slave --silent \
				 -e 'testthat::test_dir("tests/testthat")'

coverage : deps $(LOCAL)/$(PKG_NAME)/INDEX
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES=$(BASE_DEF_PACKAGES),covr $(R) -q --no-save --silent \
				 -e 'percent_coverage(package_coverage("."))'


# drop into R shell in the 'local context'
R : deps $(LOCAL)/$(PKG_NAME)/INDEX
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES=$(BASE_DEF_PACKAGES) $(R) -q --no-save

cheapR : 
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES=$(BASE_DEF_PACKAGES) $(R) -q --no-save

$(PKG_NAME).pdf: $(VIGNETTE_SRCS) deps $(LOCAL)/$(PKG_NAME)/INDEX 
	$(PRETEX) R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(PDF_NEED_PACKAGES)" \
				 $(R) $(R_FLAGS) --slave -e "knitr::knit2pdf('$<');"
	if grep Citation $(PKG_NAME).log > /dev/null; then $(PREBIB) $(BIBTEX) $(PKG_NAME); \
		$(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi
	if grep Rerun $(PKG_NAME).log > /dev/null; then $(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi

$(PKG_NAME)_fast.pdf : $(VIGNETTE_SRCS) 
	$(PRETEX) R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(PDF_NEED_PACKAGES)" \
				 $(R) $(R_FLAGS) --slave -e "knitr::knit2pdf('$<');"
	if grep Citation $(PKG_NAME).log > /dev/null; then $(PREBIB) $(BIBTEX) $(PKG_NAME); \
		$(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi
	if grep Rerun $(PKG_NAME).log > /dev/null; then $(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi
	mv $(PKG_NAME).pdf $<

fast_vignette: $(PKG_NAME)_fast.pdf

the_vignette: $(PKG_NAME).pdf

$(VIGNETTE_CACHE_SENTINEL) : $(VIGNETTE_SRCS) $(LOCAL)/$(PKG_NAME)/INDEX
	$(call WARN_DEPS)
	$(call MKDIR,$(VIGNETTE_CACHE))
	$(PRETEX) R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(PDF_NEED_PACKAGES)" \
				 FORCE_RECOMPUTE='TRUE' \
				 $(R) $(R_FLAGS) --slave -e "setwd('$(VIGNETTE_D)');knitr::knit(basename('$<'));"
	touch $@

vignette_cache : $(VIGNETTE_CACHE_SENTINEL)

%.tex : %.Rnw 
	$(call WARN_DEPS)
	$(PRETEX) R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(PDF_NEED_PACKAGES)" \
				 FORCE_RECOMPUTE='TRUE' \
				 $(R) $(R_FLAGS) --slave -e "setwd('$(VIGNETTE_D)');knitr::knit(basename('$<'));"

%.dvi : %.tex 
		$(PRETEX) $(LATEX) $<
		if grep Citation $*.log > /dev/null; then $(PREBIB) $(BIBTEX) $*; $(PRETEX) $(LATEX) $*; fi
		if grep Rerun $*.log > /dev/null; then $(PRETEX) $(LATEX) $*; fi
		@-cp $*.dvi $(VIGNETTE_D)
		@-cp $*.aux $(VIGNETTE_D)
		@-cp $*.log $(VIGNETTE_D)

%.bbl : %.bib
		$(PREBIB) $(BIBTEX) $*
		@-cp $*.bbl $(VIGNETTE_D)

# make data needed by the vignette. what bother.
$(EXTDATA_D)/%.rda : $(NODIST_R_DIR)/make_%.R
	$(call WARN_DEPS)
	$(call MKDIR,$(EXTDATA_D))
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(BASE_DEF_PACKAGES),knitr,quantmod" \
				 $(R) $(R_FLAGS) --slave -e \
				 "setwd('$(NODIST_R_DIR)');source(basename('$<'));"
	# horribly hacky!
	mv $(NODIST_R_DIR)/*.rda $(EXTDATA_D)

$(VIGNETTE_D)/rauto.bib : $(NODIST_R_DIR)/gen_bib.R
	$(call WARN_DEPS)
	$(call MKDIR,$(EXTDATA_D))
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="$(BASE_DEF_PACKAGES),knitr,quantmod" \
				 $(R) $(R_FLAGS) --slave -e \
				 "setwd('$(NODIST_R_DIR)');source(basename('$<'));"
	# horribly hacky!
	mv $(NODIST_R_DIR)/*.bib $@

cachedata: $(EXTDATA_FILES)

newbuild :
	$(MAKE) clean
	$(MAKE) cachedata
	$(MAKE) docs
	$(MAKE) tags
	$(MAKE) build

# Python. well, iPython.

%.tex : %.ipynb
		ipython nbconvert --to latex --output $* $<

################################
# CLEAN UP 
################################

texclean :
	-rm -rf $(PKG_NAME).log
	-rm -rf $(PKG_NAME).aux
	-rm -rf $(PKG_NAME).out
	-rm -rf $(PKG_NAME).bbl
	-rm -rf $(PKG_NAME).blg

clean : texclean
	-rm DESCRIPTION
	-rm -rf man/*.Rd
	-rm -rf $(STAGED_PKG)
	-rm -rf $(RCHECK)
	-rm -rf $(PKG_NAME).tex
	-rm -rf $(PKG_NAME).pdf

realclean : clean
	-rm -rf $(LOCAL)
	-rm -rf $(STAGING)
	-rm -rf ./cache
	-rm -rf convoluted_build.sh 

################################
# git FOO 
################################

gitpush :
	git push origin $(GIT_BRANCH)

gitpull :
	git pull origin $(GIT_BRANCH)

tag :
	@-echo "git tag -a r$(VERSION) -m 'release $(VERSION)'"
	@-echo "git push --tags"

################################
# CRAN SUBMISSION
################################

# FTP junk
~/.netrc :
	echo -e "machine cran.r-project.org login anonymous password anonymous macdef init\ncd incoming\n\n" > $@

.cran_upload : $(PKG_TGZ)
	@-read -p 'really upload? [y/n] ' -n 1 yorn ; \
	[[ "$$yorn" == "y" ]] && echo -e "user anonymous anonymous\nbinary\ncd incoming\nput $(PKG_TGZ)\nls\nbye\n" | ftp -n -v cran.r-project.org

.send_email : 
	@-read -p 'really send email? [y/n] ' -n 1 yorn ; \
	[[ "$$yorn" == "y" ]] && echo "automatic message" | mail -s "CRAN submission $(PKG_NAME) $(VERSION)" CRAN@R-project.org

submit : .cran_upload .send_email

subadvice :
	@-echo -e "make docs && make build && make check"
	@-echo -e "upload $(PKG_TGZ) to cran.r-project.org/incoming via anonymous ftp"
	@-echo -e "then email CRAN@R-project.org w/ subject 'CRAN submission $(PKG_NAME) $(VERSION)'"

#vignette:
#cd $(VIGNETTE_D);\
#$(R) CMD Sweave $(PKG_NAME).Rnw;\
#texi2dvi --pdf $(PKG_NAME).tex;\
#$(R) --vanilla --slave -e "tools:::compactPDF(getwd(), gs_quality='printer')"

mactex : 
	sudo port install -v \
		texlive texlive-basic texlive-bibtex-extra texlive-bin texlive-bin-extra \
		texlive-common texlive-fonts-extra texlive-fonts-recommended \
		texlive-fontutils texlive-formats-extra texlive-generic-extra \
		texlive-generic-recommended texlive-latex texlive-latex-extra \
		texlive-latex-recommended texlive-math-extra 

# cheesy checkin:
#
# sleep `jot -r 1 5 55` && git commit -a -m 'working on vignette'
# git push origin dev1311

suggestions : 
	@-echo 'make docs'
	@-echo 'sleep `jot -r 1 2 57` && git commit -a -m "working on package"'
	@-echo "git push origin $(GIT_BRANCH)"

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
