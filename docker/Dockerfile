#
# dockerfile to CRAN-check with r-dev
#
# docker build --rm -t shabbychef/madness-crancheck .
#
# docker run -it --rm --volume $(pwd):/srv:rw madness-crancheck
#
# Created: 2016.01.10
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav
# Comments: Steven E. Pav

#####################################################
# preamble# FOLDUP
FROM shabbychef/littler-devel-onbuild
MAINTAINER Steven E. Pav, shabbychef@gmail.com
USER root
# UNFOLD

WORKDIR /srv

# see http://stackoverflow.com/a/10017736/164611
ENV _R_CHECK_CRAN_INCOMING_ TRUE
ENV _R_CHECK_FORCE_SUGGESTS_ FALSE
ENV _R_CHECK_VC_DIRS_ TRUE
ENV _R_CHECK_UNSAFE_CALLS_ TRUE
ENV _R_CHECK_TIMINGS_ 10
ENV _R_CHECK_INSTALL_DEPENDS_ TRUE
ENV _R_CHECK_SUGGESTS_ONLY_ TRUE
ENV _R_CHECK_NO_RECOMMENDED_ TRUE
ENV _R_CHECK_SUBDIRS_NOCASE_ TRUE
ENV _R_CHECK_EXECUTABLES_EXCLUSIONS_ FALSE
ENV _R_CHECK_LICENSE_ TRUE
ENV _R_CHECK_DOC_SIZES2_ TRUE
ENV _R_CHECK_CODETOOLS_PROFILE_ 'suppressPartialMatchArgs=false'
ENV _R_CHECK_VIGNETTES_NLINES_ 50
ENV _R_CHECK_DOT_INTERNAL_ TRUE

#####################################################
# entry and cmd# FOLDUP
# always use array syntax:
ENTRYPOINT ["/usr/bin/R","CMD","check","--as-cran","--output=/tmp"]

# ENTRYPOINT and CMD are better together:
CMD ["/srv/*.tar.gz"]
# UNFOLD

#for vim modeline: (do not edit)
# vim:nu:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=Dockerfile:ft=Dockerfile:fo=croql
