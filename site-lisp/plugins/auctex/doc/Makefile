# Makefile for AUCTeX

# Maintainer: auctex-devel@gnu.org

# Copyright (C) 2003-2008, 2013-2015, 2018-2019 Free Software Foundation, Inc.

# This file is part of AUCTeX.

# AUCTeX is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# AUCTeX is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AUCTeX; see the file COPYING.  If not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.

prefix=/usr/local$(null)
datarootdir=${prefix}/share$(null)
datadir=${datarootdir}$(null)
infodir=${datarootdir}/info$(null)
docdir=${datarootdir}/doc/${PACKAGE_TARNAME}$(null)
TEX=/Library/TeX/texbin/tex
PDFTEX=/Library/TeX/texbin/pdftex
MAKEINFO=/usr/bin/makeinfo
MAKEINFO_PLAIN=-D rawfile --no-headers
INSTALL_INFO=/usr/bin/install-info
INSTALL=/usr/local/bin/ginstall -c
INSTALL_DATA=${INSTALL} -m 644
DESTDIR=
# If `texi2html' is not available, use `makeinfo' when possible.  Set the ToC
# file accordingly.  Actually, makeinfo >= 5 is needed, but we don't check the
# version.
ifneq (:,:)
	TEXI2HTML=:
	TEXI2HTML_TOC=auctex_toc.html
else
	ifneq (/usr/bin/makeinfo,:)
		TEXI2HTML=/usr/bin/makeinfo --html
		TEXI2HTML_TOC=index.html
	else
		TEXI2HTML=:
		TEXI2HTML_TOC=auctex_toc.html
	endif
endif
TEXI2DVI=/usr/bin/texi2dvi
TEXI2PDF=/usr/bin/texi2pdf
MKINSTALLDIRS = ../mkinstalldirs
DVIPS=/Library/TeX/texbin/dvips
PERL=/usr/local/bin/perl
AUCTEXDATE=2021-08-04
AUCTEXVERSION=12.3
PACKAGE_TARNAME=auctex

AUCTEXTEXIFILES = auctex.texi install.texi wininstall.texi intro.texi \
	changes.texi todo.texi faq.texi macros.texi quickstart.texi \
	fdl.texi version.texi
PREVIEWTEXIFILES = copying.texi preview-faq.texi macros.texi \
	preview-dtxdoc.texi preview-latex.texi preview-problems.texi \
	preview-readme.texi preview-todo.texi version.texi
DISTTEXTS = ../README ../INSTALL ../TODO ../INSTALL.windows ../FAQ ../CHANGES	\
	../PROBLEMS.preview

all: dist

info: auctex.info preview-latex.info

dvi: auctex.dvi tex-ref.dvi preview-latex.dvi

install: install-auctex install-preview

dist: $(DISTTEXTS) preview-latex.info auctex.info tex-ref.pdf

extradist: html/$(TEXI2HTML_TOC) auctex.ps auctex.pdf tex-ref.ps tex-ref.pdf

.PHONY: all info dvi dist install-auctex disttexts clean distclean \
	maintainer-clean install-preview uninstall html-docs \
	extradist

# AUCTeX

html/$(TEXI2HTML_TOC): auctex.texi
	rm -rf html
	mkdir html
	cd html && $(TEXI2HTML) --split=node -I .. ../auctex.texi && \
	test ! -d auctex || { mv auctex/* . && rm -rf auctex ; }

tex-ref.dvi: tex-ref.tex
	$(TEX) tex-ref

tex-ref.ps: tex-ref.dvi
	$(DVIPS) tex-ref.dvi -Ppdf -o tex-ref.ps

tex-ref.pdf: tex-ref.tex
	$(PDFTEX) tex-ref.tex

version.texi: ../ChangeLog.1
	echo @set VERSION $(AUCTEXVERSION) >version.texi
	echo @set UPDATED $(AUCTEXDATE) >>version.texi

auctex.dvi: $(AUCTEXTEXIFILES)
	$(TEXI2DVI) auctex.texi

auctex.pdf: $(AUCTEXTEXIFILES)
	$(TEXI2PDF) auctex.texi

auctex.ps: auctex.dvi
	$(DVIPS) auctex.dvi -Ppdf -o auctex.ps

auctex.info: $(AUCTEXTEXIFILES)
	$(MAKEINFO) auctex.texi

install-auctex: auctex.info tex-ref.pdf
	-$(MKINSTALLDIRS) $(DESTDIR)$(infodir)
	rm -f $(DESTDIR)$(infodir)/auctex $(DESTDIR)$(infodir)/auctex-*
	$(INSTALL_DATA) auctex.info $(DESTDIR)$(infodir)
	for x in auctex.info-*; do \
	  if [ -r $$x ]; then \
	    $(INSTALL_DATA) $$x $(DESTDIR)$(infodir) ; \
	  fi; \
	done
	-$(INSTALL_INFO)  --info-dir=$(DESTDIR)$(infodir) auctex.info
	$(MKINSTALLDIRS) $(DESTDIR)$(docdir)
	rm -f $(DESTDIR)$(docdir)/tex-ref.pdf
	$(INSTALL_DATA) tex-ref.pdf $(DESTDIR)$(docdir)

# preview

preview-latex/index.html: $(PREVIEWTEXIFILES)
	rm -f preview-latex/*
	$(TEXI2HTML) preview-latex.texi

html-docs: preview-latex/index.html

preview-dtxdoc.texi: ../latex/preview.dtx preview-dtxdoc.pl
	-$(PERL) preview-dtxdoc.pl ../latex/preview.dtx \
		preview-dtxdoc.texi

preview-latex.dvi: $(PREVIEWTEXIFILES)
	$(TEXI2DVI) preview-latex.texi

preview-latex.ps: preview-latex.dvi
	$(DVIPS) preview-latex.dvi -Ppdf -o preview-latex.ps

preview-latex.pdf: $(PREVIEWTEXIFILES)
	$(TEXI2PDF) preview-latex.texi

preview-latex.info: $(PREVIEWTEXIFILES)
	$(MAKEINFO) preview-latex.texi

install-preview: preview-latex.info
	-$(MKINSTALLDIRS) $(DESTDIR)$(infodir)
	for x in preview-latex.info* ; do \
		$(INSTALL_DATA) $$x $(DESTDIR)$(infodir) ; \
	done
	-$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) preview-latex.info

# disttexts

../INSTALL: install.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) install.texi --output $@

../INSTALL.windows: wininstall.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) wininstall.texi --output $@

../README: intro.texi preview-readme.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) intro.texi --output $@
	$(MAKEINFO) $(MAKEINFO_PLAIN) preview-readme.texi --output - >> $@

../CHANGES: changes.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) changes.texi --output $@

../TODO: todo.texi preview-todo.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) todo.texi --output $@
	$(MAKEINFO) $(MAKEINFO_PLAIN) preview-todo.texi --output - >> $@

../FAQ: faq.texi preview-faq.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) faq.texi --output $@
	$(MAKEINFO) $(MAKEINFO_PLAIN) --number-sections preview-faq.texi --output - >> $@

../PROBLEMS.preview: preview-problems.texi macros.texi
	$(MAKEINFO) $(MAKEINFO_PLAIN) --number-sections preview-problems.texi --output $@

# clean

uninstall:
	for info in $(DESTDIR)$(infodir)/{auctex.info,preview-latex.info}; do \
		test ! -f $${info} || \
		$(INSTALL_INFO) --delete --info-dir=$(DESTDIR)$(infodir) $${info}; \
	done
	rm -f $(DESTDIR)$(infodir)/auctex.info $(DESTDIR)$(infodir)/auctex.info-* \
	$(DESTDIR)$(infodir)/preview-latex.info*
	rm -rf -f $(DESTDIR)$(docdir)

clean:
	rm -f *.dvi *.ps *.pdf *.aux *.cp *.fn *.ky *.log *~ \#*\# \
		*.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps \
		*.fns *.vrs *.pgs *.tmp *.html
	rm -rf html preview-latex

distclean: clean
	rm -f Makefile

maintainer-clean: distclean
	rm -f preview-dtxdoc.texi version.texi $(DISTTEXTS) *.info *.info-*
