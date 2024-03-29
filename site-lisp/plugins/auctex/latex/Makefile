# Makefile for AUCTeX

# Maintainer: auctex-devel@gnu.org

# Copyright (C) 2002-2006, 2008, 2014, 2015, 2019 Free Software Foundation,
#   Inc.

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


LATEX=/Library/TeX/texbin/latex
PDFLATEX=/Library/TeX/texbin/pdflatex
TEX=/Library/TeX/texbin/tex
TEXHASH=/Library/TeX/texbin/texhash

prefix=/usr/local$(null)
datarootdir=${prefix}/share$(null)
datadir=${datarootdir}$(null)
exec_prefix=${prefix}$(null)
libdir=${exec_prefix}/lib$(null)
previewtexmfdir = /Users/zerrari/Library/texmf/tex/latex/preview$(null)
previewdocdir = /Users/zerrari/Library/texmf/doc/latex/styles$(null)
TEXMFGEN = preview.sty prauctex.def prauctex.cfg prshowbox.def prshowlabels.def prtracingall.def prtightpage.def prlyx.def prcounters.def prfootnotes.def

DESTDIR=
INSTALL = /usr/local/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644
MKINSTALLDIRS = ../mkinstalldirs

.PHONY: all install install-texmf install-texmf-doc uninstall clean distclean

all: $(TEXMFGEN) preview.drv preview-mk.ins preview.pdf

install: install-texmf install-texmf-doc

install-texmf: $(TEXMFGEN)
	-$(MKINSTALLDIRS) $(DESTDIR)$(previewtexmfdir)
	for x in $(TEXMFGEN) ; do \
	  echo $(INSTALL_DATA) $$x $(DESTDIR)$(previewtexmfdir) ; \
	  $(INSTALL_DATA) $$x $(DESTDIR)$(previewtexmfdir) ; \
	done
# Isn't the texmfdir directory without "tex/latex/preview" available?
	@ texmfdir="$(DESTDIR)$(previewtexmfdir)/../../.."; \
	if [ -r "$$texmfdir"/ls-R ]; then \
	  $(TEXHASH) "$$texmfdir";  \
	else \
	  echo "Warning: Cannot update ls-R database in $$texmfdir"; \
	  sleep 2; \
	fi

install-texmf-doc: preview.pdf
	$(MKINSTALLDIRS) $(DESTDIR)$(previewdocdir)
	$(INSTALL_DATA) preview.pdf $(DESTDIR)$(previewdocdir)

uninstall:
	rm -rf $(DESTDIR)$(previewdocdir)/preview.dvi $(DESTDIR)$(previewdocdir)/preview.pdf $(DESTDIR)$(previewtexmfdir)

preview-mk.ins: preview.dtx bootstrap.ins
	$(TEX) '\nonstopmode \input bootstrap.ins'

#only for standalone preview.sty package:
preview.ins: preview.dtx
	$(TEX) '\nonstopmode\def\jobname{.ins}\input docstrip ' \
	'\generate{\file{preview.ins}{\from{preview.dtx}{installer}}}' \
	'\endbatchfile'

$(TEXMFGEN) preview.drv: preview.dtx preview-mk.ins
	$(TEX) '\nonstopmode \input preview-mk.ins'

preview.dvi: preview.drv preview.dtx preview.sty
	$(LATEX) '\nonstopmode \input preview.drv'
	$(LATEX) '\nonstopmode \input preview.drv'
	$(LATEX) '\nonstopmode \input preview.drv'

preview.pdf: preview.drv preview.dtx preview.sty
	$(PDFLATEX) '\nonstopmode \input{preview.drv}'
	$(PDFLATEX) '\nonstopmode \input{preview.drv}'
	$(PDFLATEX) '\nonstopmode \input{preview.drv}'

clean:
	rm -f *~ *.aux *.dvi *.drv *.log preview.pdf
	rm -f $(TEXMFGEN) preview.ins preview-mk.ins

distclean: clean
	rm -f Makefile
