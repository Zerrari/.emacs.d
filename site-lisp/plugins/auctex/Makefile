# Makefile for AUCTeX

# Maintainer: auctex-devel@gnu.org

# Copyright (C) 2003-2008, 2010, 2013-2015, 2018-2019 Free Software
#   Foundation, Inc.

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



prefix = /usr/local$(null)
exec_prefix = ${prefix}$(null)
libdir = ${exec_prefix}/lib$(null)
packagedatadir = ${datadir}/emacs/site-lisp/auctex$(null)
datarootdir = ${prefix}/share$(null)
datadir = ${datarootdir}$(null)
localstatedir = ${prefix}/var$(null)
infodir = ${datarootdir}/info$(null)
lispdir = ${datadir}/emacs/site-lisp$(null)
auctexstartfile=${datadir}/emacs/site-lisp/auctex.el
previewstartfile = ${datadir}/emacs/site-lisp/preview-latex.el
packagelispdir = ${datadir}/emacs/site-lisp/auctex$(null)
packagedatadir = ${datadir}/emacs/site-lisp/auctex$(null)
autodir = ${localstatedir}/auctex$(null)
preview_enabled=yes
styledir = $(packagedatadir)/style
AUCTEXDATE = 2021-08-04
AUCTEXVERSION = 12.3

INSTALL = /usr/local/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644
MKINSTALLDIRS = ./mkinstalldirs
DESTDIR=

PACKAGE=auctex
PACKAGE_INFO=auctex preview-latex
EMACS=/usr/local/bin/emacs
ELCC=$(EMACS) -batch -q -no-site-file -no-init-file -l lpath.el
AUTOLOAD=--eval '(let ((autoload-file (expand-file-name "$@"))) \
  (mapcar (lambda (file) \
            (update-file-autoloads file nil autoload-file)) \
          command-line-args-left) \
  (save-buffers-kill-emacs t))'

PREVIEW_AUTOLOAD=--eval '(let ((autoload-file (expand-file-name "$@"))) \
  (update-file-autoloads "preview.el" nil autoload-file) \
  (save-buffers-kill-emacs t))'

# Files and directories excluded from distributed tar ball.
EXCLUDEDFILES=autogen.sh .gitignore doc/.gitignore doc/tex-ref.log \
	latex/.gitignore README.GIT tests build-aux

.SUFFIXES: .el .elc .texi

SHELL = /bin/sh
CP = cp -p
CP_A = $(CP) -R

DIST_PREFIX=$(PWD)/auctex-dist
FTPDIR = $(DIST_PREFIX)/ftp
WWWDIR = $(DIST_PREFIX)/www
CTANDIR = $(DIST_PREFIX)/ctan
PREVIEW_BUILD_DIR = preview-build
COMMITTER="`git config --get user.name`\ \ \<`git config --get user.email`\>"
RPMROOT = /usr/src/redhat
RPM_SIGN = --sign

AUCSRC = tex.el tex-buf.el tex-style.el plain-tex.el latex.el tex-info.el \
	texmathp.el multi-prompt.el tex-mik.el font-latex.el tex-font.el \
	context.el context-en.el context-nl.el tex-fold.el tex-jp.el \
	toolbar-x.el tex-bar.el bib-cite.el tex-ispell.el latex-flymake.el
AUCELC = $(AUCSRC:.el=.elc)

STYLESRC = style/prosper.el \
	   style/slides.el    style/foils.el     style/amstex.el \
	   style/article.el   style/book.el      style/letter.el \
	   style/report.el    style/amsart.el    style/amsbook.el \
	   style/epsf.el      style/psfig.el     style/latexinfo.el \
	   style/dutch.el     style/german.el    style/dk.el \
	   style/j-article.el style/j-book.el    style/j-report.el \
	   style/jarticle.el  style/jbook.el     style/jreport.el \
	   style/tarticle.el  style/tbook.el     style/treport.el \
	   style/jsarticle.el style/jsbook.el    style/jsreport.el \
	   style/dinbrief.el  style/virtex.el    style/plfonts.el \
	   style/plhb.el      style/harvard.el	 style/swedish.el \
	   style/danish.el    style/slovak.el    style/czech.el \
	   style/amsmath.el   style/amstext.el   style/amsbsy.el \
	   style/amsopn.el    style/amsthm.el	 style/natbib.el \
	   style/index.el     style/makeidx.el   style/multind.el \
	   style/varioref.el  style/fancyref.el	 style/mdwlist.el \
	   style/ngerman.el   style/graphicx.el	 style/graphics.el \
	   style/verbatim.el  style/scrbase.el   style/scrartcl.el \
	   style/scrbook.el   style/scrreprt.el	 style/scrlttr2.el \
	   style/scrpage2.el  style/captcont.el  style/subfigure.el \
	   style/paralist.el  style/booktabs.el  style/emp.el \
	   style/jura.el      style/alphanum.el	 style/beamer.el \
	   style/alltt.el     style/italian.el   style/doc.el \
	   style/ltxdoc.el    style/ltx-base.el	 style/units.el \
	   style/nicefrac.el  style/url.el       style/listings.el \
	   style/inputenc.el  style/frenchb.el   style/francais.el \
	   style/MinionPro.el style/pdfsync.el   style/polish.el \
	   style/polski.el    style/icelandic.el style/comment.el \
	   style/flashcards.el style/pstricks.el style/nomencl.el \
	   style/hyperref.el  style/bulgarian.el style/CJK.el \
	   style/ruby.el      style/CJKutf8.el   style/tabularx.el \
	   style/multido.el   style/pst-grad.el  style/pst-node.el \
	   style/pst-plot.el  style/pst-slpe.el  style/shortvrb.el \
	   style/austrian.el  style/naustrian.el style/french.el \
	   style/lettrine.el  style/multicol.el  style/xspace.el \
	   style/setspace.el  style/biblatex.el  style/siunitx.el \
	   style/bm.el        style/footmisc.el  style/ulem.el \
	   style/bigdelim.el  style/bigstrut.el  style/everysel.el \
	   style/mathtools.el style/ragged2e.el  style/amssymb.el \
	   style/lscape.el    style/epigraph.el  style/mflogo.el \
	   style/multirow.el  style/imakeidx.el  style/afterpage.el \
	   style/longtable.el style/lipsum.el    style/kantlipsum.el \
	   style/memoir.el    style/placeins.el  style/nameref.el \
	   style/fancynum.el  style/fancyhdr.el	 style/filecontents.el \
	   style/array.el     style/kpfonts.el   style/acro.el \
	   style/acronym.el   style/xparse.el    style/fancyvrb.el \
	   style/tabulary.el  style/fontspec.el  style/unicode-math.el \
	   style/luacode.el   style/metalogo.el  style/english.el \
	   style/exercise.el  style/plext.el     style/cleveref.el \
	   style/eso-pic.el   style/fontaxes.el  style/AnonymousPro.el \
	   style/mdsymbol.el  style/MyriadPro.el style/textcomp.el \
	   style/XCharter.el  style/zlmtt.el     style/ifluatex.el \
	   style/luatextra.el style/erewhon.el   style/baskervaldx.el \
	   style/fbb.el       style/newtxmath.el style/newtxsf.el \
	   style/newtxtext.el style/newtxttt.el  style/minted.el \
	   style/wrapfig.el   style/relsize.el   style/currvita.el \
	   style/tcolorbox.el style/color.el	 style/expl3.el \
	   style/bidibeamer.el style/enumitem.el style/caption.el \
	   style/geometry.el  style/ltablex.el   style/ltxtable.el \
	   style/mn2e.el      style/colortbl.el  style/attachfile.el \
	   style/newpxtext.el style/newpxmath.el style/pdfpages.el \
	   style/mnras.el     style/environ.el   style/polyglossia.el \
	   style/vwcol.el     style/textpos.el   style/transparent.el \
	   style/fontenc.el   style/Alegreya.el  style/gloss-italian.el \
	   style/newfloat.el  style/subcaption.el style/AlegreyaSans.el \
	   style/hologo.el    style/theorem.el   style/ntheorem.el \
	   style/splitidx.el  style/tikz.el      style/xcolor.el \
	   style/pdflscape.el style/commath.el   style/empheq.el \
	   style/framed.el    style/paracol.el   style/menukeys.el \
	   style/bidi.el      style/FiraMono.el  style/FiraSans.el \
	   style/bicaption.el style/amsfonts.el  style/subfiles.el \
	   style/dcolumn.el   style/mdframed.el  style/tcolorboxlib-raster.el \
	   style/titleps.el   style/titlesec.el  style/titletoc.el \
	   style/exam.el      style/breqn.el     style/sourcecodepro.el \
	   style/preview.el   style/fvextra.el   style/jurabib.el \
	   style/csquotes.el  style/babel.el     style/dk-bib.el \
	   style/floatrow.el  style/moodle.el    style/canadian.el \
	   style/arabxetex.el style/australian.el style/newzealand.el \
	   style/xltabular.el style/marginnote.el style/thmtools.el \
	   style/ocg-p.el     style/ocgx.el      style/thm-restate.el \
	   style/pythontex.el style/dashundergaps.el style/beamerarticle.el \
	   style/changelog.el style/ltugboat.el  style/beamerswitch.el \
	   style/multitoc.el  style/fbox.el      style/xkcdcolors.el \
	   style/ltxguide.el  style/revtex4-2.el style/overpic.el \
	   style/tex-live.el  style/algorithm.el style/algpseudocode.el

STYLEELC = $(STYLESRC:.el=.elc)

ifeq (yes,yes)
   PREVIEWSRC = preview.el
   PREVIEWELC = $(PREVIEWSRC:.el=.elc)
   PREVIEWLATEX = $(MAKE) preview-latex.el
   TEXMF = (cd latex ; $(MAKE) all)

   # Install actions
   INS_TEXMF = (cd latex ; $(MAKE) DESTDIR=$(DESTDIR) install)
   PREVIEW_INS_STARTUP = \
     $(INSTALL_DATA) preview-latex.el $(DESTDIR)$(previewstartfile)
else
   # Don't do anything with preview if the user doesn't want the
   # preview component.
   PREVIEWSRC =
   PREVIEWELC =
   PREVIEWLATEX =
   TEXMF =
   INS_TEXMF =
   PREVIEW_INS_STARTUP =
endif



CLEANFILES = $(AUCELC) $(STYLEELC) $(PREVIEWELC) preview-latex.el
DISTCLEANFILES = Makefile tex-site.el tex-site.el.out auctex.el \
	auto-loads.el config.* preview.el
DISTTEXTS = FAQ INSTALL INSTALL.windows README TODO PROBLEMS.preview

NOSEARCH = style/.nosearch

all: texmf lisp docs

.PHONY: all lisp info dvi some install install-el install-lisp docs	\
	install-docs install-auto install-startup			\
	clean distclean maintainer-clean extraclean			\
	tar-ball-clean check-tag check release-commit dist snapshot	\
	patch tar-ball rpm-packages wc windows-package	\
	release-sign release-upload preview-ball uninstall

texmf: latex/Makefile
	$(TEXMF)

lisp:	tex-site.el $(AUCSRC) $(STYLESRC) $(PREVIEWSRC)
	$(ELCC) -f batch-byte-compile $(AUCSRC) $(STYLESRC) $(PREVIEWSRC)
	$(PREVIEWLATEX)

preview-latex.el: preview.el
	rm -f preview-latex.el
	-$(EMACS) -batch -no-site-file -no-init-file $(PREVIEW_AUTOLOAD) ; \
	test -r $@ || { \
	  echo ";; Auto-generated preview-latex.el" > $@ ; \
	  echo "" >> $@ ; \
	  $(EMACS) -batch -no-site-file -no-init-file $(PREVIEW_AUTOLOAD) ; \
	}

docs:
	cd doc ; $(MAKE) all

info:
	cd doc ; $(MAKE) info

dvi:
	cd doc ; $(MAKE) dvi

tex-site.el: tex-site.el.out auto-loads.el Makefile
	cat tex-site.el.out >$@
	cat auto-loads.el >>$@
	echo "(provide 'tex-site)" >>$@ ; \
	echo ";;; tex-site.el ends here" >>$@

tex-site.el.out: tex-site.el.in Makefile config.status
	./config.status

auctex.el: auctex.el.in config.status
	./config.status

latex/Makefile: latex/Makefile.in Makefile config.status
	./config.status

configure: configure.ac aclocal.m4
	autoconf

Makefile: Makefile.in config.status
	./config.status

doc/Makefile: doc/Makefile.in config.status ChangeLog.1
	./config.status

config.status: configure
	./config.status --recheck

auto-loads.el: $(AUCSRC) Makefile
	rm -f $@
	-$(ELCC) $(AUTOLOAD) $(AUCSRC)
	test -r $@ || { \
	  echo ";; Auto-generated part of tex-site.el" > $@ ; \
	  echo "" >> $@ ; \
	  $(ELCC) $(AUTOLOAD) $(AUCSRC) ; \
	}

some:	$(AUCELC) $(STYLEELC)

install: install-texmf install-lisp install-docs install-images \
         install-startup

install-startup:
	$(PREVIEWLATEX)
	$(PREVIEW_INS_STARTUP)
	$(INSTALL_DATA) auctex.el $(DESTDIR)$(auctexstartfile)

install-texmf:
	$(INS_TEXMF)

install-el:
	-$(MKINSTALLDIRS) $(DESTDIR)$(lispdir)
	rm -f $(DESTDIR)$(packagelispdir)/tex-site.el # Remove old (Git-version) mistakes
	test ! -f $(DESTDIR)$(lispdir)/tex-site.el || { \
	   if grep -q "tex-site.*Don't edit." $(DESTDIR)$(lispdir)/tex-site.el; then \
	    echo "Overwriting old tex-site.el" ; \
	   else \
	    echo "Renaming old tex-site.el to tex-site.el.save" ; \
	    mv $(DESTDIR)$(lispdir)/tex-site.el $(DESTDIR)$(lispdir)/tex-site.el.save ; \
	   fi; \
	}
	$(INSTALL_DATA) tex-site.el $(DESTDIR)$(lispdir)
	-$(MKINSTALLDIRS) $(DESTDIR)$(packagelispdir)
	@: $(MAKE) ; for x in $(AUCSRC) $(PREVIEWSRC); do \
	  echo '$(INSTALL_DATA)' $$x '$(DESTDIR)$(packagelispdir)' ; \
	done
	@for x in $(AUCSRC) $(PREVIEWSRC); do \
	  $(INSTALL_DATA) $$x $(DESTDIR)$(packagelispdir) ; \
	done;
	-$(MKINSTALLDIRS) $(DESTDIR)$(styledir)
	$(INSTALL_DATA) $(NOSEARCH) $(DESTDIR)$(styledir)
	@: $(MAKE) ; for x in $(STYLESRC) ; do \
	  echo '$(INSTALL_DATA)' $$x '$(DESTDIR)$(styledir)' ; \
	done;
	@for x in $(STYLESRC) ; do \
	  $(INSTALL_DATA) $$x $(DESTDIR)$(styledir) ; \
	done;
	-$(MKINSTALLDIRS) $(DESTDIR)$(autodir)
	-$(INSTALL_DATA) $(NOSEARCH) $(DESTDIR)$(autodir)

install-lisp: some install-el
	@: $(MAKE) ; for x in $(AUCSRC) $(PREVIEWSRC); do \
	  echo '$(INSTALL_DATA)' $${x}c '$(DESTDIR)$(packagelispdir)' ; \
	done
	@for x in $(AUCSRC) $(PREVIEWSRC); do \
	  $(INSTALL_DATA) $${x}c $(DESTDIR)$(packagelispdir) ; \
	done
	@: $(MAKE) ; for x in $(STYLESRC) ; do \
	  echo '$(INSTALL_DATA)' $${x}c '$(DESTDIR)$(styledir)' ; \
	done
	@for x in $(STYLESRC) ; do \
	  $(INSTALL_DATA) $${x}c $(DESTDIR)$(styledir) ; \
	done

install-images:
	-$(MKINSTALLDIRS) $(DESTDIR)$(packagedatadir)/images
	@: $(MAKE) ; for x in images/*.xpm ; do \
	  echo '$(INSTALL_DATA)' $${x} '$(DESTDIR)$(packagedatadir)/images' ; \
	done
	@for x in images/*.xpm ; do \
	  $(INSTALL_DATA) $${x} $(DESTDIR)$(packagedatadir)/images ; \
	done

install-docs:	doc/Makefile
	cd doc ; $(MAKE) DESTDIR=$(DESTDIR) install-auctex
	test "x$(preview_enabled)" != xyes || { \
		cd doc ; $(MAKE) DESTDIR=$(DESTDIR) install-preview; }

install-auto:
	@echo "Use \"M-x TeX-auto-generate-global RET\" instead."

uninstall:
	rm -rf $(DESTDIR)$(packagelispdir) $(DESTDIR)$(packagedatadir) \
		$(DESTDIR)$(autodir)
	rm -f $(DESTDIR)$(lispdir)/tex-site.el $(DESTDIR)$(auctexstartfile)
	cd doc ; $(MAKE) DESTDIR=$(DESTDIR) uninstall
	rm -f $(DESTDIR)$(previewstartfile)
	rm -rf $(DESTDIR)$(packagelispdir)
	(cd latex ; $(MAKE) DESTDIR=$(DESTDIR) uninstall)

.el.elc:
	$(ELCC) -f batch-byte-compile $<

clean:	doc/Makefile latex/Makefile
	rm -f $(CLEANFILES)
	cd doc && $(MAKE) clean
	cd latex && $(MAKE) clean

distclean: doc/Makefile latex/Makefile
	rm -f $(CLEANFILES) $(DISTCLEANFILES)
	cd doc && $(MAKE) distclean
	cd latex && $(MAKE) distclean

maintainer-clean: doc/Makefile latex/Makefile
	rm -f $(DISTTEXTS)
	rm -rf autom4te.cache
	rm -f $(CLEANFILES) $(DISTCLEANFILES)
	cd doc && $(MAKE) maintainer-clean
	cd latex && $(MAKE) distclean
	cd tests && $(MAKE) clean

extraclean: maintainer-clean
	rm -f *~ \#*\#

tar-ball-clean: check-tag
	rm -rf auctex-$(TAG)/

wc:
	wc $(AUCSRC) $(STYLESRC)

# ----------------------------------------------------------------------
# The targets below update the ChangeLog from git log
# and are intended to be used only by the maintainers.
# These rules are adapted from GNU Emacs Makefile.

.PHONY: ChangeLog change-history change-history-commit change-history-nocommit
.PHONY: preferred-branch-is-current unchanged-history-files

CHANGELOG = ChangeLog
auctexlog = build-aux/gitlog-to-auctexlog
# The ChangeLog history files are called ChangeLog.1, ChangeLog.2, ...,
# ChangeLog.$(CHANGELOG_HISTORY_INDEX_MAX).  $(CHANGELOG_N) stands for
# the newest (highest-numbered) ChangeLog history file.
CHANGELOG_HISTORY_INDEX_MAX = 1
CHANGELOG_N = ChangeLog.$(CHANGELOG_HISTORY_INDEX_MAX)

# Check that we are in a good state for changing history.
PREFERRED_BRANCH = master
preferred-branch-is-current:
	git branch | grep -q '^\* $(PREFERRED_BRANCH)$$'
unchanged-history-files:
	x=$$(git diff-files --name-only $(CHANGELOG_N) $(auctexlog)) && \
	  test -z "$$x"

# Convert git commit log to ChangeLog file.  make-dist uses this.
# I guess this is PHONY so it always updates?
ChangeLog:
	  ./$(auctexlog) -o $(CHANGELOG) -n $(CHANGELOG_HISTORY_INDEX_MAX)

# Regular expression that matches the newest commit covered by a ChangeLog.
new_commit_regexp = ^commit [0123456789abcdef]* (inclusive)

# Copy newer commit messages to the start of the ChangeLog history file,
# and consider them to be older.
change-history-nocommit: #preferred-branch-is-current unchanged-history-files
	-rm -f ChangeLog.tmp
	$(MAKE) ChangeLog CHANGELOG=ChangeLog.tmp
	sed '/^This file records repository revisions/,$$d' \
	  ChangeLog.tmp > $(CHANGELOG_N).tmp
	new_commit_line=`grep --text '$(new_commit_regexp)' ChangeLog.tmp` && \
	sed 's/$(new_commit_regexp).*/'"$$new_commit_line/" \
	  $(CHANGELOG_N) >>$(CHANGELOG_N).tmp
	rm ChangeLog.tmp
	mv $(CHANGELOG_N).tmp $(CHANGELOG_N)

change-history: change-history-nocommit
	$(MAKE) $@-commit

# If 'make change-history' fails because the newest ChangeLog history
# file contains invalid text, fix the file by hand and then run
# 'make change-history-commit'.
change-history-commit:
	git commit -m'; make $@' $(CHANGELOG_N) $(auctexlog)

# ----------------------------------------------------------------------
# The targets below are concerned with the release process and
# intended to be used only by the maintainers.

# Steps for making a release:
#
# 0) check
#    Run tests using ERT tool.  Not necessary but strongly suggested before a
#    release.
#
# 1) change-history
#    Update the ChangeLog file.
#
# 2) release-commit TAG=<tag> COMMITTER=<committer>
#    Tag the release.
#    Pass `COMMITTER=<committer>' argument only if it is different from
#    your git name and email.  `<committer>' should be in the form
#        "John Doe  <john.doe@example.com>"
#
# 3) dist TAG=<tag>
#    Create the tar ball and other release files and put them into $FTPDIR.
#
# 4) windows-package WEMACSVER=<emacs-version> TAG=<tag>
#    Create the precompiled AUCTeX package for Windows.
#    This requires a compiled Emacs of the same version at location $WEMACS.
#    NOTA BENE: you may need to use bash shell for this recipe to work.
#
# 5) release-sign TAG=<tag>
#    Sign the tar ball and create directive files for upload.  NOTA BENE: you
#    may need to use bash shell for this recipe to work.
#
# 6) release-upload
#    Upload files to GNU FTP server.
#
# 7) www-doc TAG=<tag>
#    Create documentation for AUCTeX home page.
#
# 8) preview-ball TAG=<tag>
#    Create preview package.  (Not for GNU FTP server but for CTAN.)

dist: check-tag tar-ball

snapshot: dist

check-tag:
	@if [ "X$(TAG)" = "X" ]; then echo "*** Error: No TAG ***"; exit 1; fi

check:
	cd tests && $(MAKE) check

release-commit: check-tag
	@echo "Tagging release $(TAG) in Git ..."
	sleep 5
	mv $(CHANGELOG_N) ChangeLog.old
	mv ChangeLog-preview ChangeLog-preview.old
# Make sure the release ChangeLog entry is encoded with UTF-8.
# This requires the `iconv' program.
	echo `date "+%Y-%m-%d "`" ${COMMITTER}" | iconv -t UTF-8 - > $(CHANGELOG_N)
	echo >> $(CHANGELOG_N)
	echo "	* Version" $(TAG) released. >> $(CHANGELOG_N)
	echo >> $(CHANGELOG_N)
	cp $(CHANGELOG_N) ChangeLog-preview
	cat ChangeLog.old >> $(CHANGELOG_N)
	cat ChangeLog-preview.old >> ChangeLog-preview
	git commit -m '; Release_$(TAG)' -- $(CHANGELOG_N) ChangeLog-preview
	git tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
	@echo
	@echo "Congratulations!  Release $(TAG) of AUCTeX is ready."
	@echo "Please, remember to run"
	@echo "    git push --tags origin master"
	@echo "to propagate release commit and tag to the remote repository."

tar-ball: doc/Makefile
	test ! -d auctex-$(TAG) || rm -r auctex-$(TAG)
	mkdir auctex-$(TAG)
# Use TAG_EXPORT if set (for snapshots)
	git archive $${TAG_EXPORT:=`echo release_$(TAG) | sed 's/[.]/_/g'`} | tar -xC auctex-$(TAG)
	cd auctex-$(TAG) && AUCTEXVERSION=$(TAG) AUCTEXDATE=$(AUCTEXDATE) ./autogen.sh && rm -rf $(EXCLUDEDFILES)
	chmod -R go-w+rX auctex-$(TAG)
	rm -rf $(FTPDIR)
	mkdir -p $(FTPDIR)
	tar -cf - --owner=root --group=root auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
	-zip -r $(FTPDIR)/auctex-$(TAG).zip auctex-$(TAG)
	$(CP) auctex-$(TAG)/CHANGES $(FTPDIR)/CHANGES
	$(CP) auctex-$(TAG)/RELEASE $(FTPDIR)/RELEASE-$(TAG)
# The following files aren't produced in auctex-$(TAG) since they
# are not in the tarball and should not become part of the patch target.
	$(CP) auctex-$(TAG)/doc/version.texi doc/version.texi
	cd doc && $(MAKE) extradist
	rm -f doc/version.texi
	$(CP) doc/auctex.ps doc/auctex.pdf doc/tex-ref.ps doc/tex-ref.pdf $(FTPDIR)
	$(CP) $(CHANGELOG_N) $(FTPDIR)

rpm-packages:
	rpmbuild -ta $(RPM_SIGN) $(FTPDIR)/auctex-$(TAG).tar.gz
	$(CP) $(RPMROOT)/{RPMS/noarch/{auctex-emacs,preview-tetex},SRPMS/auctex}-$(TAG)[.-]* $(FTPDIR)
	@echo "If you need to prepare RPM on some other system, you need to add"
	@echo "them in $(FTPDIR) before 'make release-sign'."
	sleep 2

patch:
	diff -u auctex-$(OLD) auctex-$(TAG) | gzip --best \
		>$(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch.gz

# Pre-compiled AUCTeX package for Emacs on Windows
WBUILDDIR=$(PWD)/windows-package
WEMACS=/usr/src/emacs-$(WEMACSVER)/src/emacs
WPACKAGE=auctex-$(TAG)-e$(WEMACSVER)-msw.zip
# Starting from Emacs 24.4, the name of the Windows package and the tree
# structure of the package have been changed.  The following variables cater for
# these differences.
ifeq ($(shell echo "$(WEMACSVER)" | grep '\(2[2-3].[1-4]\|24.[1-3]\)' -),$(WEMACSVER))
	WEMACSDATAROOTDIR=""
	WEMACSEXTRACTDIR=""
	WEMACSPACKAGE="emacs-$(WEMACSVER)-bin-i386.zip"
	WEMACSSITELISP="site-lisp"
else
	WEMACSDATAROOTDIR="share/"
	WEMACSEXTRACTDIR="emacs-$(WEMACSVER)"
# And just to add more complexity, the package name changed again with Emacs
# 24.5.  Actually, we don't really need to keep track of all these changes as we
# should support only the latest Emacs version, but unless something else
# changes again again in the near future we can keep them (and it's useful to
# reproduce old AUCTeX packages!).
	ifeq (24.4,$(WEMACSVER))
		WEMACSPACKAGE="emacs-$(WEMACSVER)-bin-i686-pc-mingw32.zip"
	else
		WEMACSPACKAGE="emacs-$(WEMACSVER)-bin-i686-mingw32.zip"
	endif
	WEMACSSITELISP="$(WEMACSDATAROOTDIR)/emacs/site-lisp"
endif
WPACKAGEFILES=$(WEMACSSITELISP)/auctex $(WEMACSSITELISP)/site-start.d \
	$(WEMACSSITELISP)/site-start.el $(WEMACSSITELISP)/tex-site.el \
	var/auctex $(WEMACSDOC) $(WEMACSDATAROOTDIR)doc $(WEMACSDATAROOTDIR)info/dir \
	$(WEMACSDATAROOTDIR)info/auctex.info* $(WEMACSDATAROOTDIR)info/preview-latex.info*
windows-package: check-tag
	@if [ "X$(WEMACSVER)" = "X" ]; then \
	  echo "*** Error: No Emacs version (WEMACSVER) ***"; \
	  exit 1; \
	fi
# Make sure the prerequisites are available
	test -d $(WBUILDDIR) || mkdir $(WBUILDDIR)
	test -f $(WBUILDDIR)/$(WEMACSPACKAGE) \
	|| { cd $(WBUILDDIR) \
	&& wget https://ftp.gnu.org/gnu/emacs/windows/$(WEMACSPACKAGE); }
	test -f $(WBUILDDIR)/auctex-$(TAG).zip \
	|| { test -f $(FTPDIR)/auctex-$(TAG).zip \
	  && $(CP) $(FTPDIR)/auctex-$(TAG).zip $(WBUILDDIR); } \
	|| { cd $(WBUILDDIR) \
	  && wget https://ftp.gnu.org/gnu/auctex/auctex-$(TAG).zip; }
# Reset to clean state
	cd $(WBUILDDIR) \
	&& rm -rf emacs-$(WEMACSVER) \
	&& rm -rf auctex-$(TAG) \
	&& unzip -d "$(WEMACSEXTRACTDIR)" $(WEMACSPACKAGE) \
	&& unzip auctex-$(TAG).zip
	rm -f $(WPACKAGE)
# Create site-start.el and site-start.d
	cd $(WBUILDDIR)/emacs-$(WEMACSVER)/$(WEMACSSITELISP) \
	&& echo -e ";; Load files in \`site-start.d' directory.\
\n(dolist (file (directory-files\
\n	       (concat (file-name-directory load-file-name) \"site-start.d\")\
\n	       t \"\\\\\\\\.el\\\\\\\\'\"))\
\n  (load file nil t t))" > site-start.el \
	&& mkdir site-start.d
# Build and install AUCTeX
	cd $(WBUILDDIR)/auctex-$(TAG) \
	&& ./configure --prefix=$(WBUILDDIR)/emacs-$(WEMACSVER) \
	  --without-texmf-dir --with-emacs=$(WEMACS) \
	  --with-lispdir=$(WBUILDDIR)/emacs-$(WEMACSVER)/$(WEMACSSITELISP) \
	  --datarootdir=$(WBUILDDIR)/emacs-$(WEMACSVER)/$(WEMACSDATAROOTDIR) \
	&& $(MAKE) \
	&& sed -i -e 's/"\/[^\n]\+/(expand-file-name "..\/var\/auctex" (file-name-directory load-file-name))/' tex-site.el \
	&& $(MAKE) install
# Create the package
	test -d $(FTPDIR) || mkdir -p $(FTPDIR)
	rm -f $(FTPDIR)/$(WPACKAGE)
	cd $(WBUILDDIR)/emacs-$(WEMACSVER) \
	&& zip -r $(FTPDIR)/$(WPACKAGE) $(WPACKAGEFILES)

release-sign: check-tag
	rm -f $(FTPDIR)/*.{directive,asc,sig}
	if [ "x$$GPG_AGENT_INFO" = "x" ]; then \
	  read -sp "Enter pass phrase: " phrase ; \
	  gpg_opt="--passphrase-fd 0"; \
	else \
	  gpg_opt=""; phrase=""; \
	fi; \
	for i in $(FTPDIR)/*; do \
	   echo "$$phrase" | gpg --detach-sign $$gpg_opt $$i ; \
	   echo "version: 1.1" >$$i.directive; \
	   case $$i in *-$(TAG)*) echo "directory: auctex" >>$$i.directive;; \
	      *) echo "directory: auctex/$(TAG)-extra" >>$$i.directive ;; \
	   esac ; \
	   echo "filename: "`basename $$i` >>$$i.directive ; \
	   echo "$$phrase" | gpg $$gpg_opt --clearsign $$i.directive ; \
	done
	rm $(FTPDIR)/*.directive

release-upload:
	@echo "Uploading everything in FTPDIR to GNU FTP server ..."
	@sleep 5
	ncftpput ftp-upload.gnu.org /incoming/ftp $(FTPDIR)/*

# AUCTeX and preview-latex manuals for the home page
www-doc: check-tag
	rm -rf $(WWWDIR)
	mkdir -p $(WWWDIR)/manual
	$(CP) auctex-$(TAG)/doc/*.texi $(WWWDIR)/manual
	cd $(WWWDIR)/manual \
	&& wget -O gendocs.sh 'https://cvs.savannah.gnu.org/viewvc/%2acheckout%2a/texinfo/texinfo/util/gendocs.sh' \
	&& wget -O gendocs.sh.patch 'https://git.savannah.gnu.org/gitweb/?p=auctex.git;a=blob_plain;f=gendocs.sh.patch;hb=gendocspatch' \
	&& patch <gendocs.sh.patch \
	&& chmod u+x gendocs.sh \
	&& wget -O gendocs_template 'https://cvs.savannah.gnu.org/viewvc/%2acheckout%2a/texinfo/texinfo/util/gendocs_template' \
	&& sed -i -e 's/html_node\/index\.html/%%PACKAGE%%\/index.html/' gendocs_template \
	&& mkdir .texinfo \
	&& echo -e "emacs split https://www.gnu.org/software/emacs/manual/html_node/emacs/\nemacs mono https://www.gnu.org/software/emacs/manual/html_mono/emacs.html\nreftex split https://www.gnu.org/software/auctex/manual/reftex/\nreftex mono https://www.gnu.org/software/auctex/manual/reftex.html\n" > .texinfo/htmlxref.cnf \
	&& ./gendocs.sh --email bug-auctex@gnu.org --texi2html \
	  --html --use-nodes auctex "AUCTeX Manual" \
	&& ./gendocs.sh --email bug-auctex@gnu.org --texi2html \
	  --html --use-nodes preview-latex "preview-latex Manual"

preview-ball: check-tag
	test ! -d $(PREVIEW_BUILD_DIR) || rm -r $(PREVIEW_BUILD_DIR)
	mkdir $(PREVIEW_BUILD_DIR)
# Use TAG_EXPORT if set (for snapshots)
	git archive $${TAG_EXPORT:=`echo release_$(TAG) | sed 's/[.]/_/g'`} \
	  latex/ | tar -xC $(PREVIEW_BUILD_DIR) --strip-components=1
	cd $(PREVIEW_BUILD_DIR) && $(MAKE) -f ../latex/Makefile preview.ins preview.pdf
	chmod -R go-w+rX $(PREVIEW_BUILD_DIR)
	test -d $(CTANDIR) || mkdir -p $(CTANDIR)
# CTAN requires a top level directory "preview/" for the archive.
	cd $(PREVIEW_BUILD_DIR) && mkdir -p preview && \
	cp README preview.dtx preview.ins preview.pdf preview/ && \
	tar -cf - --owner=root --group=root preview/ | gzip --best > $(CTANDIR)/preview-$(TAG).tar.gz
