info_TEXINFOS = kawa-tour.texi

kawa.txt:  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

kawa_toc.html:  $(srcdir)/kawa.texi version.texi
	texi2html -menu -split_chapter $<

kawa.html: kawa_toc.html

EXTRA_DIST = kawa.txt kawa_toc.html kawa_[1-9*.html scm2java.html \
  license.terms kawa-tour.tex kawa-tour.ps Compliance.html
info_TEXINFOS = kawa.texi
CLEANFILES = kawa.txt *.info kawa_toc.html kawa_[1-9]*.html
