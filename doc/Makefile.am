info_TEXINFOS = kawa-tour.texi

kawa.txt:  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

kawa.html:  $(srcdir)/kawa.texi version.texi
	texi2html -menu $<

EXTRA_DIST = kawa.txt kawa.html scm2java.html license.terms \
  kawa-tour.tex kawa-tour.ps Compliance.html
info_TEXINFOS = kawa.texi
