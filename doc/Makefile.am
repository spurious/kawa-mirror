SUFFIXES = .pdf

kawa.txt:  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

KAWA_HTMLS = kawa/index.html
$(KAWA_HTMLS):  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) --html $<
GENERATED_HTML = $(KAWA_HTMLS)

TEXI2PDF = texi2pdf

.texi.pdf:
	TEXINPUTS=$(top_srcdir)/../texinfo/texinfo.tex:$$TEXINPUTS \
	  MAKEINFO='$(MAKEINFO) -I $(srcdir)' $(TEXI2PDF) $<

man_MANS = kawa.1 qexo.1
kawa.1: $(srcdir)/kawa.man
	nroff -man $(srcdir)/kawa.man | col -b > kawa1.tmp
	mv kawa1.tmp kawa.1

qexo.1: $(srcdir)/qexo.man
	nroff -man $(srcdir)/qexo.man | col -b > qexo1.tmp
	mv qexo1.tmp qexo.1

../kawa-doc-$(VERSION).tar.gz: kawa/index.html kawa.pdf kawa-tour.ps
	tar cf - kawa/*.html kawa.pdf kawa-tour.ps|gzip -c --best >$@

EXTRA_DIST = kawa.txt scm2java.html kawa.man qexo.man \
  kawa-tour.tex mdate-sh
info_TEXINFOS = kawa.texi kawa-tour.texi
CLEANFILES = kawa.txt *.info kawa/*.html kawa1.tmp kawa.1 qexo.1

# Redundant if configured with  --enable-maintainer-mode.
# Work around a questionable autoconf feature.
version.texi: stamp-vti

HTML_HOME = $$HOME/public_html

install-html:
	-mkdir $(HTML_HOME)/kawa
	cp $(KAWA_HTML_PARTS) $(HTML_HOME)/kawa/
	cp kawa_toc.html $(HTML_HOME)/kawa/kawa_toc.html
	cp kawa_toc.html $(HTML_HOME)/kawa/index.html
	sed -e 's|HREF="kawa_|HREF="kawa/kawa_|g' -e 's|HREF="../|HREF="|g' \
		<kawa_toc.html >$(HTML_HOME)/kawa.html
