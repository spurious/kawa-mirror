AUTOMAKE_OPTIONS = cygnus

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

man_MANS = kawa.1
kawa.1: $(srcdir)/kawa.man
	nroff -man $(srcdir)/kawa.man | col -b > kawa1.tmp
	mv kawa1.tmp kawa.1

../kawa-doc-$(VERSION).tar.gz: kawa/index.html internals.html kawa-tour.ps
	tar cf - kawa/*.html internals.html kawa-tour.ps|gzip -c --best >$@

EXTRA_DIST = kawa.txt scm2java.html kawa.man \
  kawa-tour.tex mdate-sh
info_TEXINFOS = kawa.texi kawa-tour.texi
CLEANFILES = kawa.txt *.info kawa/*.html internals.html kawa1.tmp kawa.1

HTML_HOME = $$HOME/public_html

install-html:
	-mkdir $(HTML_HOME)/kawa
	cp $(KAWA_HTML_PARTS) $(HTML_HOME)/kawa/
	cp kawa_toc.html $(HTML_HOME)/kawa/kawa_toc.html
	cp kawa_toc.html $(HTML_HOME)/kawa/index.html
	sed -e 's|HREF="kawa_|HREF="kawa/kawa_|g' -e 's|HREF="../|HREF="|g' \
		<kawa_toc.html >$(HTML_HOME)/kawa.html

XSLT = xsltproc

internals.html: $(srcdir)/internals.xml
	$(XSLT) /opt/docbook-xsl/html/docbook.xsl $< > $@

TransformToLatex.class:
	ln -s /home/bothner/public_html/papers/Freenix00/TransformToLatex.class .
usenix.sty:
	ln -s /home/bothner/public_html/papers/Freenix00/usenix.sty .

internals.tex: $(srcdir)/internals.xml TransformToLatex.class
	CLASSPATH=$(XSL_CLASSPATH) \
	  $(JAVA) TransformToLatex $(srcdir)/internals.xml /home/bothner/public_html/papers/Freenix00/docbook-to-latex.xsl internals.tex

internals.dvi: internals.tex usenix.sty
	latex internals.tex

internals.ps: internals.dvi
	dvips -o internals.ps internals.dvi
