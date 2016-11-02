SUFFIXES = .pdf
# We cannot build kawa.dvi because of the png-only images
DVIS =

kawa.txt:  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

KAWA_HTMLDIR = kawa-html
KAWA_HTMLS = $(KAWA_HTMLDIR)/index.html
$(KAWA_HTMLS):  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) --html --no-node-files $< -o $(KAWA_HTMLDIR)
HTMLS = $(KAWA_HTMLS)

html: $(KAWA_HTMLS)

TEXI2PDF = texi2pdf

.texi.pdf:
	TEXINPUTS=$(top_srcdir)/../texinfo/texinfo.tex:$$TEXINPUTS \
	  MAKEINFO='$(MAKEINFO) -I $(srcdir)' $(TEXI2PDF) -E $<

man_MANS = kawa.1 qexo.1
kawa.1: $(srcdir)/kawa.man
	nroff -man $(srcdir)/kawa.man > tpm-kawa.1
	mv tpm-kawa.1 kawa.1

qexo.1: $(srcdir)/qexo.man
	nroff -man $(srcdir)/qexo.man > tpm-qexo1
	mv tpm-qexo1 qexo.1

../kawa-doc-$(VERSION).tar.gz: kawa.info $(KAWA_HTMLDIR)/index.html kawa.pdf
	tar cf - $(KAWA_HTMLDIR)/*.html kawa.pdf|gzip -c --best >$@

clean-local:
	-rm -rf $(KAWA_HTMLDIR) tpm-qexo1 tpm-qexo1 kawa.1 qexo.1 kawa-manual.epub web ebook

DOC_IMAGES = \
  images/border-1.png \
  images/fill-circ-1.png \
  images/image-scaled-1.png \
  images/image-cat-1a.png \
  images/image-cat-1b.png \
  images/padding-1.png \
  images/paint-circ-1.png \
  images/paint-circ-2.png \
  images/paint-circ-3.png \
  images/paint-circ-4.png \
  images/polygon-1.png \
  images/polyline-1.png \
  images/re-center-1.png \
  images/re-center-2.png \
  images/show-draw-1.png \
  images/show-draw-2.png \
  images/show-draw-3.png \
  images/domterm-pictures-1.png

EXTRA_DIST = scm2java.html kawa.man qexo.man mdate-sh +default+ README-epub \
  style/docbook-to-html.xsl style/kawa-epub.xsl style/kawa.xsl \
  style/kawa-1col.css style/kawa-ebook.css style/kawa-l.css \
  style/kawa-r.css style/kawa.css style/top-navbar.css \
  style/kawa-logo.png $(DOC_IMAGES)
info_TEXINFOS = kawa.texi
kawa_TEXINFOS = news.texi
MOSTLYCLEANFILES = kawa.xml \
  kawa.vr kawa.aux kawa.fn kawa.log kawa.cp kawa.toc \
  kawa.cps kawa.fns kawa.vrs
MAINTAINERCLEANFILES = kawa.txt *.info 

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


kawa.xml: $(srcdir)/kawa.texi
	$(MAKEINFO) -I=$(srcdir) --docbook $(srcdir)/kawa.texi -o - | \
	sed \
	-e 's|_002d|-|g' \
	-e 's|<book id="-" lang="en">|<book id="book" lang="en"><?dbhtml filename="top.html"?>|' \
	-e 's|<chapter label="" id="Top">|<chapter label="Top" id="Top"><?dbhtml filename="index.html"?>|' \
	-e 's|<chapter label="1" id="News">|<chapter label="1" id="News"><?dbhtml filename="news.html"?>|' \
	-e 's|id="Tutorial">|\0<?dbhtml filename="tutorial/index.html"?>|' \
	-e 's|id="Tutorial---\([^"]*\)">|\0<?dbhtml filename="tutorial/\1.html"?>|' \
	-e 's|<title>Compiling to byte-code or an executable</title>|\0<titleabbrev>Compiling</titleabbrev>|' \
	-e 's|<title>Scheme Extensions and Language Reference</title>|\0<titleabbrev>Language Reference</titleabbrev>|' \
	-e 's|<title>Summary of Kawa Scheme Features</title>|\0<titleabbrev>Feature Summary</titleabbrev>|' \
	-e 's|<title>Input, output, and file handling</title>|\0<titleabbrev>Input, output, files</titleabbrev>|' \
	-e 's|<title>How to start up and run Kawa</title>|\0<titleabbrev>Usage Reference (running Kawa)</titleabbrev>|' \
	-e 's|<title>Technical Support for Kawa</title>|\0<titleabbrev>Technical Support</titleabbrev>|' \
	-e 's|<title>Quantities and Numbers</title>|\0<titleabbrev>Numbers</titleabbrev>|' \
	-e 's|<title>Features of Kawa Scheme</title>|\0<titleabbrev>Scheme Features</titleabbrev>|' \
	-e 's|<title>Building and installing Kawa</title>|\0<titleabbrev>Building and installing</titleabbrev>|' \
	-e 's|<title>The Kawa Scheme system</title>|\0<titleabbrev>Kawa Home</titleabbrev>|' \
	-e 's|\([^<>]*\)<userinput>|<prompt>\1</prompt><userinput>|' \
	-e 's|<appendix .*id="Overall-Index">|<chapter label="" id="Overall-Index">|' \
	-e 's|</appendix>|</chapter>|' \
	-e '/@ifnotdocbook/,/@end ifnotdocbook/d' \
	> kawa.xml

XSLT = xsltproc
HTML_TARGET_DIRECTORY = $(builddir)/web
# Either override this, or set up a symlink to the DocBook XSL stylesheets.
DOCBOOK_XSL_DIR = ../../docbook-xsl

$(HTML_TARGET_DIRECTORY)/index.html: kawa.xml Makefile
	sed \
	  -e '/<title>Kawa Scheme Tutorial/,/<title>Reference Documentation/s|<imagedata fileref="images/|<imagedata fileref="../images/|' \
	  < $< | \
	$(XSLT) --path $(DOCBOOK_XSL_DIR)/html$(pathsep)$(srcdir)/style \
	  --stringparam base.dir $(HTML_TARGET_DIRECTORY)  \
	  --stringparam chunker.output.doctype-public "-//W3C//DTD HTML 4.01 Transitional//EN" \
	  --stringparam chunker.output.encoding UTF-8 \
	  --stringparam generate.section.toc.level 0 \
	  --stringparam generate.index 1 \
	  --stringparam use.id.as.filename 1 \
	  --stringparam chunker.output.indent yes \
	  --stringparam chunk.first.sections 1 \
	  --stringparam chunk.section.depth 1 \
	  --stringparam chapter.autolabel 0 \
	  --stringparam chunk.fast 1 \
	  --stringparam toc.max.depth 4 \
	  --stringparam toc.list.type ul \
	  --stringparam toc.section.depth 3 \
	  --stringparam chunk.separate.lots 1 \
	  --stringparam chunk.tocs.and.lots 1 \
	  kawa.xsl -
	mv $(HTML_TARGET_DIRECTORY)/bk01-toc.html $(HTML_TARGET_DIRECTORY)/ToC.html
	mkdir -p $(HTML_TARGET_DIRECTORY)/style
	cp $(srcdir)/style/kawa-logo.png $(srcdir)/style/*.css \
	  $(HTML_TARGET_DIRECTORY)/style
	mkdir -p $(HTML_TARGET_DIRECTORY)/images
	cp $(DOC_IMAGES) $(HTML_TARGET_DIRECTORY)/images/

# There is some weuid bug causing "tutorial/XXX.xhtml" to be
# treated as "XXX.xhtml", so use "tutorial-XXX.xhtml" instead.
kawa-manual.epub: kawa.xml Makefile
	sed \
	  -e 's|<?dbhtml filename="tutorial/\([^".]*\)\.html|<?dbhtml filename="tutorial-\1.xhtml|'\
	  -e 's|<?dbhtml filename="\([^"]*\)\.html|<?dbhtml filename="\1.xhtml|'\
	  < $< | \
	$(XSLT) --path $(DOCBOOK_XSL_DIR)/epub3$(pathsep)$(srcdir)/style \
	  --stringparam base.dir ebook/OEBPS/ \
	  --stringparam html.script kawa-ebook.js \
	  --stringparam html.stylesheet kawa.css \
	  --stringparam chunker.output.encoding UTF-8 \
	  --stringparam generate.section.toc.level 0 \
	  --stringparam generate.index 1 \
	  --stringparam use.id.as.filename 1 \
	  --stringparam autotoc.label.in.hyperlink 0 \
	  --stringparam chunker.output.indent yes \
	  --stringparam chunk.first.sections 1 \
	  --stringparam chunk.section.depth 1 \
	  --stringparam chapter.autolabel 0 \
	  --stringparam chunk.fast 1 \
	  --stringparam toc.max.depth 4 \
	  --stringparam toc.list.type ul \
	  --stringparam toc.section.depth 3 \
	  --stringparam chunk.separate.lots 1 \
	  --stringparam chunk.tocs.and.lots 1 \
	  kawa-epub.xsl -
	sed -e '/<footer>/,/<.footer>/d' <ebook/OEBPS/bk01-toc.xhtml \
	  >ebook/OEBPS/ToC.xhtml
	rm ebook/OEBPS/bk01-toc.xhtml
	cp style/kawa-ebook.css ebook/OEBPS/kawa.css
	cp style/kawa-ebook.js ebook/OEBPS/kawa-ebook.js
	cp with-frames.html ebook/OEBPS/with-frames.html
	mkdir -p ebook/OEBPS/images
	cp $(DOC_IMAGES) ebook/OEBPS/images/
	cd ebook && zip -r -X ../kawa-manual.epub mimetype META-INF OEBPS

check: kawa.pdf
