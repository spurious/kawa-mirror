## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = gnu kawa doc testsuite
javadir=$(datadir)/java

# Build a .zip archive from the compiled classes.

kawa-@VERSION@-compiled.zip:
	zip -q -r -n .class $@ kawa gnu -i '*.class'

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs


HTML_HOME = $$HOME/public_html
HTML_DOC_DIR = $(HTML_HOME)/kawa/api
JAVADOC_PACKAGES = gnu.bytecode gnu.math kawa kawa.lang kawa.standard

install-javadoc-html:
	-mkdir $(HTML_HOME)/kawa
	-mkdir $(HTML_HOME)/kawa/api
	cd $(top_srcdir) && javadoc -d $(HTML_DOC_DIR) $(JAVADOC_PACKAGES)
	-cd $(HTML_DOC_DIR) && ln -s $(HTML_HOME)/javadoc-images images

install-html: install-javadoc-html
	cp $(srcdir)/NEWS $(HTML_HOME)/kawa/News.txt
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"
	cd gnu/bytecode;  make install-html HTML_HOME="$(HTML_HOME)"
	cd gnu/math;  make install-html HTML_HOME="$(HTML_HOME)"
