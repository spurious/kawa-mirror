## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus
SUBDIRS = gnu kawa doc testsuite
javadir=$(datadir)/java

# Build a .zip archive from the compiled classes.

kawa-@VERSION@-compiled.zip:
	zip -q -r -n .class $@ kawa gnu -i '*.class'

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $(HTML_HOME)/kawa/api
JAVADOC_PACKAGES = gnu.bytecode gnu.mapping gnu.expr gnu.math gnu.text \
  kawa.lang kawa.standard kawa/*.java \
  gnu/ecmascript/Reserved.java gnu/ecmascript/BinaryOp.java \
  gnu/ecmascript/Convert.java gnu/ecmascript/Lexer.java \
  gnu/ecmascript/Parser.java gnu/ecmascript/Prompter.java
JAVADOC = javadoc

install-javadoc-html:
	rm -rf $(JAVADOC_DIR)
	-mkdir $(JAVADOC_DIR)
	cd $(top_srcdir) && $(JAVADOC) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
#	-cd $(HTML_DOC_DIR) && ln -s $(HTML_HOME)/javadoc-images images

install-html: install-javadoc-html
	cp $(srcdir)/NEWS $(HTML_HOME)/kawa/News.txt
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"
	cd gnu;  make install-html HTML_HOME="$(HTML_HOME)"
