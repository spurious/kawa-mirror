## Process this with automake to create Makefile.in

JAVATOP =
SUBDIRS = doc testsuite gnu kawa bin
javadir=$(datadir)/java

JAR = jar

if WITH_SWING
JAR_DEPS_SWING = \
  ./gnu/jemacs/buffer/java-classes.stamp \
  ./gnu/jemacs/buffer/scm-classes.stamp \
  ./gnu/jemacs/lang/java-classes.stamp \
  ./gnu/jemacs/lang/scm-classes.stamp \
  ./gnu/jemacs/lisp/elisp-classes.stamp
endif
if ENABLE_XML
JAR_DEPS_XML = ./gnu/xml/java-classes.stamp \
  ./gnu/xquery/lang/java-classes.stamp \
  ./gnu/xquery/util/java-classes.stamp
endif
if ENABLE_SERVLET
JAR_DEPS_SERVLET = ./gnu/kawa/servlet/java-classes.stamp
endif

JAR_DEPS = $(JAR_DEPS_SWING) $(JAR_DEPS_XML) $(JAR_DEPS_SERVLET) \
  ./gnu/bytecode/java-classes.stamp \
  ./gnu/math/java-classes.stamp \
  ./gnu/lists/java-classes.stamp \
  ./gnu/text/java-classes.stamp \
  ./gnu/mapping/java-classes.stamp \
  ./gnu/expr/java-classes.stamp \
  ./gnu/kawa/util/java-classes.stamp \
  ./gnu/kawa/reflect/java-classes.stamp \
  ./gnu/kawa/functions/java-classes.stamp \
  ./gnu/kawa/lispexpr/java-classes.stamp \
  ./gnu/kawa/slib/scm-classes.stamp \
  ./gnu/ecmascript/java-classes.stamp \
  ./gnu/commonlisp/lang/java-classes.stamp \
  ./kawa/lang/java-classes.stamp \
  ./kawa/standard/java-classes.stamp \
  ./kawa/lib/scm-classes.stamp \
  ./kawa/java-classes.stamp

kawa-@VERSION@.jar: $(JAR_DEPS)
	$(JAR) cmf $(srcdir)/jar-manifest kawa-@VERSION@.jar \
	    `find gnu -name '*.class' -print` `find kawa -name '*.class' -print`

kawa-doc.tar.gz: kawa-doc-$(VERSION).tar.gz

kawa-doc-$(VERSION).tar.gz:  doc/kawa.texi doc/internals.xml doc/kawa-tour.texi
	cd doc; $(MAKE) ../kawa-doc-$(VERSION).tar.gz

dist-kawa-@VERSION@.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure --with-servlet --with-sax2 --with-swing --with-awt \
	  && $(MAKE) \
	  && $(JAR) cmf $$srcdir/jar-manifest \
	    ../kawa-@VERSION@.jar \
	    `find . -name '*.class' -print` \
	  && cd .. && rm -rf tmpdir

jemacs-@JEMACS_VERSION@.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure --with-swing \
	  && $(MAKE) \
	  && $(JAR) cmf $$srcdir/gnu/jemacs/jar-manifest \
	      ../jemacs-@JEMACS_VERSION@.jar \
	      `find . -name '*.class' -print` \
	  && cd .. && rm -rf tmpdir

kawa-@VERSION@-src.zip: kawa-@VERSION@.tar.gz
	mkdir zip-temp
	cd zip-temp ;\
	  tar xzf ../kawa-@VERSION@.tar.gz; \
	  zip -q -r ../kawa-@VERSION@-src.zip kawa-@VERSION@
	rm -rf zip-temp

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa.jar: kawa-@VERSION@.jar
kawa-src.zip: kawa-@VERSION@-src.zip
jemacs.jar: jemacs-@JEMACS_VERSION@.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs makekawa.bat \
  libtool.m4 ltmain.sh \
  build.xml build.properties \
  $(JAVATOP)kawa/Version.java jar-manifest Make-rules

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.commonlisp.lang

JAVADOC = javadoc
JAVADOC_FLAGS = -J-Xms40m -J-Xmx40m
# NOTE:  To update javadoc do:
# make install-javadoc-html
# cd $JAVADOC_DIR
# cvs import -m "Javadoc generated." software/kawa/api javadoc kawa_X_Y_ZZ
# cvs checkout -jjavadoc:yesterday -jjavadoc software/kawa/api
# cd software/kawa/api/
# cvs ci -m 'Updated.' .

MAINTAINERCLEANFILES = makekawa.bat $(JAVATOP)kawa/Version.java
MOSTLYCLEANFILES = tmp *.o kawa1$(exeext)
CLEANFILES = *.jar

install-javadoc-html:
	-mkdir $(JAVADOC_DIR)
	-#rm `find $(JAVADOC_DIR)/ -name CVS -prune -o '(' '!' -type d ')' -print`
	-rm `find $(JAVADOC_DIR)/ -name '*.html'`
	$(JAVADOC) $(JAVADOC_FLAGS) -sourcepath .:$(top_srcdir) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cp $(srcdir)/NEWS $(HTML_HOME)/kawa/News.txt
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"
	cd gnu;  make install-html HTML_HOME="$(HTML_HOME)"

install-data-am:  install-jar
install-jar:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa-@VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f kawa.jar \
	  && $(LN_S) kawa-@VERSION@.jar kawa.jar

uninstall-local: uninstall-jar
uninstall-jar:
	rm -f $(DESTDIR)$(javadir)/kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa.jar

$(JAVATOP)kawa/Version.java: $(srcdir)/$(JAVATOP)kawa/Version.java.in $(srcdir)/configure.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/$(JAVATOP)kawa/Version.java.in >$@

all-recursive:  $(JAVATOP)kawa/Version.java
all: kawa-@VERSION@.jar

makekawa.bat:  $(srcdir)/configure.in
	echo "rem Build Kawa under Windows" >tmp
	echo "rem You may need to change the following definitions:" >>tmp
	echo "set javahome=c:\jdk1.2.1" >>tmp
	echo "set javac=%javahome%\bin\javac" >>tmp
	echo "set java=%javahome%\bin\java" >>tmp
	echo "set jcflags=-g" >>tmp
	for dir in `find . -name Makefile -print \
	    | sed  -e 's|[.]/\(.*\)Makefile|\1|'`; do \
	  test "`cd $(srcdir)/$$dir; echo *.java`" = "*.java"  \
	    || (echo "%javac% %jcflags% $$dir*.java" \
	      | sed -e 's|/|\\|g' >>tmp); \
	done
	echo 'cd kawa\lib' >>tmp
	echo '%java% -classpath ..\.. kawa.repl -d ..\.. -P kawa.lib. -C' $$(cd kawa/lib; make -s list-sources) >>tmp
	sed -e 's/$$/!/' <tmp | tr '!' '\r' >$@
	echo 'cd ..\..\gnu\kawa\slib' >>tmp
	echo '%java% -classpath ..\..\.. kawa.repl -d ..\..\.. -P gnu.kawa.slib -C' $$(cd gnu/kawa/slib; make -s list-sources) >>tmp
	sed -e 's/$$/!/' <tmp | tr '!' '\r' >$@
	echo 'cd ..\..\..\gnu\jemacs\lang' >>tmp
	echo '%java% -classpath ..\..\.. kawa.repl -d ..\..\.. -P gnu.jemacs.lang -C' $$(cd gnu/jemacs/lang; make -s list-sources) >>tmp
	sed -e 's/$$/!/' <tmp | tr '!' '\r' >$@
	echo 'cd ..\..\..\gnu\jemacs\buffer' >>tmp
	echo '%java% -classpath ..\..\.. kawa.repl -d ..\..\.. -P gnu.jemacs.buffer -C' $$(cd gnu/jemacs/buffer; make -s list-sources) >>tmp
	sed -e 's/$$/!/' <tmp | tr '!' '\r' >$@

$(srcdir)/java1-java2-files:
	(cd $(srcdir); \
	find . -name '*.java' -exec grep -l 'BEGIN JAVA[12]' '{}' ';') > $@

select-java1: $(srcdir)/java1-java2-files
	cd $(srcdir); \
	for file in `cat java1-java2-files`; do \
	echo Converting $$file to Java1; \
        sed -e '/BEGIN JAVA2/,/END JAVA2/s|^\([[:space:]]*\)\([^/[:space:]]\)|\1// \2|' \
            -e '/BEGIN JAVA1/,/END JAVA1/s|^\([[:space:]]*\)//[ ]*|\1|' \
	<$$file >$$file.tmp; mv $$file.tmp $$file; done

select-java2: $(srcdir)/java1-java2-files
	cd $(srcdir); \
	for file in `cat java1-java2-files`; do \
	echo Converting $$file to Java2; \
        sed -e '/BEGIN JAVA1/,/END JAVA1/s|^\([[:space:]]*\)\([^/[:space:]]\)|\1// \2|' \
            -e '/BEGIN JAVA2/,/END JAVA2/s|^\([[:space:]]*\)//[ ]*|\1|' \
	<$$file >$$file.tmp; mv $$file.tmp $$file; done

distall:  distcheck dist-kawa-@VERSION@.jar kawa-doc-$(VERSION).tar.gz \
  kawa-@VERSION@-src.zip jemacs.jar
