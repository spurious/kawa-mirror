## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus
JAVATOP =
SUBDIRS = doc testsuite gnu kawa bin
javadir=$(datadir)/java

JAR = jar

# Build a .zip archive from the compiled classes.

kawa-@VERSION@-compiled.zip:
	zip -q -r -n .class $@ kawa gnu -i '*.class'

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
	    `find . -name '*.class' -print`

dist-kawa-@VERSION@.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure \
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

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa.jar: kawa-@VERSION@.jar
jemacs.jar: jemacs-@JEMACS_VERSION@.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs makekawa.bat \
  libtool.m4 ltcf-c.sh ltcf-cxx.sh ltcf-gcj.sh ltconfig ltmain.sh \
  build.xml build.properties \
  $(JAVATOP)kawa/Version.java jar-manifest Make-rules

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  kawa kawa.lang kawa.standard \
  gnu.jemacs.lang gnu.jemacs.buffer
JAVADOC = javadoc
JAVADOC_FLAGS = -J-Xms40m -J-Xmx40m

MAINTAINERCLEANFILES = makekawa.bat
MOSTLYCLEANFILES = tmp *.o kawa1$(exeext)
CLEANFILES = *.jar

install-javadoc-html:
	rm -rf $(JAVADOC_DIR)/*
	-mkdir $(JAVADOC_DIR)
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


$(JAVATOP)kawa/Version.java: $(srcdir)/$(JAVATOP)kawa/Version.java.in $(srcdir)/configure.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/$(JAVATOP)kawa/Version.java.in >$@

all-recursive:  $(JAVATOP)kawa/Version.java
all-redirect: kawa-@VERSION@.jar

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

distall:  distcheck dist-kawa-@VERSION@.jar jemacs.jar
