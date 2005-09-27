## Process this with automake to create Makefile.in

SUBDIRS = doc testsuite gnu/bytecode gnu/math gnu/lists gnu/text \
  gnu/mapping $(XML_SUBDIRS) gnu/expr \
  gnu/kawa/util gnu/kawa/reflect gnu/kawa/functions gnu/kawa/lispexpr \
  $(SERVLET_SUBDIRS) $(SAX_SUBDIR) gnu/kawa/ant \
  gnu/kawa/models $(SWINGVIEWS_SUBDIRS) \
  kawa gnu/kawa/slib gnu/commonlisp gnu/q2 gnu/ecmascript \
  $(JEMACS_SUBDIRS) $(XQUERY_SUBDIR) $(BRL_SUBDIRS) bin
if ENABLE_XML
XML_SUBDIRS = gnu/xml gnu/kawa/xml gnu/kawa/xslt
BRL_SUBDIRS = gnu/brl gnu/kawa/brl
XQUERY_SUBDIR = gnu/xquery
if WITH_SAX2
SAX_SUBDIR = gnu/kawa/sax
endif
endif
if ENABLE_SERVLET
SERVLET_SUBDIRS = gnu/kawa/servlet
endif
if WITH_SWING
SWINGVIEWS_SUBDIRS = gnu/kawa/swingviews
JEMACS_SWING = gnu/jemacs/swing
endif
if WITH_SWT
JEMACS_SWT = gnu/jemacs/swt
endif
JEMACS_SUBDIRS = gnu/jemacs/lang gnu/jemacs/buffer gnu/jemacs/lisp \
  gnu/jemacs/testsuite $(JEMACS_SWING) $(JEMACS_SWT)
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
  ./gnu/kawa/xml/java-classes.stamp \
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

dist-hook:
	rm -f $(distdir)/patch-source-list
	$(MAKE) srcdir=`pwd`/$(distdir) select-java4

dist-kawa-@VERSION@.jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && rm $$srcdir/patch-source-list
	  && $$srcdir/configure --with-servlet --with-sax2 --with-swing --with-awt --with-java-source=2\
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

bytecode-@BYTECODE_VERSION@.jar: ./gnu/bytecode/java-classes.stamp
	$(JAR) cmf $(srcdir)/gnu/bytecode/jar-manifest \
	  bytecode-@BYTECODE_VERSION@.jar \
	  `find gnu/bytecode -name '*.class' -print` 

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
bytecode.jar: bytecode-@BYTECODE_VERSION@.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs \
  ltmain.sh patch-source-list selected-java-source \
  build.xml build.properties \
  gnu/jemacs/jar-manifest gnu/jemacs/status.html \
  kawa/Version.java jar-manifest Make-rules

MAINTAINERCLEANFILES = makekawa.bat kawa/Version.java
MOSTLYCLEANFILES = tmp *.o kawa1$(exeext)
CLEANFILES = *.jar

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard kawa.lib \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax gnu.commonlisp.lang \
  gnu.kawa.models gnu.kawa.swingviews gnu.q2.lang \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.jemacs.swt

JAVADOC = javadoc
#JAVADOC_FLAGS = -J-Xmx60m

# NOTE:  To update javadoc:
#   make install-javadoc-html
# When satisfied:
#   make javadoc-cvs.log
# When satisfied:
#   cd ~/Kawa/api && cvs ci -m 'Re-run javadoc.'
javadoc-cvs.log:
	cd $(JAVADOC_DIR) \
	  && find . -name CVS -prune -o -type f -exec echo rm '{}' ';'
	$(MAKE) install-javadoc-html >&LOG.JAVADOC
	(cd $(JAVADOC_DIR) && cvs -n update -d .>&/tmp/cvs-changes \
	  && cvs add `grep '^[?] ' /tmp/cvs-changes |sed -e 's|[?] ||'` \
	  && cvs remove `grep '^[U] ' /tmp/cvs-changes |sed -e 's|[U] ||'` \
	  && cvs update -d .) >cvs-update.log
	echo ExamineCheck ./cvs-update.log
	echo then: "(cd $(JAVADOC_DIR) && cvs ci -m 'Re-run javadoc.')".

ECLIPSE_PLUGINS_PATH = /opt/eclipse/plugins
JAVADOC_ANT_JARS = /opt/ant/lib/ant.jar
JAVADOC_ECLIPSE_JARS = $(ECLIPSE_PLUGINS_PATH)/org.eclipse.jface_3.1.0/jface.jar:$(ECLIPSE_PLUGINS_PATH)/org.eclipse.swt.gtk_3.1.0/ws/gtk/swt.jar
JAVADOC_EXTRA_PATH = $(JAVADOC_ECLIPSE_JARS):$(JAVADOC_ANT_JARS)

install-javadoc-html:
	-mkdir $(JAVADOC_DIR)
	-#rm `find $(JAVADOC_DIR)/ -name CVS -prune -o '(' '!' -type d ')' -print`
	-rm `find $(JAVADOC_DIR)/ -name '*.html'`
	CLASSPATH=$(JAVADOC_EXTRA_PATH):@conf_classpath@$$CLASSPATH \
	$(JAVADOC) $(JAVADOC_FLAGS) -sourcepath .:$(top_srcdir) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"

install-data-am:  install-jar
install-jar:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa-@VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f kawa.jar \
	  && $(LN_S) kawa-@VERSION@.jar kawa.jar

install-bytecode-jar: bytecode-@BYTECODE_VERSION@.jar
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f bytecode.jar \
	  && $(LN_S) bytecode-@BYTECODE_VERSION@.jar bytecode.jar

uninstall-local: uninstall-jar
uninstall-jar:
	rm -f $(DESTDIR)$(javadir)/kawa-@VERSION@.jar \
	  $(DESTDIR)$(javadir)/kawa.jar

uninstall-bytecode-jar:
	rm -f $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode.jar

kawa/Version.java: $(srcdir)/kawa/Version.java.in $(srcdir)/configure.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/kawa/Version.java.in >$@

all-recursive:  kawa/Version.java selected-java-source
all: kawa-@VERSION@.jar

selected-java-source: Makefile
	@java_source=`cat $(srcdir)/selected-java-source 2>/dev/null || echo none`; \
	if test "$$java_source" = "@JAVA_SOURCE@"; \
	then echo current java-source is ok; \
	elif test "@JAVA_SOURCE@" = 1; \
	then $(MAKE) select-java1; \
	elif test "@JAVA_SOURCE@" = 2; \
	then $(MAKE) select-java2; \
	elif test "@JAVA_SOURCE@" = 4; \
	then $(MAKE) select-java4; \
	elif test "@JAVA_SOURCE@" = 4x; \
	then $(MAKE) select-java4x; \
	elif test "@JAVA_SOURCE@" = 5; \
	then $(MAKE) select-java5; \
	else echo Unrecognized JAVA_SOURCE: @JAVA_SOURCE@; exit 1; \
	fi

$(srcdir)/patch-source-list:
	(cd $(srcdir); \
	find . -name '*.java' -exec grep -lE ' #ifdef ' '{}' ';') \
	  | sed -e 's|./||' | sort > $@

COMMENT_IN = s|^\([[:space:]]*\)//[ ]*|\1|
COMMENT_OUT = s|^\([[:space:]]*\)\([^/[:space:]]\)|\1// \2|

gnu/kawa/util/PreProcess.class: $(srcdir)/gnu/kawa/util/PreProcess.java
	$(JAVAC) -d . $(srcdir)/gnu/kawa/util/PreProcess.java

JAVA4_FEATURES = +use:java.util.IdentityHashMap +use:java.lang.CharSequence +use:java.lang.Throwable.getCause +use:java.net.URI
NO_JAVA4_FEATURES = -use:java.util.IdentityHashMap -use:java.lang.CharSequence -use:java.lang.Throwable.getCause -use:java.net.URI -use:org.w3c.dom.Node -JAXP-1.3 -JAVA5

# JDK 1.1
select-java1: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(JAVA) gnu.kawa.util.PreProcess -JAVA2 $(NO_JAVA4_FEATURES) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=1

# JDK 1.2+, GCJ-3.x
select-java2: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(JAVA) gnu.kawa.util.PreProcess +JAVA2 $(NO_JAVA4_FEATURES) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=2

# JDK 1.4, GCJ-4.x, without JAXP (default - matches CVS)
select-java4: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(JAVA) gnu.kawa.util.PreProcess +JAVA2 -JAVA5 $(JAVA4_FEATURES) -use:org.w3c.dom.Node -JAXP-1.3 \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=4

# JDK 1.4, GCJ-4.x with JAXP 1.3
select-java4x: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(JAVA) gnu.kawa.util.PreProcess +JAVA2 -JAVA5 $(JAVA4_FEATURES) +use:org.w3c.dom.Node +JAXP-1.3 \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=4x

# JDK 1.5
select-java5: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	$(JAVA) gnu.kawa.util.PreProcess +JAVA2 +JAVA5 $(JAVA4_FEATURES) +use:org.w3c.dom.Node +JAXP-1.3 \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=5

set-selected-java-source:
	test "@JAVA_SOURCE@" = `cat $(srcdir)/selected-java-source` \
	  || echo "@JAVA_SOURCE@" >$(srcdir)/selected-java-source

distall:  distcheck dist-kawa-@VERSION@.jar kawa-doc-$(VERSION).tar.gz \
  kawa-@VERSION@-src.zip jemacs-@JEMACS_VERSION@.jar bytecode-@BYTECODE_VERSION@.jar
