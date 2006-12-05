# Process this with automake to create Makefile.in

SUBDIRS = doc testsuite gnu/bytecode gnu/math gnu/lists gnu/text \
  gnu/mapping $(XML_SUBDIRS) gnu/expr \
  gnu/kawa/util gnu/kawa/reflect gnu/kawa/functions gnu/kawa/lispexpr \
  gnu/kawa/models gnu/kawa/swingviews gnu/kawa/swtviews \
  kawa gnu/kawa/slib gnu/commonlisp gnu/q2 gnu/ecmascript \
  gnu/kawa/servlet $(SAX_SUBDIR) gnu/kawa/ant \
  gnu/jemacs/lang gnu/jemacs/buffer gnu/jemacs/lisp \
  gnu/jemacs/testsuite gnu/jemacs/swing gnu/jemacs/swt \
  $(XQUERY_SUBDIR) $(BRL_SUBDIRS) bin
if ENABLE_XML
XML_SUBDIRS = gnu/xml gnu/kawa/xml gnu/kawa/xslt
BRL_SUBDIRS = gnu/brl gnu/kawa/brl
XQUERY_SUBDIR = gnu/xquery
if WITH_SAX2
SAX_SUBDIR = gnu/kawa/sax
endif
endif
javadir=$(datadir)/java

JAR = jar

kawa-@VERSION@.jar: ./kawa-jar.stamp
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

MOSTLYCLEANFILES = tmp *.o kawa1$(exeext) *.stamp
CLEANFILES = *.jar kawa/Version.java

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard kawa.lib \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax gnu.commonlisp.lang \
  gnu.kawa.models gnu.kawa.swingviews gnu.kawa.swtviews gnu.q2.lang \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.jemacs.swt

JAVADOC = javadoc
JAVADOC_FLAGS = -use
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
	  && (grep '^[?] ' /tmp/cvs-changes |sed -e 's|[?] ||') >/tmp/cvs-adds \
	  && (test '!' -s /tmp/cvs-adds || cvs add `cat /tmp/cvs-adds`) \
	  && (grep '^[U] ' /tmp/cvs-changes |sed -e 's|[U] ||') >/tmp/cvs-rems\
	  && (test '!' -s /tmp/cvs-rems || cvs remove `cat /tmp/cvs-rems`) \
	  && cvs update -d .) >cvs-update.log
	echo ExamineCheck ./cvs-update.log
	echo then: "(cd $(JAVADOC_DIR) && cvs ci -m 'Re-run javadoc.')".

ECLIPSE_PLUGINS_PATH = /opt/eclipse/plugins
JAVADOC_ANT_JARS = $(ECLIPSE_PLUGINS_PATH)/org.apache.ant_1.6.5/lib/ant.jar
JAVADOC_ECLIPSE_JARS = $(ECLIPSE_PLUGINS_PATH)/org.eclipse.jface_3.1.0.jar:$(ECLIPSE_PLUGINS_PATH)/org.eclipse.swt.gtk.linux.x86_3.1.0.jar
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
	test -f .svn/entries && SVN_REVISION=" (revision `sed -n 's|.* revision="\(.*\)".*|\1|p' .svn/entries|head -n 1` `sed -n 's|.* committed-date="\(.*\)T.*|\1|p' .svn/entries|head -n 1`)" ; \
	sed -e 's|VERSION|@VERSION@'"$$SVN_REVISION|" <$(srcdir)/kawa/Version.java.in >$@

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
	find . -name '*.java' -exec grep -lE ' #endif ' '{}' ';') \
	  | sed -e 's|./||' | sort > $@

COMMENT_IN = s|^\([[:space:]]*\)//[ ]*|\1|
COMMENT_OUT = s|^\([[:space:]]*\)\([^/[:space:]]\)|\1// \2|

gnu/kawa/util/PreProcess.class: $(srcdir)/gnu/kawa/util/PreProcess.java
	$(JAVAC) -d . $(srcdir)/gnu/kawa/util/PreProcess.java

JAVA4_FEATURES = +JAVA2 +use:java.util.IdentityHashMap +use:java.lang.CharSequence +use:java.lang.Throwable.getCause +use:java.net.URI +use:java.util.regex
NO_JAVA4_FEATURES = -use:java.util.IdentityHashMap -use:java.lang.CharSequence -use:java.lang.Throwable.getCause -use:java.net.URI -use:java.util.regex -use:org.w3c.dom.Node -JAXP-1.3 -JAVA5 -JAVA6 -JAXP-QName -use:java.text.Normalizer
JAVA5_FEATURES = +JAVA5 $(JAVA4_FEATURES) +use:org.w3c.dom.Node +JAXP-1.3 -JAXP-QName

# JDK 1.1
select-java1: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess -JAVA2 $(NO_JAVA4_FEATURES) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=1

# JDK 1.2+, GCJ-3.x
select-java2: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess +JAVA2 $(NO_JAVA4_FEATURES) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=2

# JDK 1.4, GCJ-4.x, without JAXP (default - matches CVS)
select-java4: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess -JAVA5 $(JAVA4_FEATURES) -use:org.w3c.dom.Node -JAXP-1.3 -JAXP-QName -JAVA6 -use:java.text.Normalizer \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=4

# JDK 1.4, GCJ-4.x with JAXP 1.3
select-java4x: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess -JAVA5 $(JAVA4_FEATURES) +use:org.w3c.dom.Node +JAXP-1.3 -JAXP-QName -JAVA6 -use:java.text.Normalizer \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=4x

# JDK 1.5
select-java5: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess $(JAVA5_FEATURES) -JAVA6 -use:java.text.Normalizer \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=5

# JDK 1.6
select-java6: $(srcdir)/patch-source-list gnu/kawa/util/PreProcess.class
	CLASSPATH=.:$$CLASSPATH $(JAVA) gnu.kawa.util.PreProcess $(JAVA5_FEATURES) +JAVA6 +use:java.text.Normalizer \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`
	$(MAKE) set-selected-java-source JAVA_SOURCE=6

set-selected-java-source:
	test "@JAVA_SOURCE@" = `cat $(srcdir)/selected-java-source` \
	  || echo "@JAVA_SOURCE@" >$(srcdir)/selected-java-source

distall:  distcheck dist-kawa-@VERSION@.jar kawa-doc-$(VERSION).tar.gz \
  kawa-@VERSION@-src.zip jemacs-@JEMACS_VERSION@.jar bytecode-@BYTECODE_VERSION@.jar
