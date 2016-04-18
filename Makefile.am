# Process this with automake to create Makefile.in

FIXED_SUBDIRS = doc testsuite gnu/bytecode gnu/math \
  gnu \
  kawa gnu/kawa/slib gnu/kawa/servlet \
  gnu/commonlisp/lang gnu/commonlisp/lisp gnu/commonlisp/testsuite \
  gnu/q2/lang gnu/q2/testsuite
ANDROID_SUBDIRS = gnu/kawa/android
BRL_SUBDIRS = gnu/brl gnu/kawa/brl
JAVAFX_SUBDIRS = gnu/kawa/javafx
JEMACS_SUBDIRS = gnu/jemacs/lang gnu/jemacs/buffer gnu/jemacs/lisp \
  gnu/jemacs/testsuite gnu/jemacs/swing gnu/jemacs/swt
XQUERY_SUBDIRS = gnu/xquery/util gnu/xquery/lang gnu/xquery/testsuite

SUBDIRS = $(FIXED_SUBDIRS)
if ENABLE_JEMACS
  SUBDIRS += $(JEMACS_SUBDIRS)
endif
if ENABLE_XML
if ENABLE_BRL
SUBDIRS += $(BRL_SUBDIRS)
endif
if ENABLE_XQUERY
SUBDIRS += $(XQUERY_SUBDIRS)
endif
endif
if WITH_ANDROID
SUBDIRS += $(ANDROID_SUBDIRS)
endif
if WITH_JAVAFX
SUBDIRS += $(JAVAFX_SUBDIRS)
endif
if ENABLE_KAWA_FRONTEND
SUBDIRS += bin
endif

DIST_SUBDIRS = $(FIXED_SUBDIRS) \
  $(ANDROID_SUBDIRS) \
  $(BRL_SUBDIRS) \
  $(JAVAFX_SUBDIRS) \
  $(JEMACS_SUBDIRS) \
  $(XQUERY_SUBDIRS)

javadir=$(datadir)/java

SED = sed
CHMOD = chmod
JAR = jar
FIND = @FIND@
KAWAJAR = kawa-@VERSION@.jar
KAWALIB = $(datadir)/kawa/lib/$(KAWAJAR)

META_ScriptEngineFactory = META-INF/services/javax.script.ScriptEngineFactory
if WITH_JAVAX_SCRIPT
META_INF_FILES = $(META_ScriptEngineFactory)
endif

classes.list: ./kawa-jar.stamp
	$(FIND) gnu kawa -name '*.class' -print >classes.list

classes-rt.list: ./kawa-jar.stamp $(srcdir)/classes-to-skip-for-runtime 
	$(FIND) gnu kawa -name '*.class' -print | grep -v -f $(srcdir)/classes-to-skip-for-runtime >classes-rt.list

lib/kawa-@VERSION@.jar: ./kawa-jar.stamp $(META_INF_FILES) classes.list
	mkdir -p lib
	$(JAR) cmf $(srcdir)/jar-manifest $@ $(META_INF_FILES) @classes.list

lib/kawart-@VERSION@.jar: ./kawa-jar.stamp classes-rt.list
	mkdir -p lib
	$(JAR) cf $@ @classes-rt.list

$(META_ScriptEngineFactory): Makefile
	echo 'kawa.standard.SchemeScriptEngineFactory #Scheme' >tmp.ScriptEngineFactory
if ENABLE_XQUERY
	echo 'gnu.xquery.lang.XQueryScriptEngineFactory #XQuery' >>tmp.ScriptEngineFactory
endif
	mkdir -p META-INF/services
	mv tmp.ScriptEngineFactory $(META_ScriptEngineFactory)

kawa-doc.tar.gz: kawa-doc-$(VERSION).tar.gz

kawa-doc-$(VERSION).tar.gz:  doc/kawa.texi
	cd doc; $(MAKE) ../kawa-doc-$(VERSION).tar.gz

dist-kawa-$(VERSION).jar: dist
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir -p tmpdir tmpdir/lib \
	  && cd tmpdir \
	  && tar xzf ../$(distdir).tar.gz \
	  && rm -f ./patch-source-list \
	  && ./kawa-$(VERSION)/configure --with-java-source=7 \
	    --with-jline2=@WITH_JLINE2_ARG@ \
	    --with-servlet=@WITH_SERVLET_ARG@ --with-javafx=@WITH_JAVAFX_ARG@ \
	  && CLASSPATH=@conf_classpath@.$(CLASSPATH_SEPARATOR)$$CLASSPATH $(MAKE) \
	  && mv lib/kawa-$(VERSION).jar ../dist-kawa-$(VERSION).jar \
	  && cd .. && rm -rf tmpdir

kawa.zip: kawa-$(VERSION).zip
kawa-$(VERSION).zip: dist-kawa-$(VERSION).jar
	mkdir -p kawa-$(VERSION) kawa-$(VERSION)/lib  kawa-$(VERSION)/bin
	cd kawa-$(VERSION)/lib; \
	  $(LN_S) ../../dist-kawa-$(VERSION).jar kawa-$(VERSION).jar; \
	  $(LN_S) @WITH_JLINE2_ARG@ jline.jar
	cd kawa-$(VERSION)/bin; \
	  $(LN_S) ../../bin/kawa.sh kawa
	zip $@ kawa-$(VERSION)/bin/kawa kawa-$(VERSION)/lib/kawa-$(VERSION).jar kawa-$(VERSION)/lib/jline.jar

lib/jemacs-$(JEMACS_VERSION).jar:
	srcdir=`(cd $(srcdir); /bin/pwd)` \
	  && rm -rf tmpdir \
	  && mkdir tmpdir \
	  && cd tmpdir \
	  && $$srcdir/configure --with-swing \
	  && $(MAKE) \
	  && $(JAR) cmf $$srcdir/gnu/jemacs/jar-manifest \
	      ../lib/jemacs-$(JEMACS_VERSION).jar \
	      `$(FIND) . -name '*.class' -print` \
	  && cd .. && rm -rf tmpdir

lib/bytecode-$(BYTECODE_VERSION).jar: ./gnu/bytecode/java-classes.stamp
	$(JAR) cmf $(srcdir)/gnu/bytecode/jar-manifest \
	  lib/bytecode-$(BYTECODE_VERSION).jar \
	  `$(FIND) gnu/bytecode -name '*.class' -print` 

kawa-@VERSION@-src.zip: kawa-@VERSION@.tar.gz
	mkdir zip-temp
	cd zip-temp ;\
	  tar xzf ../kawa-@VERSION@.tar.gz; \
	  zip -q -r ../kawa-@VERSION@-src.zip kawa-@VERSION@
	rm -rf zip-temp

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa.jar: lib/kawa-@VERSION@.jar
kawa-src.zip: kawa-@VERSION@-src.zip
jemacs.jar: lib/jemacs-@JEMACS_VERSION@.jar
bytecode.jar: lib/bytecode-@BYTECODE_VERSION@.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs \
  patch-source-list selected-java-source \
  build.xml build.properties \
  gnu/jemacs/jar-manifest gnu/jemacs/status.html \
  kawa/Version.java jar-manifest Make-rules classes-to-skip-for-runtime \
  bin/configure.ac bin/configure bin/Makefile.am bin/Makefile.in \
  bin/ChangeLog bin/kawa.bat.in bin/kawa.c bin/kawapath.c bin/cgi-servlet.c \
  bin/kawa.sh.in bin/qexo.in

MOSTLYCLEANFILES = tmp* *.o bin/*.o *.stamp classes.list classes-rt.list
CLEANFILES = *.jar lib/*.jar *.zip bin/qexo kawa/Version.java bin/kawa.bat $(META_ScriptEngineFactory)
DISTCLEANFILES = selected-java-source bin/kawa.sh
MAINTAINERCLEANFILES = \
  $(GENERATED_SIMPLEVECTOR_FILES) $(srcdir)/patch-source-list

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/api
JAVADOC_PACKAGES = \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text gnu.lists\
  gnu.xml gnu.kawa.reflect gnu.kawa.util gnu.kawa.lispexpr gnu.kawa.functions \
  gnu.kawa.servlet gnu.kawa.ant kawa kawa.lang kawa.standard kawa.lib \
  gnu.kawa.xml gnu.xquery.lang gnu.xquery.util gnu.brl gnu.kawa.brl \
  gnu.kawa.xslt gnu.kawa.sax gnu.commonlisp.lang gnu.q2.lang \
  gnu.kawa.models gnu.kawa.swingviews gnu.kawa.swtviews gnu.kawa.echo2 \
  gnu.jemacs.lang gnu.jemacs.buffer gnu.jemacs.swing gnu.jemacs.swt \
  gnu.kawa.android gnu.kawa.javafx

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
	  && $(FIND) . -name CVS -prune -o -type f -exec echo rm '{}' ';'
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
	-#rm `$(FIND) $(JAVADOC_DIR)/ -name CVS -prune -o '(' '!' -type d ')' -print`
	-rm `$(FIND) $(JAVADOC_DIR)/ -name '*.html'`
	CLASSPATH=$(JAVADOC_EXTRA_PATH)$(CLASSPATH_SEPARATOR)@conf_classpath@$$CLASSPATH \
	$(JAVADOC) $(JAVADOC_FLAGS) -sourcepath .:$(top_srcdir) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"

install-binSCRIPTS: $(bin_SCRIPTS)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	$(INSTALL_SCRIPT) bin/qexo $(DESTDIR)$(bindir)/qexo
if ! ENABLE_KAWA_FRONTEND
	cd $(DESTDIR)$(bindir) && \
	  $(LN_S) $(datadir)/kawa/bin/kawa kawa
endif

install-data-am:  install-jar
	$(mkinstalldirs) $(DESTDIR)$(datadir)/kawa \
	  $(DESTDIR)$(datadir)/kawa/bin $(DESTDIR)$(datadir)/kawa/lib
	$(INSTALL_SCRIPT) bin/kawa.sh $(DESTDIR)$(datadir)/kawa/bin/kawa

install-jar:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(datadir)/kawa \
	  $(DESTDIR)$(datadir)/kawa/bin $(DESTDIR)$(datadir)/kawa/lib
	$(INSTALL_DATA) lib/kawa-@VERSION@.jar \
	  $(DESTDIR)$(datadir)/kawa/lib/$(KAWAJAR)
	cd $(DESTDIR)$(datadir)/kawa/lib && rm -f kawa.jar \
	  && $(LN_S) $(KAWAJAR) kawa.jar
if WITH_JLINE2
	cd $(DESTDIR)$(datadir)/kawa/lib && \
	  $(LN_S) @WITH_JLINE2_ARG@ jline.jar
endif

install-bytecode-jar: lib/bytecode-@BYTECODE_VERSION@.jar
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(javadir)
	$(INSTALL_DATA) lib/bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar
	cd $(DESTDIR)$(javadir) && rm -f bytecode.jar \
	  && $(LN_S) bytecode-@BYTECODE_VERSION@.jar bytecode.jar

uninstall-local: uninstall-jar uninstall-exec-bindir uninstall-data
uninstall-jar:
	rm -f $(DESTDIR)$(datadir)/kawa/lib/$(KAWAJAR) \
	  $(DESTDIR)$(datadir)/kawa/lib/kawa.jar

uninstall-data:
	rm -f $(DESTDIR)$(datadir)/kawa/bin/kawa

uninstall-exec-bindir:
	rm -f $(DESTDIR)$(bindir)/kawa $(DESTDIR)$(bindir)/qexo

uninstall-bytecode-jar:
	rm -f $(DESTDIR)$(javadir)/bytecode-@BYTECODE_VERSION@.jar \
	  $(DESTDIR)$(javadir)/bytecode.jar

kawa/Version.java: $(srcdir)/kawa/Version.java.in $(srcdir)/configure.ac
	test -f $(srcdir)/.svn/entries \
	&& SVN_REVISION=`cd $(srcdir) && svnversion 2>/dev/null` \
	&& test -n "$$SVN_REVISION" \
	&& SVN_REVISION=" (revision $${SVN_REVISION})"; \
	sed -e 's|VERSION|@VERSION@'"$$SVN_REVISION|" <$(srcdir)/kawa/Version.java.in >$@

all-recursive:  kawa/Version.java selected-java-source $(GENERATED_SIMPLEVECTOR_FILES)
all: lib/kawa-@VERSION@.jar lib/kawart-@VERSION@.jar $(JLINE_LINK)

if WITH_JLINE2
JLINE_LINK = lib/jline.jar

lib/jline.jar:
	mkdir -p lib
	$(LN_S) @WITH_JLINE2_ARG@ lib/jline.jar
endif

JAVA_SOURCE = @JAVA_SOURCE@

if WITH_HTTPSERVER
PREPROCESS_HTTPSERVER = +use:com.sun.net.httpserver
else
PREPROCESS_HTTPSERVER = -use:com.sun.net.httpserver
endif
if WITH_JFREESVG
PREPROCESS_JFREESVG = +with:JFreeSVG
else
PREPROCESS_JFREESVG = -with:JFreeSVG
endif
if ENABLE_XML
PREPROCESS_XML = +enable:XML
else
PREPROCESS_XML = -enable:XML
endif

$(srcdir)/patch-source-list: $(top_srcdir)/configure
	(cd $(srcdir); \
	$(FIND) . -name '*.java' -exec grep -lE ' #endif ' '{}' ';') \
	  | sed -e 's|./||' | sort > $@

gnu/kawa/util/PreProcess.class: $(srcdir)/gnu/kawa/util/PreProcess.java
	$(JAVAC) -d . $(srcdir)/gnu/kawa/util/PreProcess.java

JAVA_PREPROCESS_OPTIONS = @JAVA_SOURCE_SELECTOR@ $(PREPROCESS_HTTPSERVER) $(PREPROCESS_XML) $(PREPROCESS_JFREESVG)

selected-java-source: Makefile $(srcdir)/patch-source-list
	if test ! -e selected-java-source -o \
	   "$(JAVA_PREPROCESS_OPTIONS)" != "`cat selected-java-source`"; \
	   then $(MAKE) do-preprocess; \
	fi
	echo "$(JAVA_PREPROCESS_OPTIONS)" >selected-java-source

do-preprocess: gnu/kawa/util/PreProcess.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$$CLASSPATH" $(JAVA) gnu.kawa.util.PreProcess \
	  $(JAVA_PREPROCESS_OPTIONS) \
	  `sed -e 's|\([^ ]*\)|$(srcdir)/\1|' < $(srcdir)/patch-source-list`

GENERATED_SIMPLEVECTOR_FILES =\
  $(srcdir)/gnu/lists/FVector.java \
  $(srcdir)/gnu/lists/BitVector.java \
  $(srcdir)/gnu/lists/ByteVector.java \
  $(srcdir)/gnu/lists/ShortVector.java \
  $(srcdir)/gnu/lists/IntVector.java \
  $(srcdir)/gnu/lists/LongVector.java \
  $(srcdir)/gnu/lists/F32Vector.java \
  $(srcdir)/gnu/lists/F64Vector.java \
  $(srcdir)/gnu/lists/S8Vector.java \
  $(srcdir)/gnu/lists/S16Vector.java \
  $(srcdir)/gnu/lists/S32Vector.java \
  $(srcdir)/gnu/lists/S64Vector.java \
  $(srcdir)/gnu/lists/U8Vector.java \
  $(srcdir)/gnu/lists/U16Vector.java \
  $(srcdir)/gnu/lists/U32Vector.java \
  $(srcdir)/gnu/lists/U64Vector.java

.PHONY: make-primvector-source
make-primvector-source: $(srcdir)/gnu/lists/PrimVector.template gnu/kawa/util/PreProcess.class
	CLASSPATH=".$(CLASSPATH_SEPARATOR)$$CLASSPATH" $(JAVA) gnu.kawa.util.PreProcess \
	  '%$(TAG)' '%UniformVector' -o $(OUT) $<

$(srcdir)/gnu/lists/FVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=OBJECT
$(srcdir)/gnu/lists/BitVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=BIT
$(srcdir)/gnu/lists/ByteVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=BYTE
$(srcdir)/gnu/lists/ShortVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=SHORT
$(srcdir)/gnu/lists/IntVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=INT
$(srcdir)/gnu/lists/LongVector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=LONG
$(srcdir)/gnu/lists/F32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=F32
$(srcdir)/gnu/lists/F64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=F64
$(srcdir)/gnu/lists/S8Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S8
$(srcdir)/gnu/lists/S16Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S16
$(srcdir)/gnu/lists/S32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S32
$(srcdir)/gnu/lists/S64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=S64
$(srcdir)/gnu/lists/U8Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U8
$(srcdir)/gnu/lists/U16Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U16
$(srcdir)/gnu/lists/U32Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U32
$(srcdir)/gnu/lists/U64Vector.java: $(srcdir)/gnu/lists/PrimVector.template
	$(MAKE) make-primvector-source OUT=$@ TAG=U64

clean-generated-simplevector-files:
	-rm -f $(GENERATED_SIMPLEVECTOR_FILES)

if ON_WINDOWS
# kawa.sh is still needed for 'make check', even on Windows.
KAWASCRIPTS_KAWA = bin/kawa.bat bin/kawa.sh
else
KAWASCRIPTS_KAWA = bin/kawa.sh
endif
noinst_SCRIPTS = bin/kawa.sh
bin_SCRIPTS = bin/qexo $(KAWASCRIPTS_KAWA)

bin/kawa.bat: $(srcdir)/bin/kawa.bat.in
	sed -e 's|%DEFAULT_KAWA_VERSION%|$(VERSION)|' \
	    -e 's|%DEFAULT_EXTRA_PATH%|'`echo '$(conf_classpath)'| sed -e 's|[\]|\\\\|g' -e 's|\&|\\&|g'`'|' \
	  <$(srcdir)/bin/kawa.bat.in > bin/kawa.bat

distall:  distcheck dist-kawa-@VERSION@.jar kawa-doc-$(VERSION).tar.gz
# Consider also:
# kawa-@VERSION@-src.zip jemacs-@JEMACS_VERSION@.jar bytecode-@BYTECODE_VERSION@.jar
