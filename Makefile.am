## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus
JAVATOP =
SUBDIRS = doc testsuite gnu kawa
javadir=$(datadir)/java

JAR = jar

# Build a .zip archive from the compiled classes.

kawa-@VERSION@-compiled.zip:
	zip -q -r -n .class $@ kawa gnu -i '*.class'

kawa-@VERSION@-compiled.jar:
	$(JAR) cmf $(srcdir)/jar-manifest $@ \
	  gnu/*/*.class gnu/*/*/*.class kawa/*.class kawa/*/*.class

kawa-compiled.zip: kawa-@VERSION@-compiled.zip
kawa-compiled.jar: kawa-@VERSION@-compiled.jar

EXTRA_DIST = debian/control debian/rules debian/changelog debian/dirs \
  config.guess config.sub install-sh ./mkinstalldirs makekawa.bat \
  $(JAVATOP)kawa/Version.java

HTML_HOME = $$HOME/public_html
JAVADOC_DIR = $$HOME/Kawa/htdocs/api
JAVADOC_PACKAGES = kawa kawa.lang kawa.standard \
  gnu.bytecode gnu.ecmascript gnu.expr gnu.mapping gnu.math gnu.text \
  gnu.kawa.reflect gnu.jemacs.lang gnu.jemacs.buffer
JAVADOC = javadoc
JAVADOC_FLAGS = -J-Xms40m -J-Xmx40m

MAINTAINERCLEANFILES = makekawa.bat
MOSTLYCLEANFILES = tmp

install-javadoc-html:
	rm -rf $(JAVADOC_DIR)
	-mkdir $(JAVADOC_DIR)
	cd $(top_srcdir) && $(JAVADOC) $(JAVADOC_FLAGS) -d $(JAVADOC_DIR) $(JAVADOC_PACKAGES)
	cp $(srcdir)/COPYING  $(JAVADOC_DIR)/COPYING

install-html: install-javadoc-html
	cp $(srcdir)/NEWS $(HTML_HOME)/kawa/News.txt
	cd doc;  make install-html HTML_HOME="$(HTML_HOME)"
	cd gnu;  make install-html HTML_HOME="$(HTML_HOME)"

$(JAVATOP)kawa/Version.java: $(srcdir)/$(JAVATOP)kawa/Version.java.in $(srcdir)/configure.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/$(JAVATOP)kawa/Version.java.in >$@

all-recursive:  $(JAVATOP)kawa/Version.java

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

foo:
	for dir in $$(grep '^java_JAVA =' $$(find . -name Makefile -print)| sed -e 's|./\(.*\)/Makefile:.*|\1|' -e 's|/|\\|'); do \
	  echo "%javac% %jcflags% $$dir\\*.java" >>tmp; \
	done
