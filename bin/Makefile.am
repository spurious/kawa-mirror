if WITH_GCJ
bin_SCRIPTS = gckawa qexo
if ENABLE_KAWA_FRONTEND
KAWAPROGS = kawa-bin kawa
else
KAWAPROGS = kawa-bin
endif
else
bin_SCRIPTS = qexo
if ENABLE_KAWA_FRONTEND
KAWAPROGS = kawa
endif
endif

bin_PROGRAMS = $(KAWAPROGS) cgi-servlet

EXTRA_PROGRAMS = kawa
noinst_SCRIPTS = kawa.sh
kawa_SOURCES = kawa.c kawapath.c
cgi_servlet_SOURCES = cgi-servlet.c kawapath.c
kawa_bin_SOURCES =
CLEANFILES = kawa.sh qexo kawa$(EXEEXT) kawa-bin$(EXEEXT) tmp *.stamp
AM_CFLAGS = -fPIC
LIBTOOL = $(top_builddir)/libtool

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
WITH_GCJ_DEFS = -DJAVA='"$(JAVA)"' -DGCJ_COMPILED
WITHOUT_GCJ_DEFS = -DJAVA='"$(JAVA)"' -DKAWALIB='"$(datadir)/java/kawa-$(VERSION).jar"' -DKAWAJAR='"kawa-$(VERSION).jar"'
DEFS = $(@GCJ_SELECTED@_DEFS)
INCLUDES = $(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline -lhistory $(kawa_EXTRA_LIBS)

if ENABLE_XML
LIBADD_XML = gnu-xml.lo gnu-kawa-xml.lo gnu-xquery-util.lo \
  gnu-xquery-lang.lo gnu-brl.lo gnu-kawa-brl.lo gnu-kawa-xslt.lo
endif
if WITH_SWING
if ENABLE_JEMACS
LIBADD_JEMACS_SWING = gnu-jemacs-swing.lo
endif
LIBADD_SWINGVIEWS = gnu-kawa-swingviews.lo
endif
if ENABLE_JEMACS
if WITH_SWT
LIBADD_JEMACS_SWT = gnu-jemacs-swt.lo
endif
LIBADD_JEMACS = gnu-jemacs-lang.lo gnu-jemacs-buffer.lo gnu-jemacs-lisp.lo \
  $(LIBADD_JEMACS_SWING) $(LIBADD_JEMACS_SWT)
endif
if ENABLE_SERVLET
LIBADD_SERVLET = gnu-kawa-servlet.lo
EXTRA_LIB_SERVLET = -l-javax-servlet
endif

kawa.sh:
	echo '#!/bin/sh' >tmp
if WITH_GCJ
	echo 'exec `echo "$$0" | sed -e '"'"'s|/kawa[^/]*$$|/kawa-bin|'"'"'` "$$@"' >> tmp
else
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java/kawa-$(VERSION).jar}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
endif
	chmod +x tmp
	mv tmp kawa.sh

gckawa:
	echo '#!/bin/sh' >tmp
	echo 'LD_LIBRARY_PATH=$(libdir):$$LD_LIBRARY_PATH' >>tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java/kawa-$(VERSION).jar}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH LD_LIBRARY_PATH' >>tmp
	echo 'exec gcj "$$@" -L$(libdir) -lkawa' >>tmp
	chmod +x tmp
	mv tmp gckawa

qexo:
	echo '#!/bin/sh' >tmp
	echo 'case $$0 in' >>tmp
	echo '  */qexo)' >>tmp
	echo '    kawa=`echo $$0 | sed -e '"'"'s|qexo$$|kawa|'"'"'`' >>tmp
	echo '    if test -x $$kawa; then true' >>tmp
if ENABLE_KAWA_FRONTEND
else
	echo '    elif test -x $$kawa.sh; then' >>tmp
	echo '      kawa=$$kawa.sh' >>tmp
endif
	echo '    else' >>tmp
	echo '      kawa="kawa"' >>tmp
	echo '    fi' >>tmp
	echo '  ;;' >>tmp
	echo '  *)' >>tmp
	echo '    kawa="kawa";' >>tmp
	echo 'esac' >>tmp
	echo 'exec $$kawa --xquery "$$@"' >>tmp
	chmod +x tmp
	mv tmp qexo

gnu-commonlisp-lang.lo:
	cd ../gnu/commonlisp/lang && $(MAKE)
kawa-lang.lo:
	cd ../kawa/lang && $(MAKE)
kawa-standard.lo:
	cd ../kawa/standard && $(MAKE)
kawa-repl.lo:
	cd ../kawa && $(MAKE) ../bin/kawa-repl.lo
gnu-xquery-lang.lo:
	cd ../gnu/xquery/lang && $(MAKE)
gnu-xquery-util.lo:
	cd ../gnu/xquery/util && $(MAKE)

WITH_GCJ_LIBS =  libkawa.la
WITHOUT_GCJ_LIBS =
#lib_LTLIBRARIES = $(@GCJ_SELECTED@_LIBS)
#EXTRA_LTLIBRARIES = libkawa.la
if WITH_GCJ
lib_LTLIBRARIES     = libkawa.la
endif
libkawa_la_LDFLAGS = -Wl,-Bsymbolic
libkawa_la_SOURCES = 
libkawa_la_LIBADD = \
  gnu-bytecode.lo \
  gnu-lists.lo \
  gnu-expr.lo \
  gnu-kawa-functions.lo \
  gnu-kawa-lispexpr.lo \
  gnu-kawa-reflect.lo \
  gnu-kawa-util.lo \
  gnu-kawa-models.lo \
  gnu-mapping.lo \
  gnu-math.lo \
  gnu-text.lo \
  gnu-commonlisp-lang.lo \
  kawa-lang.lo \
  kawa-repl.lo \
  kawa-standard.lo \
  $(LIBADD_XML) \
  $(LIBADD_SERVLET) \
  $(LIBADD_SWINGVIEWS) \
  $(LIBADD_JEMACS) \
  gnu-ecmascript.lo \
  gnu-kawa-slib.lo \
  gnu-commonlisp-lisp.lo \
  kawa-lib.lo

kawa-bin$(EXEEXT): libkawa.la
	../libtool --mode=link --tag=GCJ $(GCJ)  $(AM_CFLAGS) $(CFLAGS) -o kawa-bin$(EXEEXT)  --main=kawa.repl libkawa.la $(EXTRA_LIB_SERVLET) $(EXTRA_LIB_XML)

install-exec-local: install-exec-bindir

install-exec-bindir: $(noninst_SCRIPTS)
	$(mkinstalldirs) $(bindir)
if ENABLE_KAWA_FRONTEND
else
	$(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa
endif
	$(INSTALL_SCRIPT) qexo $(DESTDIR)$(bindir)/qexo
if WITH_GCJ
	$(INSTALL_SCRIPT) gckawa $(DESTDIR)$(bindir)/gckawa
endif

uninstall-local: uninstall-exec-bindir

uninstall-exec-bindir:
	rm -f $(DESTDIR)$(bindir)/kawa $(DESTDIR)$(bindir)/gckawa $(DESTDIR)$(bindir)/qexo
