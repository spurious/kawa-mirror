if WITH_GCJ
bin_PROGRAMS =  @kawa_bin@ kawa-bin
bin_SCRIPTS = gckawa
else
bin_PROGRAMS =  @kawa_bin@
endif

EXTRA_PROGRAMS = kawa
noinst_SCRIPTS = kawa.sh
kawa_SOURCES = kawa.c
kawa_bin_SOURCES =
CLEANFILES = kawa.sh kawa$(EXEEXT) kawa1$(EXEEXT) kawa-bin$(EXEEXT) tmp *.stamp

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
WITH_GCJ_DEFS = -DGCJ_COMPILED
WITHOUT_GCJ_DEFS = -DKAWALIB='"$(datadir)/java/kawa-$(VERSION).jar"'
DEFS = $(@GCJ_SELECTED@_DEFS)
INCLUDES = $(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline -lhistory $(kawa_EXTRA_LIBS)

if ENABLE_XML
KAWA2_XML_OFILES = gnu-xml.o gnu-xquery-util.o gnu-xquery-lang.o
LIBADD_XML = gnu-xml.lo gnu-xquery-util.lo gnu-xquery-lang.lo
endif

kawa.sh:
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java/kawa-$(VERSION).jar}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
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

KAWA1_OFILES = \
  gnu-bytecode.lo \
  gnu-lists.lo \
  gnu-expr.lo \
  gnu-kawa-functions.lo \
  gnu-kawa-lispexpr.lo \
  gnu-kawa-reflect.lo \
  gnu-kawa-util.lo \
  gnu-mapping.lo \
  gnu-math.lo \
  gnu-text.lo \
  gnu-commonlisp-lang.lo \
  kawa-lang.lo \
  kawa-repl.lo \
  kawa-standard.lo

gnu-commonlisp-lang.lo:
	cd ../gnu/commonlisp/lang && $(MAKE)
kawa-lang.lo:
	cd ../kawa/lang && $(MAKE)
kawa-standard.lo:
	cd ../kawa/standard && $(MAKE)
kawa-repl.lo:
	cd ../kawa && $(MAKE) ../bin/kawa-repl.lo

KAWA2_OFILES = $(KAWA1_OFILES) $(KAWA2_XML_OFILES) \
  kawa-lib.o \
  gnu-commonlisp-lisp.o \
  gnu-kawa-slib.o

WITH_GCJ_LIBS =  libkawa.la
WITHOUT_GCJ_LIBS =
#lib_LTLIBRARIES = $(@GCJ_SELECTED@_LIBS)
#EXTRA_LTLIBRARIES = libkawa.la
if WITH_GCJ
lib_LTLIBRARIES     = libkawa.la
endif
#libkawa_la_LDFLAGS = -rpath $(libdir)
libkawa_la_SOURCES = 
libkawa_la_LIBADD = \
  $(KAWA1_OFILES) \
  gnu-ecmascript.lo \
  $(LIBADD_XML) \
  gnu-kawa-slib.lo \
  gnu-commonlisp-lisp.lo \
  kawa-lib.lo

kawa1$(EXEEXT): $(KAWA1_OFILES)
	../libtool --mode=link --tag=GCJ $(GCJ) \
	  -o kawa1$(EXEEXT) $(KAWA1_OFILES) --main=kawa.repl

kawa-bin$(EXEEXT): libkawa.la
	../libtool --mode=link --tag=GCJ $(GCJ) -g -O2  -o kawa-bin$(EXEEXT)  libkawa.la --main=kawa.repl

install-exec-local: install-exec-bindir

install-exec-bindir: $(noninst_SCRIPTS)
	$(mkinstalldirs) $(bindir)
	if test "@kawa_bin@" = "" ; then \
	  $(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa; \
	fi
if WITH_GCJ
	$(INSTALL_SCRIPT) gckawa $(DESTDIR)$(bindir)/gckawa
endif
