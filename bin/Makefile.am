bin_PROGRAMS = @kawa_bin@
EXTRA_PROGRAMS = kawa
noinst_SCRIPTS = kawa.sh
kawa_SOURCES = kawa.c
CLEANFILES = kawa.sh kawa$(EXEEXT) kawa1$(EXEEXT) kawa-bin$(EXEEXT) tmp *.stamp

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
ENABLE_GCJ_COMPILED_DEFS = -DGCJ_COMPILED
DISABLE_GCJ_COMPILED_DEFS = -DKAWALIB='"$(datadir)/java"'
DEFS = $(@GCJ_COMPILED_SELECTED@_DEFS) -Dxxx
$(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline -lhistory $(kawa_EXTRA_LIBS)

kawa.sh:
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
	chmod +x tmp
	mv tmp kawa.sh

KAWA1_OFILES = \
  gnu-bytecode.o \
  gnu-lists.o \
  gnu-expr.o \
  gnu-kawa-functions.o \
  gnu-kawa-lispexpr.o \
  gnu-kawa-reflect.o \
  gnu-kawa-util.o \
  gnu-mapping.o \
  gnu-math.o \
  gnu-text.o \
  kawa-lang.o \
  kawa-repl.o \
  kawa-standard.o

KAWA2_OFILES = $(KAWA1_OFILES) \
  kawa-lib.o \
  gnu-kawa-slib.o \
  gnu-xml.o

kawa1$(exeext): $(KAWA1_OFILES)
	 gcj -o kawa1$(exeext) $(KAWA1_OFILES) --main=kawa.repl

kawa-bin$(exeext): $(KAWA2_OFILES)
	 gcj -o kawa-bin$(exeext) $(KAWA2_OFILES) --main=kawa.repl

ENABLE_GCJ_COMPILED = kawa-bin$(exeext)
DISABLE_GCJ_COMPILED = kawa.sh

all: $(@GCJ_COMPILED_SELECTED@)

install-exec-local: install-exec-bindir

install-exec-bindir: $(noninst_SCRIPTS)
	$(mkinstalldirs) $(bindir)
	if test "@kawa_bin@" = "" ; then \
	  $(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa; \
	fi
