KAWAJAR = kawa-$(VERSION).jar
KAWALIB = $(datadir)/java/$(KAWAJAR)
WITHOUT_GCJ_DEFS = -DJAVA='"$(JAVA)"' -DKAWALIB='"$(KAWALIB)"' -DKAWAJAR='"$(KAWAJAR)"' -DEXTRAPATH='"$(conf_classpath)"'

if ENABLE_KAWA_FRONTEND
KAWAPROGS_KAWA = kawa
endif
if ON_WINDOWS
# kawa.sh is still needed for 'make check', even on Windows.
KAWASCRIPTS_KAWA = kawa.bat kawa.sh
else
KAWASCRIPTS_KAWA = kawa.sh
endif
noinst_SCRIPTS = kawa.sh

if WITH_GCJ
bin_SCRIPTS = gckawa qexo $(KAWASCRIPTS_KAWA)
if WITH_GCJ_DBTOOL
AM_CFLAGS = -fPIC
CDEFS = $(WITHOUT_GCJ_DEFS)
else !WITH_GCJ_DBTOOL
CDEFS = -DJAVA='"$(JAVA)"' -DGCJ_COMPILED
KAWAPROGS_KAWA_BIN = kawa-bin
if WITH_LIBTOOL
RPATH_OPTION = -rpath $(libdir)
else !WITH_LIBTOOL
RPATH_OPTION = -Wl,-rpath -Wl,$(libdir)
endif !WITH_LIBTOOL
endif !WITH_GCJ_DBTOOL
DEFSy = 
else !WITH_GCJ
CDEFS = $(WITHOUT_GCJ_DEFS)
bin_SCRIPTS = qexo $(KAWASCRIPTS_KAWA)
endif !WITH_GCJ
DEFS = $(CDEFS)

bin_PROGRAMS = $(KAWAPROGS_KAWA) $(KAWAPROGS_KAWA_BIN) cgi-servlet

kawa_SOURCES = kawa.c kawapath.c
cgi_servlet_SOURCES = cgi-servlet.c kawapath.c
kawa_bin_SOURCES =
CLEANFILES = qexo kawa$(EXEEXT) kawa.sh kawa.bat kawa-bin$(EXEEXT) gckawa tmp *.stamp *.so *.la

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
AM_CPPFLAGS = $(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline -lhistory $(kawa_EXTRA_LIBS)

if WITH_LIBTOOL
OEXT = .lo
else
OEXT = .o
endif

if ENABLE_XML
if WITH_SAX2
LIBADD_SAX2 = gnu-kawa-sax$(OEXT)
endif
if ENABLE_BRL
LIBADD_BRL = gnu-brl$(OEXT) gnu-kawa-brl$(OEXT) 
endif
LIBADD_XML = gnu-xml$(OEXT) gnu-kawa-xml$(OEXT) $(LIBADD_SAX2) $(LIBADD_BRL) \
  gnu-xquery-util$(OEXT) gnu-xquery-lang$(OEXT) gnu-kawa-xslt$(OEXT)
endif
if WITH_SWING
if ENABLE_JEMACS
LIBADD_JEMACS_SWING = gnu-jemacs-swing$(OEXT)
endif
LIBADD_SWINGVIEWS = gnu-kawa-swingviews$(OEXT)
endif
if ENABLE_JEMACS
if WITH_SWT
LIBADD_JEMACS_SWT = gnu-jemacs-swt$(OEXT)
endif
LIBADD_JEMACS = gnu-jemacs-lang$(OEXT) gnu-jemacs-buffer$(OEXT) gnu-jemacs-lisp$(OEXT) \
  $(LIBADD_JEMACS_SWING) $(LIBADD_JEMACS_SWT)
endif
if ENABLE_SERVLET
LIBADD_SERVLET = gnu-kawa-servlet$(OEXT)
EXTRA_LIB_SERVLET = -l-javax-servlet
endif

kawa.sh:
	echo '#!/bin/sh' >tmp-kawa.sh
	echo 'thisfile=`which $$0`' >>tmp-kawa.sh
	echo 'thisdir=`dirname $$thisfile`' >>tmp-kawa.sh
if ENABLE_KAWA_FRONTEND
# In this case kawa.sh is only used for pre-install testing.
# We don't need to set KAWALIB, since kawapath.c looks for it in "..".
	echo 'LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$$thisdir' >>tmp-kawa.sh
	echo 'export LD_LIBRARY_PATH' >>tmp-kawa.sh
	echo 'exec $$thisdir/kawa "$$@"' >>tmp-kawa.sh
else
if WITH_GCJ_NO_DBTOOL
	echo 'LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$$thisdir:$(libdir)' >>tmp-kawa.sh
	echo 'export LD_LIBRARY_PATH' >>tmp-kawa.sh
	echo 'exec $$thisdir/kawa-bin "$$@"' >>tmp-kawa.sh
else
	echo 'if test -f $$thisdir/../$(KAWAJAR)' >>tmp-kawa.sh
	echo 'then KAWALIB=$${KAWALIB-"`$(CYGPATH_W) "$$thisdir/../$(KAWAJAR)"`"}' >>tmp-kawa.sh
	echo 'else KAWALIB=$${KAWALIB-$(KAWALIB)}; fi' >>tmp-kawa.sh
	echo 'CLASSPATH="$${KAWALIB}$(CLASSPATH_SEPARATOR)$(conf_classpath)$${CLASSPATH-$(CLASSPATH)}"' >>tmp-kawa.sh
	echo 'export CLASSPATH' >>tmp-kawa.sh
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-"'$${JAVA}'"} kawa.repl "$$@"' >>tmp-kawa.sh
endif
endif
	chmod +x tmp-kawa.sh
	mv tmp-kawa.sh kawa.sh

gckawa: Makefile
	echo '#!/bin/sh' >tmp-gckawa
	echo 'LD_LIBRARY_PATH=$(libdir):$$LD_LIBRARY_PATH' >>tmp-gckawa
	echo 'KAWALIB=$${KAWALIB-$(KAWALIB)}' >>tmp-gckawa
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp-gckawa
	echo 'export CLASSPATH LD_LIBRARY_PATH' >>tmp-gckawa
	echo 'exec gcj $(EXTRA_GCJFLAGS) "$$@" -L$(libdir) $(LDFLAGS_DBTOOL) -lkawa' >>tmp-gckawa
	chmod +x tmp-gckawa
	mv tmp-gckawa gckawa

kawa.bat: $(srcdir)/kawa.bat.in
	sed -e 's|%DEFAULT_KAWA_VERSION%|$(VERSION)|' \
	    -e 's|%DEFAULT_EXTRA_PATH%|'`echo '$(conf_classpath)'| sed -e 's|[\]|\\\\|g' -e 's|\&|\\&|g'`'|' \
	  <$(srcdir)/kawa.bat.in > kawa.bat

qexo:
	echo '#!/bin/sh' >tmp-qexo
	echo 'case $$0 in' >>tmp-qexo
	echo '  */qexo)' >>tmp-qexo
	echo '    kawa=`echo $$0 | sed -e '"'"'s|qexo$$|kawa|'"'"'`' >>tmp-qexo
	echo '    if test -x $$kawa.sh; then' >>tmp-qexo
	echo '      kawa=$$kawa.sh' >>tmp-qexo
	echo '    elif test -x $$kawa; then true' >>tmp-qexo
	echo '    else' >>tmp-qexo
	echo '      kawa="kawa"' >>tmp-qexo
	echo '    fi' >>tmp-qexo
	echo '  ;;' >>tmp-qexo
	echo '  *)' >>tmp-qexo
	echo '    kawa="kawa";' >>tmp-qexo
	echo 'esac' >>tmp-qexo
	echo 'exec $$kawa --xquery "$$@"' >>tmp-qexo
	chmod +x tmp-qexo
	mv tmp-qexo qexo

gnu-commonlisp-lang$(OEXT):
	cd ../gnu/commonlisp/lang && $(MAKE)
kawa-lang$(OEXT):
	cd ../kawa/lang && $(MAKE)
kawa-standard$(OEXT):
	cd ../kawa/standard && $(MAKE)
kawa-repl$(OEXT):
	cd ../kawa && $(MAKE) ../bin/kawa-repl$(OEXT)
gnu-xquery-lang$(OEXT):
	cd ../gnu/xquery/lang && $(MAKE)
gnu-xquery-util$(OEXT):
	cd ../gnu/xquery/util && $(MAKE)

if WITH_LIBTOOL
LIBKAWA_LIB = libkawa.la
libLTLIBRARIES_INSTALL = $(INSTALL_DATA)
else
LIBKAWA_LIB = libkawa-$(libkawa_VERSION).so
endif
binPROGRAMS_INSTALL = $(INSTALL_PROGRAM)

#EXTRA_LTLIBRARIES = libkawa.la
if WITH_GCJ_DBTOOL
LDFLAGS_DBTOOL = -Wl,-Bsymbolic
EXTRA_GCJFLAGS = -fPIC -findirect-dispatch
endif
libkawa_VERSION = $(VERSION)
libkawa_OFILES = \
  gnu-bytecode$(OEXT) \
  gnu-lists$(OEXT) \
  gnu-expr$(OEXT) \
  gnu-kawa-functions$(OEXT) \
  gnu-kawa-lispexpr$(OEXT) \
  gnu-kawa-reflect$(OEXT) \
  gnu-kawa-util$(OEXT) \
  gnu-kawa-models$(OEXT) \
  gnu-mapping$(OEXT) \
  gnu-math$(OEXT) \
  gnu-text$(OEXT) \
  gnu-commonlisp-lang$(OEXT) \
  kawa-lang$(OEXT) \
  kawa-repl$(OEXT) \
  kawa-standard$(OEXT) \
  $(LIBADD_XML) \
  $(LIBADD_SERVLET) \
  $(LIBADD_SWINGVIEWS) \
  $(LIBADD_JEMACS) \
  gnu-ecmascript$(OEXT) \
  gnu-kawa-slib$(OEXT) \
  gnu-commonlisp-lisp$(OEXT) \
  kawa-lib$(OEXT)

LIBTOOL = libtool

$(LIBKAWA_LIB): $(libkawa_OFILES)
if WITH_LIBTOOL
	$(LIBTOOL) --tag=CC --mode=link $(CC) -o $(LIBKAWA_LIB) $(RPATH_OPTION) $(libdir) -release $(libkawa_VERSION) $(libkawa_OFILES)
else
	$(CC) -shared -Wl,-soname -Wl,libkawa-$(libkawa_VERSION).so -o libkawa-$(libkawa_VERSION).so $(libkawa_OFILES)
endif

kawa-bin$(EXEEXT): $(LIBKAWA_LIB)
if WITH_LIBTOOL
	$(LIBTOOL) --mode=link --tag=GCJ $(GCJ)  $(AM_CFLAGS) $(CFLAGS) -o kawa-bin$(EXEEXT)  --main=kawa.repl libkawa.la $(EXTRA_LIB_SERVLET) $(EXTRA_LIB_XML)
else
	$(GCJ)  $(AM_CFLAGS) $(CFLAGS) -o kawa-bin$(EXEEXT)  --main=kawa.repl libkawa-$(libkawa_VERSION).so $(EXTRA_LIB_SERVLET) $(EXTRA_LIB_XML) $(RPATH_OPTION) 
endif

kawa$(EXEEXT): $(kawa_OBJECTS)
	$(CCLD) -o kawa$(EXEEXT) $(kawa_LDFLAGS) $(kawa_OBJECTS) $(kawa_LDADD) $(LIBS)

cgi-servlet$(EXEEXT): $(cgi_servlet_OBJECTS)
	$(CCLD) -o cgi-servlet$(EXEEXT) $(cgi_servlet_LDFLAGS) $(cgi_servlet_OBJECTS) $(LIBS)

install-binSCRIPTS: $(bin_SCRIPTS)
	$(INSTALL_SCRIPT) qexo $(DESTDIR)$(bindir)/qexo
if ! ENABLE_KAWA_FRONTEND
	$(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa
endif
if WITH_GCJ
	$(INSTALL_SCRIPT) gckawa $(DESTDIR)$(bindir)/gckawa
endif

if WITH_LIBTOOL
LIBTOOL_INSTALL = $(LIBTOOL) --mode=install
endif

install-binPROGRAMS: $(bin_PROGRAMS)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
if ENABLE_KAWA_FRONTEND
	$(INSTALL_PROGRAM_ENV) $(LIBTOOL_INSTALL) $(binPROGRAMS_INSTALL) kawa$(EXEEXT) $(DESTDIR)$(bindir)/kawa$(EXEEXT)
endif
if WITH_GCJ
if ! WITH_GCJ_DBTOOL
	$(INSTALL_PROGRAM_ENV) $(LIBTOOL_INSTALL) $(binPROGRAMS_INSTALL) kawa-bin$(EXEEXT) $(DESTDIR)$(bindir)/kawa-bin$(EXEEXT)
endif
endif
if ENABLE_SERVLET
	$(INSTALL_PROGRAM_ENV) $(LIBTOOL_INSTALL) $(binPROGRAMS_INSTALL) cgi-servlet$(EXEEXT) $(DESTDIR)$(bindir)/cgi-servlet$(EXEEXT)
endif

if WITH_GCJ
install-exec-local: install-libkawa
endif

install-libkawa: $(LIBKAWA_LIB)
	@$(NORMAL_INSTALL)
	test -z "$(libdir)" || $(mkdir_p) "$(DESTDIR)$(libdir)"
if WITH_LIBTOOL
	$(LIBTOOL) --mode=install $(libLTLIBRARIES_INSTALL) $(INSTALL_STRIP_FLAG) $(LIBKAWA_LIB) $(DESTDIR)$(libdir)/$(LIBKAWA_LIB)
else
	$(INSTALL_DATA) $(LIBKAWA_LIB) $(INSTALL_STRIP_FLAG) $(DESTDIR)$(libdir)/$(LIBKAWA_LIB)
endif

uninstall-local: uninstall-exec-bindir

uninstall-exec-bindir:
	rm -f $(DESTDIR)$(bindir)/kawa $(DESTDIR)$(bindir)/gckawa $(DESTDIR)$(bindir)/qexo

EXTRA_DIST = kawa.bat.in
