## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa

SUFFIXES = .java .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@
MAKEINFO=makeinfo

.java.class:
	cd $(srcdir); $(JAVAC) $(<F)

# This redundant rule is needed for some versions of make, but not GNU make.
kawac.class:  $(srcdir)/kawac.java
	cd $(srcdir); $(JAVAC) kawac.java

kawa.txt:  $(srcdir)/kawa.texi
	$(MAKEINFO) --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,2d' \
          >$@


javadir=$(prefix)/java

OBJ = kawa.class kawac.class

java_DATA=$(OBJ)

EXTRA_DIST = $(OBJ:.class=.java) kawa.txt
info_TEXINFOS = kawa.texi
CLEANFILES=$(OBJ)

all-recursive:  kawac.class
