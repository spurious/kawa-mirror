## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa

SUFFIXES = .java .class

MAKEINFO=makeinfo
JAVAC=javac -classpath .:$(srcdir):$$CLASSPATH

.java.class:
	$(JAVAC) -d . $<

# This redundant rule is needed for some versions of make, but not GNU make.
kawac.class:  $(srcdir)/kawac.java
	$(JAVAC) -d . $<

kawa.txt:  $(srcdir)/kawa.texi
	$(MAKEINFO) --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@


javadir=$(prefix)/java

OBJ = kawa.class kawac.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

EXTRA_DIST = kawa.txt
info_TEXINFOS = kawa.texi
CLEANFILES=$(OBJ)

all-recursive:  kawac.class
