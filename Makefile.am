## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa

SUFFIXES = .java .class

JAVAC=javac -classpath .:$(srcdir):$$CLASSPATH

.java.class:
	$(JAVAC) -d . $<

# This redundant rule is needed for some versions of make, but not GNU make.
kawac.class:  $(srcdir)/kawac.java
	$(JAVAC) -d . $<

kawa.txt:  $(srcdir)/kawa.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

kawa.html:  $(srcdir)/kawa.texi
	texi2html $<

javadir=$(prefix)/java

OBJ = kawa.class kawac.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

EXTRA_DIST = kawa.txt kawa.html license.terms scm2java.html
info_TEXINFOS = kawa.texi
CLEANFILES=$(OBJ)

all-recursive:  kawac.class
