## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa

SUFFIXES = .java .class

JAVAC=javac -classpath .:$$CLASSPATH -d @TOP_BUILDDIR@

.java.class:
	cd $(srcdir); $(JAVAC) $(<F)

# This redundant rule is needed for some versions of make, but not GNU make.
kawac.class:  $(srcdir)/kawac.java
	cd $(srcdir); $(JAVAC) kawac.java

javadir=$(prefix)/java

OBJ = kawa.class kawac.class

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all-recursive:  kawac.class
