## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa doc testsuite

SUFFIXES = .java .class

JAVA = java
JAVAC=javac -classpath .:$(srcdir):$$CLASSPATH

.java.class:
	$(JAVAC) -d . $<

# This redundant rule is needed for some versions of make, but not GNU make.
kawac.class:  $(srcdir)/kawac.java
	$(JAVAC) -d . $<

javadir=$(prefix)/java

OBJ = kawac.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all-recursive:  kawac.class
