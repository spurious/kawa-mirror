## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

.SUFFIXES: .java .class

JAVAC=javac -classpath ..:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java/codegen

OBJ=\
Access.class \
ArrayType.class \
ClassType.class \
CpoolClass.class \
CpoolEntry.class \
CpoolInt.class \
CpoolNameAndType.class \
CpoolRef.class \
CpoolString.class \
CpoolUtf8.class \
Field.class \
Label.class \
Method.class \
Scope.class \
Type.class \
Variable.class 

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all: $(OBJ)

