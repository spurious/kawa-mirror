## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@

.java.class:
	cd $(srcdir); $(JAVAC) $(<F)

javadir=$(prefix)/java/codegen

OBJ=\
Access.class \
ArrayType.class \
ClassType.class \
CpoolClass.class \
CpoolEntry.class \
CpoolInt.class \
CpoolLong.class \
CpoolNameAndType.class \
CpoolRef.class \
CpoolString.class \
CpoolUtf8.class \
Field.class \
Label.class \
Method.class \
Scope.class \
Type.class \
Variable.class \
VarEnumerator.class \
ZipArchive.class \
ZipMember.class \
ZipLoader.class

java_DATA=$(OBJ)

EXTRA_DIST = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)
