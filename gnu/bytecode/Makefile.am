## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

javadir=$(prefix)/java/$(subdir)

OBJ=\
Access.class \
ArrayType.class \
ClassType.class \
ConstantPool.class \
CpoolClass.class \
CpoolEntry.class \
CpoolValue1.class \
CpoolValue2.class \
CpoolNameAndType.class \
CpoolRef.class \
CpoolString.class \
CpoolUtf8.class \
Field.class \
IfState.class \
Label.class \
Method.class \
Scope.class \
Type.class \
Variable.class \
VarEnumerator.class \
ZipArchive.class \
ZipMember.class \
ZipLoader.class \
ClassFileInput.class \
ClassTypeWriter.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)
