## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC = @JAVAC@
JAVACFLAGS = -g
SOURCES = $(java_JAVA)

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
Access.java \
Attribute.java \
AttrContainer.java \
ObjectType.java ArrayType.java \
ClassType.java \
ConstantPool.java \
CpoolClass.java \
CpoolEntry.java \
CpoolValue1.java \
CpoolValue2.java \
CpoolNameAndType.java \
CpoolRef.java \
CpoolString.java \
CpoolUtf8.java \
Location.java Field.java \
Label.java IfState.java TryState.java SwitchState.java \
Method.java \
CodeAttr.java CodeFragment.java \
LineNumbersAttr.java LocalVarsAttr.java InnerClassesAttr.java \
MiscAttr.java \
PrimType.java \
Scope.java \
SourceFileAttr.java \
Type.java \
Variable.java \
VarEnumerator.java \
ZipArchive.java \
ZipMember.java \
ZipLoader.java \
ArrayClassLoader.java \
ClassFileInput.java \
ClassTypeWriter.java \
ExceptionsAttr.java \
dump.java

JC1FLAGS = -g
GCC = ../../../gcc/xgcc -B../../../gcc/

gnu-bytecode.zip: classjava.stamp
	cd ../..; \
	  zip -q -r -n .class gnu/bytecode/gnu-bytecode.zip \
	  gnu/bytecode -i '*.class' -x gnu/bytecode/ZipLoader.class

gnu-bytecode.o:  gnu-bytecode.zip
	CLASSPATH=`pwd`/../../../libjava \
	  $(GCC) -c -fassume-compiled $(JC1FLAGS) \
	  -x java gnu-bytecode.zip -o gnu-bytecode.o

dump-main.o:  dump-main.cc
	$(GCC) -g -I$(srcdir)/../../../libjava/include -c \
	  $(srcdir)/dump-main.cc -o dump-main.o	

dump$(exext):  dump-main.o gnu-bytecode.o
	$(GCC) -o dump dump-main.o gnu-bytecode.o \
	  ../../../libjava/libjava.a

EXTRA_DIST = package.html
