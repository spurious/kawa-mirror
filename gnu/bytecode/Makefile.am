## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

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
Filter.java \
Location.java Field.java \
Label.java IfState.java TryState.java SwitchState.java \
Method.java \
CodeAttr.java CodeFragment.java ConstantValueAttr.java \
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

PACKAGE_FNAME = gnu-bytecode

ENABLE_GCJ_COMPILED = $(OFILES_DIR)/$(PACKAGE_FNAME).o dump$(exext)

all: $(@GCJ_COMPILED_SELECTED@)

dump$(exext):  $(OFILES_DIR)/gnu-bytecode.o
	$(GCJ) -o dump --main=gnu.bytecode.dump $(OFILES_DIR)/gnu-bytecode.o

CLEANFILES = *.class *.o *.stamp dump$(exext)

EXTRA_DIST = package.html

