## Process this with automake to create Makefile.in

java_sources = \
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
GCJ_INPUTS =  $(java_Java)

EXTRA_DIST = package.html

include $(top_srcdir)/Make-rules

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
