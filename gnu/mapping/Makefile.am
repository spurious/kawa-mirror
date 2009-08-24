## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  CallContext.java \
  ConstrainedLocation.java \
  Environment.java \
  EnvironmentKey.java \
  Future.java \
  HasNamedParts.java \
  HasSetter.java \
  IndirectableLocation.java \
  InheritingEnvironment.java \
  InPort.java \
  KeyPair.java \
  LazyPropertyKey.java \
  Location.java \
  LocationEnumeration.java \
  LocationProc.java \
  LogWriter.java \
  MethodProc.java \
  Named.java \
  NamedLocation.java \
  Namespace.java \
  OutPort.java \
  CharArrayInPort.java \
  CharArrayOutPort.java \
  PlainLocation.java \
  Procedure.java \
  Procedure0.java \
  Procedure0or1.java \
  Procedure1.java \
  Procedure1or2.java \
  Procedure2.java \
  Procedure3.java \
  Procedure4.java \
  ProcedureN.java \
  ProcLocation.java \
  PropertyKey.java \
  PropertyLocation.java \
  PropertySet.java \
  ReadOnlyLocation.java \
  RunnableClosure.java \
  Setter.java \
  Setter0.java \
  Setter1.java \
  SharedLocation.java \
  SimpleEnvironment.java \
  SimpleSymbol.java \
  Symbol.java \
  Table2D.java \
  ThreadLocation.java \
  TtyInPort.java \
  UnboundLocationException.java \
  Values.java \
  ValueStack.java \
  WrappedException.java \
  WrongArguments.java \
  WrongType.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-mapping
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
