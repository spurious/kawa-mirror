## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  AliasConstraint.java \
  ApplyMethodContainer.java ApplyMethodProc.java \
  Constraint.java UnboundConstraint.java TrivialConstraint.java \
  ConstantConstraint.java \
  IndirectConstraint.java \
  CallFrame.java CpsProcedure.java \
  CpsMethodContainer.java CpsMethodProc.java \
  Environment.java \
  FluidBinding.java \
  FluidConstraint.java \
  Future.java \
  HasSetter.java \
  InPort.java \
  Location.java \
  LogWriter.java \
  MethodProc.java \
  NameMap.java \
  Named.java \
  OutPort.java \
  CharArrayInPort.java \
  CharArrayOutPort.java \
  Printable.java \
  ProcLocation.java \
  Procedure.java \
  Procedure0.java \
  Procedure0or1.java \
  Procedure1.java \
  Procedure1or2.java \
  Procedure2.java \
  Procedure3.java \
  Procedure4.java \
  ProcedureN.java \
  Setter.java \
  Setter0.java \
  Setter1.java \
  Symbol.java \
  SymbolEnumeration.java \
  TtyInPort.java \
  UnboundSymbol.java \
  Values.java \
  ValueStack.java \
  WrappedException.java \
  WrongArguments.java \
  WrongType.java

java_BUILT = \
  CallContext.java

CallContext.java: $(srcdir)/CallContext.java.in
	sed $(DONT_EDIT_SUBS) $(SED_REFERENCES_SUBS) \
	  <$(srcdir)/CallContext.java.in >CallContext.java

BUILT_SOURCES = $(java_BUILT)
EXTRA_DIST = package.html CallContext.java.in

PACKAGE_FNAME = gnu-mapping
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
