## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
  AliasConstraint.java \
  ApplyMethodContainer.java ApplyMethodProc.java \
  Binding.java \
  Binding2.java \
  BindingEnumeration.java \
  Constraint.java UnboundConstraint.java TrivialConstraint.java \
  IndirectConstraint.java \
  CallFrame.java CallContext.java CpsProcedure.java \
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
  SFormat.java \
  Setter.java \
  Setter0.java \
  Setter1.java \
  SysInPort.java \
  TtyInPort.java \
  UnboundSymbol.java \
  Values.java \
  WrappedException.java \
  WrongArguments.java \
  WrongType.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-mapping

all: $(@GCJ_COMPILED_SELECTED@)
