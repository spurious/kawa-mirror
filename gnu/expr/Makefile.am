## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  AccessExp.java \
  ApplyExp.java \
  BeginExp.java \
  BindingInitializer.java \
  BlockExp.java \
  BuiltinEnvironment.java \
  CanInline.java \
  CatchClause.java \
  ChainLambdas.java \
  CheckedTarget.java \
  ClassExp.java \
  ClassInitializer.java \
  Compilation.java \
  ConditionalTarget.java \
  ConsumerTarget.java \
  Declaration.java \
  ErrorExp.java \
  ExitExp.java \
  Expression.java \
  ExpWalker.java \
  FindCapturedVars.java \
  FindTailCalls.java \
  FluidLetExp.java \
  GenericProc.java \
  IfExp.java \
  IgnoreTarget.java \
  Initializer.java \
  Inlineable.java \
  InlineCalls.java \
  KawaConvert.java \
  Keyword.java \
  LambdaExp.java \
  LangExp.java \
  Language.java \
  LetExp.java \
  Literal.java \
  LitTable.java \
  ModuleBody.java \
  ModuleContext.java \
  ModuleInfo.java \
  ModuleManager.java \
  ModuleMethod.java \
  ModuleSet.java \
  ModuleWithContext.java \
  ModuleExp.java \
  NameLookup.java \
  PairClassType.java \
  ResolveNames.java \
  ObjectExp.java \
  QuoteExp.java \
  PrimProcedure.java \
  ProcInitializer.java \
  PushApply.java \
  ReferenceExp.java \
  ScopeExp.java \
  SeriesTarget.java \
  SetExp.java \
  Special.java \
  StackTarget.java \
  Symbols.java \
  SynchronizedExp.java \
  Target.java \
  ThisExp.java \
  TryExp.java \
  TypeValue.java

EXTRA_DIST = package.html

PACKAGE_FNAME = gnu-expr
GCJ_INPUTS =  $(java_Java)

all:  Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
