## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .java

javadir=$(datadir)/java/$(subdir)

EXCEPTIONS=\
UnboundSymbol.java \
WrongArguments.java \
WrongType.java \
GenericError.java\
NamedException.java \
ReadError.java \
EofReadError.java

java_JAVA=\
Char.java \
LineBufferedReader.java QueueReader.java \
InPort.java \
OutPort.java \
TtyInPort.java SysInPort.java \
LogWriter.java NullReader.java CharArrayInPort.java \
Target.java StackTarget.java IgnoreTarget.java TailTarget.java \
ConditionalTarget.java \
Expression.java \
ApplyExp.java \
BeginExp.java \
ErrorExp.java \
IfExp.java \
LambdaExp.java \
LetExp.java \
QuoteExp.java \
ReferenceExp.java \
ScopeExp.java \
SetExp.java SetApplyExp.java \
ModuleExp.java \
TryExp.java CatchClause.java \
\
Declaration.java \
Location.java \
Binding.java \
Environment.java \
\
Pattern.java \
AnyPat.java \
EqualPat.java \
ListPat.java \
ListRepeatPat.java \
PairPat.java \
VarListPat.java \
\
Printable.java \
Named.java Inlineable.java \
Sequence.java \
List.java \
Pair.java \
PairWithPosition.java \
FString.java \
SFormat.java \
Symbol.java \
Keyword.java Special.java \
Vector.java \
Undefined.java \
Lambda.java \
Quote.java \
Translator.java \
Interpreter.java \
Eval.java \
Procedure.java \
Procedure0.java \
Procedure1.java \
Procedure2.java \
Procedure3.java \
Procedure4.java \
Procedure0or1.java \
Procedure1or2.java \
ProcedureN.java \
AutoloadProcedure.java \
HasSetter.java Setter.java Setter0.java Setter1.java \
ModuleBody.java \
Continuation.java \
CalledContinuation.java \
Syntax.java \
SyntaxRule.java \
SyntaxRules.java \
DefMacro.java \
AutoloadSyntax.java \
Literal.java \
Compilation.java \
Compilable.java \
CompileFile.java \
loadcompiled.java \
SchemeLoader.java \
CompiledProc.java \
SpecialType.java PrimProcedure.java \
PrimGetStatic.java PrimSetStatic.java \
PrimArrayNew.java PrimArrayLength.java PrimArraySet.java PrimArrayGet.java\
Values.java \
Promise.java \
Future.java \
Record.java GetFieldProc.java SetFieldProc.java RecordConstructor.java \
$(EXCEPTIONS)
