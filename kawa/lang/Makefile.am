## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .java

javadir=$(datadir)/java/$(subdir)

EXCEPTIONS=\
GenericError.java\
NamedException.java

java_JAVA=\
Char.java \
QueueReader.java \
ScmRead.java \
NullReader.java CharArrayInPort.java \
\
Pattern.java \
AnyPat.java \
EqualPat.java \
ListPat.java \
ListRepeatPat.java \
PairPat.java \
VarListPat.java \
\
Sequence.java \
List.java \
Pair.java \
PairWithPosition.java \
FString.java \
Vector.java \
Lambda.java \
Quote.java \
Translator.java \
Interpreter.java \
Eval.java \
AutoloadProcedure.java \
Continuation.java \
CalledContinuation.java \
Syntax.java Macro.java \
SyntaxRule.java \
SyntaxRules.java \
DefMacro.java \
AutoloadSyntax.java \
CompileFile.java \
loadcompiled.java \
SpecialType.java \
PrimGetStatic.java PrimSetStatic.java \
PrimArrayNew.java PrimArrayLength.java PrimArraySet.java PrimArrayGet.java\
Promise.java \
Record.java GetFieldProc.java SetFieldProc.java RecordConstructor.java \
$(EXCEPTIONS)
