## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

EXCEPTIONS=\
GenericError.java\
NamedException.java

java_JAVA=\
Pattern.java \
AnyPat.java \
EqualPat.java \
ListPat.java \
ListRepeatPat.java \
PairPat.java \
VarListPat.java \
\
Lambda.java \
Quote.java \
Translator.java \
Eval.java \
AutoloadProcedure.java \
Continuation.java \
CalledContinuation.java \
Syntax.java Macro.java \
SyntaxRule.java \
SyntaxRules.java \
AutoloadSyntax.java \
CompileFile.java \
loadcompiled.java \
SpecialType.java \
SyntaxForm.java \
Promise.java \
Record.java GetFieldProc.java SetFieldProc.java RecordConstructor.java \
$(EXCEPTIONS)

PACKAGE_FNAME = kawa-lang
CLEANFILES = *.class *.o *.stamp

all: $(@GCJ_COMPILED_SELECTED@)
