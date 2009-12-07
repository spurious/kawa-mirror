## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

EXCEPTIONS=\
GenericError.java\
NamedException.java

java_sources = \
Pattern.java \
AnyPat.java \
EqualPat.java \
ListPat.java \
ListRepeatPat.java \
PairPat.java \
VarListPat.java \
Lambda.java \
Quote.java \
Translator.java \
Eval.java \
AutoloadProcedure.java \
Continuation.java \
CalledContinuation.java \
Syntax.java Macro.java \
SyntaxPattern.java \
SyntaxRule.java \
SyntaxRules.java \
PatternScope.java \
TemplateScope.java \
SyntaxTemplate.java \
AutoloadSyntax.java \
CompileFile.java \
SyntaxForm.java \
SyntaxForms.java \
Promise.java \
Record.java GetFieldProc.java SetFieldProc.java RecordConstructor.java \
$(EXCEPTIONS)

PACKAGE_FNAME = kawa-lang
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
