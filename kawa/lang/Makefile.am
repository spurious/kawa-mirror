## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

.SUFFIXES : .java .class

JAVAC=javac -classpath ../..:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java/kawa/lang

EXCEPTIONS=\
UnboundSymbol.class \
WrongArguments.class \
WrongType.class \
GenericError.class\
SyntaxError.class \
ReadError.class \
EofReadError.class

OBJ=\
Char.class \
InPort.class \
OutPort.class \
Expression.class \
ApplyExp.class \
BeginExp.class \
ErrorExp.class \
IfExp.class \
LambdaExp.class \
LetExp.class \
QuoteExp.class \
ReferenceExp.class \
ScopeExp.class \
SetExp.class \
\
Declaration.class \
Environment.class \
\
Pattern.class \
EqualPat.class \
ListPat.class \
VarListPat.class \
\
Printable.class \
Named.class \
Sequence.class \
List.class \
Pair.class \
print.class \
Symbol.class \
Vector.class \
Executable.class \
Syntaxable.class \
Exit.class \
Undefined.class \
Lambda.class \
LambdaProcedure.class \
Quote.class \
QuasiQuote.class \
Unquote.class \
UnquoteSplicing.class \
Interpreter.class \
Procedure.class \
Procedure0.class \
Procedure1.class \
Procedure2.class \
Procedure3.class \
Procedure4.class \
Procedure0or1.class \
Procedure1or2.class \
ProcedureN.class \
Syntax.class \
$(EXCEPTIONS)

java_DATA=$(OBJ)

all: $(OBJ)

