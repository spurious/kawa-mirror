## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

javadir=$(prefix)/java/$(subdir)

EXCEPTIONS=\
UnboundSymbol.class \
WrongArguments.class \
WrongType.class \
GenericError.class\
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
ModuleExp.class \
\
Declaration.class \
Location.class \
Binding.class \
Environment.class \
\
Pattern.class \
AnyPat.class \
EqualPat.class \
ListPat.class \
ListRepeatPat.class \
PairPat.class \
VarListPat.class \
\
Printable.class \
Named.class \
Sequence.class \
List.class \
Pair.class \
PairWithPosition.class \
FString.class \
SFormat.class \
Symbol.class \
Vector.class \
Undefined.class \
Lambda.class \
Quote.class \
Interpreter.class \
Eval.class \
Procedure.class \
Procedure0.class \
Procedure1.class \
Procedure2.class \
Procedure3.class \
Procedure4.class \
Procedure0or1.class \
Procedure1or2.class \
ProcedureN.class \
AutoloadProcedure.class \
ModuleBody.class \
Continuation.class \
CalledContinuation.class \
Syntax.class \
SyntaxRule.class \
SyntaxRules.class \
AutoloadSyntax.class \
Literal.class \
Compilation.class \
Compilable.class \
CompileFile.class \
loadcompiled.class \
SchemeLoader.class \
CompiledProc.class \
Values.class \
Promise.class \
Future.class \
$(EXCEPTIONS)

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)

