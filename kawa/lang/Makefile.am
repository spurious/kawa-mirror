## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

.SUFFIXES : .java .class

JAVAC=javac -classpath ../..:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java/kawa/lang

EXCEPTIONS=\
EOFInComment.class \
UnexpectedCloseParen.class \
InvalidPoundConstruct.class \
EOFInString.class \
NumberTooLong.class \
InvalidCharacterName.class \
MalformedList.class \
UnboundSymbol.class \
WrongArguments.class \
WrongType.class \
GenericError.class \
NotImplemented.class

OBJ=\
Expression.class \
ApplyExp.class \
BeginExp.class \
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
List.class \
Pair.class \
print.class \
iport.class \
oport.class \
Symbol.class \
vector.class \
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
Procedure1or2.class \
ProcedureN.class \
Syntax.class \
$(EXCEPTIONS)

java_DATA=$(OBJ)

all: $(OBJ)

