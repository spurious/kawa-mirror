## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

.SUFFIXES : .java .class

JAVAC=javac -classpath ../..:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java/kawa/standard

OBJ=\
set_b.class \
not.class \
boolean_p.class \
eqv_p.class \
eq_p.class \
equal_p.class \
pair_p.class \
cons.class \
car.class \
cdr.class \
setcar_b.class \
setcdr_b.class \
caar.class \
cadr.class \
cdar.class \
cddr.class \
caaar.class \
caadr.class \
cadar.class \
caddr.class \
cdaar.class \
cdadr.class \
cddar.class \
cdddr.class \
caaaar.class \
caadar.class \
caaadr.class \
caaddr.class \
cadaar.class \
cadadr.class \
caddar.class \
cadddr.class \
cdaaar.class \
cdaadr.class \
cdadar.class \
cdaddr.class \
cddaar.class \
cddadr.class \
cdddar.class \
cddddr.class \
null_p.class \
list_p.class \
list.class \
length.class \
append.class \
list_tail.class \
list_ref.class \
mem.class \
ass.class \
symbol_p.class \
symbol2string.class \
string2symbol.class \
number_p.class \
real_p.class \
integer_p.class \
zero_p.class \
positive_p.class \
negative_p.class \
equal_oper.class \
less_oper.class \
lessequal_oper.class \
greater_oper.class \
greaterequal_oper.class \
plus_oper.class \
minus_oper.class \
multiply_oper.class \
divide_oper.class \
abs.class \
char_p.class \
char_equal_p.class \
char_less_p.class \
char_less_equal_p.class \
char_greater_p.class \
char_greater_equal_p.class \
string_p.class \
make_string.class \
string.class \
string_length.class \
string_ref.class \
string_set_b.class \
vector_p.class \
make_vector.class \
vector.class \
vector_length.class \
vector_ref.class \
vector_set_b.class \
read.class \
readchar.class \
eof_object_p.class \
write.class \
writechar.class \
load.class \
procedure_p.class \
apply.class \
exit.class \
define.class \
let.class \
letrec.class \
letstar.class \
begin.class \
ifp.class \
when.class \
unless.class \
and.class \
or.class \
StandardInterpreter.class 

java_DATA=$(OBJ)

all: $(OBJ)
