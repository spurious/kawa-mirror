## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

javadir=$(prefix)/java/$(subdir)

OBJ=\
set_b.class \
quasiquote.class \
not.class \
boolean_p.class \
and_or.class \
eqv_p.class \
eq_p.class \
equal_p.class \
pair_p.class \
cons.class \
car.class \
cdr.class \
setcar_b.class \
setcdr_b.class \
cxr.class \
null_p.class \
list_p.class \
list_v.class \
length.class \
append.class \
reverse.class \
list_tail.class \
list_ref.class \
mem.class \
ass.class \
symbol_p.class \
symbol2string.class \
string2symbol.class \
number_p.class quantity_p.class complex_p.class \
real_p.class rational_p.class integer_p.class \
exact_p.class \
inexact_p.class \
zero_p.class \
positive_p.class \
negative_p.class \
odd_p.class \
even_p.class \
max.class \
min.class \
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
quotient.class \
remainder.class \
modulo.class \
gcd.class \
lcm.class \
numerator.class \
denominator.class \
floor.class \
ceiling.class \
truncate.class \
round.class \
rationalize.class \
exp.class \
log.class \
sin.class \
cos.class \
tan.class \
asin.class \
acos.class \
atan.class \
sqrt.class \
expt.class \
make_rectangular.class \
real_part.class imag_part.class \
exact2inexact.class \
inexact2exact.class \
number2string.class \
string2number.class \
char_p.class \
char_equal_p.class \
char_less_p.class \
char_less_equal_p.class \
char_greater_p.class \
char_greater_equal_p.class \
char_ci_equal_p.class \
char_ci_less_p.class \
char_ci_less_equal_p.class \
char_ci_greater_p.class \
char_ci_greater_equal_p.class \
char_alphabetic_p.class \
char_numeric_p.class \
char_whitespace_p.class \
char_upper_case_p.class \
char_lower_case_p.class \
char2integer.class \
integer2char.class \
char_upcase.class \
char_downcase.class \
string_p.class \
make_string.class \
string_v.class \
string_length.class \
string_ref.class \
string_set_b.class \
string2list.class \
list2string.class \
string_equal_p.class \
string_ci_equal_p.class \
string_lessthan_p.class \
string_lessequal_p.class \
string_greaterthan_p.class \
string_greaterequal_p.class \
string_ci_lessthan_p.class \
string_ci_lessequal_p.class \
string_ci_greaterthan_p.class \
string_ci_greaterequal_p.class \
substring.class \
string_append.class \
string_copy.class \
string_fill_b.class \
vector_p.class \
make_vector.class \
vector_v.class \
vector_length.class \
vector_ref.class \
vector_set_b.class \
vector2list.class \
list2vector.class \
vector_fill_b.class \
vector_append.class \
force.class \
make_promise.class \
call_with_input_file.class \
call_with_output_file.class \
current_input_port.class \
current_output_port.class \
with_input_from_file.class \
with_output_to_file.class \
read.class \
readchar.class \
eof_object_p.class \
char_ready_p.class \
write.class \
newline.class \
writechar.class \
input_port_p.class \
output_port_p.class \
open_input_file.class \
open_output_file.class \
close_input_port.class \
close_output_port.class \
call_with_input_string.class \
call_with_output_string.class \
load.class \
procedure_p.class \
apply.class \
map.class \
callcc.class \
exit.class \
logand.class logior.class logxor.class lognot.class logop.class \
logbit_p.class logtest.class logcount.class \
ashift.class bit_extract.class int_length.class \
define.class \
define_syntax.class \
syntax_error.class \
let.class \
letrec.class \
begin.class \
ifp.class \
values_v.class \
call_with_values.class \
make_quantity.class quantity2number.class quantity2unit.class \
Scheme.class \
null_env.class \
user_env.class \
scheme_env.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)
