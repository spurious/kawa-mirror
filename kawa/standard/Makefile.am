# Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
set_b.java \
quasiquote.java \
prim_method.java prim_throw.java throw_name.java \
not.java \
and_or.java \
cxr.java \
append.java \
integer_p.java \
max.java \
min.java \
gcd.java \
lcm.java \
fluid_let.java \
rationalize.java \
atan.java \
sqrt.java \
expt.java \
exact2inexact.java \
inexact2exact.java \
string2number.java \
string_v.java \
string_append.java \
vector_append.java \
force.java \
call_with_input_file.java \
call_with_output_file.java \
with_input_from_file.java \
with_output_to_file.java \
read.java \
readchar.java \
read_line.java \
char_ready_p.java \
open_input_file.java \
open_output_file.java \
call_with_output_string.java \
load.java \
constant_fold.java \
map.java \
callcc.java \
logand.java logior.java logxor.java \
define.java \
define_syntax.java syntax_rules.java let_syntax.java syntax_case.java \
require.java module_extends.java module_implements.java module_name.java \
module_compile_options.java with_compile_options.java \
module_static.java export.java define_unit.java \
syntax_error.java \
let.java \
letrec.java \
begin.java \
ifp.java \
call_with_values.java \
make_quantity.java quantity2number.java quantity2unit.java \
make_future.java sleep.java \
try_catch.java try_finally.java synchronizd.java \
make.java location.java \
define_alias.java define_variable.java define_autoload.java \
define_member_alias.java \
ScmEnv.java Scheme.java \
object.java define_class.java thisRef.java \
TracedProcedure.java \
IfFeature.java \
scheme_env.java

PACKAGE_FNAME = kawa-standard
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
