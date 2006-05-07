# Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
set_b.java \
prim_method.java prim_throw.java throw_name.java \
not.java \
append.java \
fluid_let.java \
atan.java \
expt.java \
string2number.java \
vector_append.java \
read.java \
readchar.java \
read_line.java \
char_ready_p.java \
load.java \
constant_fold.java \
map.java \
callcc.java \
define.java \
define_syntax.java syntax_rules.java let_syntax.java \
syntax.java syntax_case.java \
require.java module_extends.java module_implements.java module_name.java \
module_compile_options.java with_compile_options.java \
module_static.java export.java define_unit.java \
syntax_error.java \
let.java \
begin.java \
call_with_values.java \
sleep.java \
try_catch.java \
make.java location.java \
define_alias.java define_variable.java define_autoload.java \
define_member_alias.java \
Scheme.java \
object.java define_class.java thisRef.java \
TracedProcedure.java \
IfFeature.java

PACKAGE_FNAME = kawa-standard
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
