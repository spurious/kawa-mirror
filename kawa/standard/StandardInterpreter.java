package kawa.standard;
import kawa.lang.*;

public class StandardInterpreter extends Interpreter
{
  final void define_proc (Named proc)
  {
    define (proc.name (), proc);
  }

  /* Define a procedure to be autoloaded. */
  final void define_proc (String name, String className)
  {
    Symbol symbol = Symbol.make (name);
    define (symbol, new AutoloadProcedure (symbol, className));
  }

  /* Define a Syntax to be autoloaded. */
  final void define_syntax (String name, String className)
  {
    Symbol symbol = Symbol.make (name);
    define (symbol, new AutoloadSyntax (symbol, className));
  }

  public kawa.standard.StandardInterpreter(InPort i, OutPort o, OutPort e)
  {
      super(i,o,e);

      kawa.lang.Named proc;
      kawa.lang.Named syn;

      kawa.lang.Procedure2 eqv;
      kawa.lang.Procedure2 eq;
      kawa.lang.Procedure2 equal;

      //-- Section 4.1  -- complete
      define (Interpreter.quote_sym, new kawa.lang.Quote ());
      define_syntax ("define", "kawa.standard.define");
      define_syntax ("if", "kawa.standard.ifp");
      define_syntax ("set!", "kawa.standard.set_b");

      // Section 4.2  -- complete
      define_syntax ("cond", "kawa.lib.cond");
      define_syntax ("case", "kawa.lib.case");
      define ("and", new kawa.standard.and_or (true));
      define ("or", new kawa.standard.and_or (false));
      define_syntax ("%let", "kawa.standard.let");
      define_syntax ("let", "kawa.lib.let");
      define_syntax ("let*", "kawa.standard.letstar");
      define_syntax ("letrec", "kawa.standard.letrec");
      define_syntax ("begin", "kawa.standard.begin");
      define_syntax ("do", "kawa.lib.do");
      define_syntax ("delay", "kawa.lib.delay");
      define_proc ("%make-promise", "kawa.standard.make_promise");
      define_syntax ("quasiquote", "kawa.standard.quasiquote");

      //-- Section 5  -- complete [except for internal definitions]
      define_syntax ("lambda", "kawa.lang.Lambda");

      //-- Section 6.1  -- complete
      define_proc ("not", "kawa.standard.not");
      define_proc ("boolean?", "kawa.standard.boolean_p");

      //-- Section 6.2  -- complete
      eqv = new kawa.standard.eqv_p();
      define(eqv.name (), eqv);
      eq = new kawa.standard.eq_p();
      define(eq.name (), eq);
      equal = new kawa.standard.equal_p();
      define(equal.name (), equal);

      //-- Section 6.3  -- complete
      define_proc ("pair?", "kawa.standard.pair_p");
      define ("cons", kawa.standard.cons.consProcedure);
      define_proc ("car", "kawa.standard.car");
      define_proc ("cdr", "kawa.standard.cdr");
      define_proc ("set-car!", "kawa.standard.setcar_b");
      define_proc ("set-cdr!", "kawa.standard.setcdr_b");

      define_proc ("caar", "kawa.standard.cxr");
      define_proc ("cadr", "kawa.standard.cxr");
      define_proc ("cdar", "kawa.standard.cxr");
      define_proc ("cddr", "kawa.standard.cxr");
      define_proc ("caaar", "kawa.standard.cxr");
      define_proc ("caadr", "kawa.standard.cxr");
      define_proc ("cadar", "kawa.standard.cxr");
      define_proc ("caddr", "kawa.standard.cxr");
      define_proc ("cdaar", "kawa.standard.cxr");
      define_proc ("cdadr", "kawa.standard.cxr");
      define_proc ("cddar", "kawa.standard.cxr");
      define_proc ("cdddr", "kawa.standard.cxr");
      define_proc ("caaaar", "kawa.standard.cxr");
      define_proc ("caaadr", "kawa.standard.cxr");
      define_proc ("caadar", "kawa.standard.cxr");
      define_proc ("caaddr", "kawa.standard.cxr");
      define_proc ("cadaar", "kawa.standard.cxr");
      define_proc ("cadadr", "kawa.standard.cxr");
      define_proc ("caddar", "kawa.standard.cxr");
      define_proc ("cadddr", "kawa.standard.cxr");
      define_proc ("cdaaar", "kawa.standard.cxr");
      define_proc ("cdaadr", "kawa.standard.cxr");
      define_proc ("cdadar", "kawa.standard.cxr");
      define_proc ("cdaddr", "kawa.standard.cxr");
      define_proc ("cddaar", "kawa.standard.cxr");
      define_proc ("cddadr", "kawa.standard.cxr");
      define_proc ("cdddar", "kawa.standard.cxr");
      define_proc ("cddddr", "kawa.standard.cxr");
      define_proc ("null?", "kawa.standard.null_p");
      define_proc ("list?", "kawa.standard.list_p");
      define_proc ("list", "kawa.standard.list");
      define_proc ("length", "kawa.standard.length");
      define ("append", kawa.standard.append.appendProcedure);
      define_proc ("reverse", "kawa.standard.reverse");
      define_proc ("list-tail", "kawa.standard.list_tail");
      define_proc ("list-ref", "kawa.standard.list_ref");

      proc = new kawa.standard.mem("memq",eq);
      define(proc.name (), proc);
      proc = new kawa.standard.mem("memv",eqv);
      define(proc.name (), proc);
      proc = new kawa.standard.mem("member",equal);
      define(proc.name (), proc);
      proc = new kawa.standard.ass("assq",eq);
      define(proc.name (), proc);
      proc = new kawa.standard.ass("assv",eqv);
      define(proc.name (), proc);
      proc = new kawa.standard.ass("assoc",equal);
      define(proc.name (), proc);

      //-- Section 6.4  -- complete, including slashified read/write
      
      define_proc ("symbol?", "kawa.standard.symbol_p");
      define_proc ("symbol->string", "kawa.standard.symbol2string");
      define_proc ("string->symbol", "kawa.standard.string2symbol");

      //-- Section 6.5
      define_proc ("number?", "kawa.standard.number_p");
      define_proc ("complex?", "kawa.standard.real_p");  // For now
      define_proc ("real?", "kawa.standard.real_p");
      define_proc ("rational?", "kawa.standard.rational_p");
      define_proc ("integer?", "kawa.standard.integer_p");
      define_proc ("exact?", "kawa.standard.exact_p");
      define_proc ("inexact?", "kawa.standard.inexact_p");
      define_proc ("=", "kawa.standard.equal_oper");
      define_proc ("<", "kawa.standard.less_oper");
      define_proc (">", "kawa.standard.greater_oper");
      define_proc ("<=", "kawa.standard.lessequal_oper");
      define_proc (">=", "kawa.standard.greaterequal_oper");
      define_proc ("zero?", "kawa.standard.zero_p");
      define_proc ("positive?", "kawa.standard.positive_p");
      define_proc ("negative?", "kawa.standard.negative_p");
      define_proc ("odd?", "kawa.standard.odd_p");
      define_proc ("even?", "kawa.standard.even_p");
      define_proc ("+", "kawa.standard.plus_oper");
      define_proc ("-", "kawa.standard.minus_oper");
      define_proc ("*", "kawa.standard.multiply_oper");
      define_proc ("/", "kawa.standard.divide_oper");
      define_proc ("abs", "kawa.standard.abs");
      define_proc ("numerator", "kawa.standard.numerator");
      define_proc ("denominator", "kawa.standard.denominator");
      define_proc ("floor", "kawa.standard.floor");
      define_proc ("ceiling", "kawa.standard.ceiling");
      define_proc ("truncate", "kawa.standard.truncate");
      define_proc ("round", "kawa.standard.round");
      define_proc ("exp", "kawa.standard.exp");
      define_proc ("log", "kawa.standard.log");
      define_proc ("sin", "kawa.standard.sin");
      define_proc ("cos", "kawa.standard.cos");
      define_proc ("tan", "kawa.standard.tan");
      define_proc ("asin", "kawa.standard.asin");
      define_proc ("acos", "kawa.standard.acos");
      define_proc ("atan", "kawa.standard.atan");
      define_proc ("sqrt", "kawa.standard.sqrt");
      define_proc ("expt", "kawa.standard.expt");
      define_proc ("exact->inexact", "kawa.standard.exact2inexact");
      define_proc ("inexact->exact", "kawa.standard.inexact2exact");
      define_proc ("number->string", "kawa.standard.number2string");

      //-- Section 6.6  -- complete
      define_proc ("char?", "kawa.standard.char_p");
      define_proc ("char=?", "kawa.standard.char_equal_p");
      define_proc ("char<?", "kawa.standard.char_less_p");
      define_proc ("char>?", "kawa.standard.char_greater_p");
      define_proc ("char<=?", "kawa.standard.char_less_equal_p");
      define_proc ("char>=?", "kawa.standard.char_greater_equal_p");
      define_proc ("char-ci=?", "kawa.standard.char_ci_equal_p");
      define_proc ("char-ci<?", "kawa.standard.char_ci_less_p");
      define_proc ("char-ci>?", "kawa.standard.char_ci_greater_p");
      define_proc ("char-ci<=?", "kawa.standard.char_ci_less_equal_p");
      define_proc ("char-ci>=?", "kawa.standard.char_ci_greater_equal_p");
      define_proc ("char-alphabetic?", "kawa.standard.char_alphabetic_p");
      define_proc ("char-numeric?", "kawa.standard.char_numeric_p");
      define_proc ("char-whitespace?", "kawa.standard.char_whitespace_p");
      define_proc ("char-upper-case?", "kawa.standard.char_upper_case_p");
      define_proc ("char-lower-case?", "kawa.standard.char_lower_case_p");
      define_proc ("char->integer", "kawa.standard.char2integer");
      define_proc ("integer->char", "kawa.standard.integer2char");
      define_proc ("char-upcase", "kawa.standard.char_upcase");
      define_proc ("char-downcase", "kawa.standard.char_downcase");
      
      //-- Section 6.7  -- complete
      define_proc ("string?", "kawa.standard.string_p");
      define_proc ("make-string", "kawa.standard.make_string");
      define_proc ("string", "kawa.standard.string");
      define_proc ("string-length", "kawa.standard.string_length");
      define_proc ("string-ref", "kawa.standard.string_ref");
      define_proc ("string-set!", "kawa.standard.string_set_b");

      define_proc ("string=?", "kawa.standard.string_equal_p");
      define_proc ("string-ci=?", "kawa.standard.string_ci_equal_p");
      define_proc ("string<?", "kawa.standard.string_lessthan_p");
      define_proc ("string>?", "kawa.standard.string_greaterthan_p");
      define_proc ("string<=?", "kawa.standard.string_lessequal_p");
      define_proc ("string>=?", "kawa.standard.string_greaterequal_p");

      define_proc ("string-ci<?", "kawa.standard.string_ci_lessthan_p");
      define_proc ("string-ci>?", "kawa.standard.string_ci_greaterthan_p");
      define_proc ("string-ci<=?", "kawa.standard.string_ci_lessequal_p");
      define_proc ("string-ci>=?", "kawa.standard.string_ci_greaterequal_p");

      define_proc ("substring", "kawa.standard.substring");
      define_proc ("string-append", "kawa.standard.string_append");
      define_proc ("string->list", "kawa.standard.string2list");
      define_proc ("list->string", "kawa.standard.list2string");
      define_proc ("string-copy", "kawa.standard.string_copy");
      define_proc ("string-fill!", "kawa.standard.string_fill_b");

      //-- Section 6.8  -- complete
      define_proc ("vector?", "kawa.standard.vector_p");
      define_proc ("make-vector", "kawa.standard.make_vector");
      define ("vector", kawa.standard.vector.vectorProcedure);
      define_proc ("vector-length", "kawa.standard.vector_length");
      define_proc ("vector-ref", "kawa.standard.vector_ref");
      define_proc ("vector-set!", "kawa.standard.vector_set_b");
      define_proc ("list->vector", "kawa.standard.list2vector");
      define_proc ("vector->list", "kawa.standard.vector2list");
      define_proc ("vector-fill!", "kawa.standard.vector_fill_b");
      // Extension:
      define ("vector-append", kawa.standard.vector_append.vappendProcedure);

      //-- Section 6.9  -- complete [except restricted call/cc]
      define_proc ("procedure?", "kawa.standard.procedure_p");
      define_proc ("apply", "kawa.standard.apply");
      define_proc (new map (true));        // map
      define_proc (new map (false));       // for-each
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");
      define_proc ("force", "kawa.standard.force");

      //-- Section 6.10 [complete except for char-ready? and transcript-on/off]
      define_proc ("call-with-input-file",
		   "kawa.standard.call_with_input_file");
      define_proc ("call-with-input-string",  //Extension
		   "kawa.standard.call_with_input_string");
      define_proc ("call-with-output-file",
		   "kawa.standard.call_with_output_file");
      define_proc ("input-port?", "kawa.standard.input_port_p.java");
      define_proc ("output-port?", "kawa.standard.output_port_p.java");
      define_proc ("current-input-port", "kawa.standard.current_input_port");
      define_proc ("current-output-port", "kawa.standard.current_output_port");
      define_proc ("with-input-from-file",
		   "kawa.standard.with_input_from_file");
      define_proc ("with-output-to-file",
		   "kawa.standard.with_output_to_file");
      define_proc ("open-input-file", "kawa.standard.open_input_file");
      define_proc ("open-output-file", "kawa.standard.open_output_file");
      define_proc ("close-input-port", "kawa.standard.close_input_port");
      define_proc ("close-output-port", "kawa.standard.close_output_port");
      define_proc ("read", "kawa.standard.read");
      define_proc (new readchar (false));  // read-char
      define_proc (new readchar (true));   // peek-char
      define_proc ("eof-object?", "kawa.standard.eof_object_p");
      define_proc (new write(true));       // write
      define_proc (new write(false));      // display
      define_proc ("write-char", "kawa.standard.writechar");
      define_proc ("newline", "kawa.standard.newline");
      define_proc ("load", "kawa.standard.load");

      define_syntax ("%syntax-error", "kawa.standard.syntax_error");

      define_proc ("exit", "kawa.standard.exit");
      define_proc ("values", "kawa.standard.values");
      define_proc ("call-with-values", "kawa.standard.call_with_values");

      define ("define-syntax", new kawa.standard.define_syntax ());
      //-- (when cond exp ...)
      define_syntax ("when", "kawa.lib.when_unless");
      //-- (unless cond exp ...)
      define_syntax ("unless", "kawa.lib.when_unless");

      define_proc ("compile-file", "kawa.lang.CompileFile");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
   }
}
