package kawa.standard;
import kawa.lang.*;

public class Scheme extends Interpreter
{
  // transitional hack FIXME
  private static Environment env;
  public static Environment curEnvironment ()
  {
    return env;
  }

  public void define(String name, Object p)
  {
    env.put (Symbol.make (name), p);
  }

  public void define(Symbol sym, Object p)
  {
    env.define (sym, p);
  }

  public Object lookup(java.lang.String name)
  {
    return env.get (Symbol.make (name));
  }

  public Object lookup(Symbol name)
  {
    return env.get (name);
  }

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

  static Environment null_environment;
  static Environment r4_environment;
  static Environment r5_environment;
  static Environment user_environment;

  public static Syntax beginSyntax;
  public static Syntax defineSyntax;

  // FIX transitinal hack - should create new user env each time
  public static Environment makeEnvironment ()
  {
    if (user_environment == null)
      new Scheme ();
    return user_environment;
  }

  public Scheme ()
  {
      kawa.lang.Named proc;
      kawa.lang.Named syn;

      kawa.lang.Procedure2 eqv;
      kawa.lang.Procedure2 eq;
      kawa.lang.Procedure2 equal;

      // (null-environment)
      null_environment = new Environment ();
      null_environment.setName ("null-environment");
      env = null_environment;

      //-- Section 4.1  -- complete
      define (Interpreter.quote_sym, new kawa.lang.Quote ());
      define ("define", defineSyntax = new kawa.standard.define());
      define_syntax ("if", "kawa.standard.ifp");
      define_syntax ("set!", "kawa.standard.set_b");

      // Section 4.2  -- complete
      define_syntax ("cond", "kawa.lib.std_syntax");
      define_syntax ("case", "kawa.lib.std_syntax");
      define ("and", new kawa.standard.and_or (true));
      define ("or", new kawa.standard.and_or (false));
      define_syntax ("%let", "kawa.standard.let");
      define_syntax ("let", "kawa.lib.std_syntax");
      define_syntax ("let*", "kawa.lib.std_syntax");
      define_syntax ("letrec", "kawa.standard.letrec");

      define ("begin", beginSyntax = new kawa.standard.begin());
      define_syntax ("do", "kawa.lib.std_syntax");
      define_syntax ("delay", "kawa.lib.std_syntax");
      define_proc ("%make-promise", "kawa.standard.make_promise");
      define_syntax ("quasiquote", "kawa.standard.quasiquote");

      //-- Section 5  -- complete [except for internal definitions]
      define_syntax ("lambda", "kawa.lang.Lambda");

      // Appendix (and R5RS)
      define ("define-syntax", new kawa.standard.define_syntax ());

      r4_environment = new Environment (null_environment);
      r4_environment.setName ("r4rs-environment");
      env = r4_environment;

      //-- Section 6.1  -- complete
      define_proc ("not", "kawa.standard.not");
      define_proc ("boolean?", "kawa.standard.boolean_p");

      //-- Section 6.2  -- complete
      eqv = new kawa.standard.eqv_p();
      define("eqv?", eqv);
      eq = new kawa.standard.eq_p();
      define("eq?", eq);
      equal = new kawa.standard.equal_p();
      define("equal?", equal);

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
      define_proc ("list", "kawa.standard.list_v");
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
      define_proc ("quantity?", "kawa.standard.number_p");
      define_proc ("complex?", "kawa.standard.complex_p");
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
      define_proc ("max", "kawa.standard.max");
      define_proc ("min", "kawa.standard.min");
      define_proc ("+", "kawa.standard.plus_oper");
      define_proc ("-", "kawa.standard.minus_oper");
      define_proc ("*", "kawa.standard.multiply_oper");
      define_proc ("/", "kawa.standard.divide_oper");
      define_proc ("abs", "kawa.standard.abs");
      define_proc ("quotient", "kawa.standard.quotient");
      define_proc ("remainder", "kawa.standard.remainder");
      define_proc ("modulo", "kawa.standard.modulo");
      define_proc ("gcd", "kawa.standard.gcd");
      define_proc ("lcm", "kawa.standard.lcm");
      define_proc ("numerator", "kawa.standard.numerator");
      define_proc ("denominator", "kawa.standard.denominator");
      define_proc ("floor", "kawa.standard.floor");
      define_proc ("ceiling", "kawa.standard.ceiling");
      define_proc ("truncate", "kawa.standard.truncate");
      define_proc ("round", "kawa.standard.round");
      define_proc ("rationalize", "kawa.standard.rationalize");
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
      define_proc ("make-rectangular", "kawa.standard.make_rectangular");
      define_proc ("make-polar", "kawa.standard.make_polar");
      define_proc ("real-part", "kawa.standard.real_part");
      define_proc ("imag-part", "kawa.standard.imag_part");
      define_proc ("magnitude", "kawa.standard.abs");
      define_proc ("angle", "kawa.standard.angle");
      define_proc ("exact->inexact", "kawa.standard.exact2inexact");
      define_proc ("inexact->exact", "kawa.standard.inexact2exact");
      define_proc ("number->string", "kawa.standard.number2string");
      define_proc ("string->number", "kawa.standard.string2number");

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
      define_proc ("char-numeric?", "kawa.lib.characters");
      define_proc ("char-whitespace?", "kawa.lib.characters");
      define_proc ("char-upper-case?", "kawa.lib.characters");
      define_proc ("char-lower-case?", "kawa.lib.characters");
      define_proc ("char->integer", "kawa.lib.characters");
      define_proc ("integer->char", "kawa.lib.characters");
      define_proc ("char-upcase", "kawa.lib.characters");
      define_proc ("char-downcase", "kawa.lib.characters");
      
      //-- Section 6.7  -- complete
      define_proc ("string?", "kawa.standard.string_p");
      define_proc ("make-string", "kawa.standard.make_string");
      define_proc ("string", "kawa.standard.string_v");
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
      define ("vector", kawa.standard.vector_v.vectorProcedure);
      define_proc ("vector-length", "kawa.lib.vectors");
      define_proc ("vector-ref", "kawa.lib.vectors");
      define_proc ("vector-set!", "kawa.lib.vectors");
      define_proc ("list->vector", "kawa.lib.vectors");
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

      //-- Section 6.10 [complete except for transcript-on/off]
      define_proc ("call-with-input-file",
		   "kawa.standard.call_with_input_file");
      define_proc ("call-with-output-file",
		   "kawa.standard.call_with_output_file");
      define_proc ("input-port?", "kawa.standard.input_port_p");
      define_proc ("output-port?", "kawa.standard.output_port_p");
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
      define_proc ("char-ready?", "kawa.standard.char_ready_p");
      define_proc (new write(true));       // write
      define_proc (new write(false));      // display
      define_proc ("write-char", "kawa.standard.writechar");
      define_proc ("newline", "kawa.standard.newline");
      define_proc ("load", "kawa.standard.load");
      define_proc ("call-with-input-string",  // Extension
		   "kawa.standard.call_with_input_string");
      define_proc ("call-with-output-string",  // Extension
		   "kawa.standard.call_with_output_string");

      define_syntax ("%syntax-error", "kawa.standard.syntax_error");

      r5_environment = new Environment (r4_environment);
      r5_environment.setName ("r5rs-environment");
      env = r5_environment;
      define_proc ("values", "kawa.standard.values_v");
      define_proc ("call-with-values", "kawa.standard.call_with_values");
      define_proc ("eval", "kawa.lang.Eval");
      define_proc ("scheme-report-environment", "kawa.standard.scheme_env");
      define_proc ("null-environment", "kawa.standard.null_env");
      define_proc ("interaction-environment", "kawa.standard.user_env");

      user_environment = new Environment (r5_environment);
      user_environment.setName ("interaction-environment");
      env = user_environment;
      define_proc ("exit", "kawa.standard.exit");

      Symbol sym = Symbol.make ("arithmetic-shift");
      proc = new AutoloadProcedure (sym, "kawa.standard.ashift");
      define (sym, proc);
      define ("ash", proc);
      define_proc ("logand", "kawa.standard.logand");
      define_proc ("logior", "kawa.standard.logior");
      define_proc ("logxor", "kawa.standard.logxor");
      define_proc ("lognot", "kawa.standard.lognot");
      define_proc ("logop", "kawa.standard.logop");
      define_proc ("logbit?", "kawa.standard.logbit_p");
      define_proc ("logtest", "kawa.standard.logtest");
      define_proc ("logcount", "kawa.standard.logcount");
      define_proc ("bit-extract", "kawa.standard.bit_extract");
      define_proc ("integer-length", "kawa.standard.int_length");

      define ("primitive-virtual-method",new kawa.standard.prim_method(false));
      define ("primitive-static-method", new kawa.standard.prim_method(true));
      define ("primitive-op1", new kawa.standard.prim_method());

      //-- (when cond exp ...)
      define_syntax ("when", "kawa.lib.when_unless");
      //-- (unless cond exp ...)
      define_syntax ("unless", "kawa.lib.when_unless");

      define_proc ("compile-file", "kawa.lang.CompileFile");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");

      define_proc ("quantity->number", "kawa.standard.quantity2number");
      define_proc ("quantity->unit", "kawa.standard.quantity2unit");
      define_proc ("make-quantity", "kawa.standard.make_quantity");
      define_syntax ("define-unit", "kawa.lib.when_unless");

      define_syntax ("future", "kawa.lib.thread");
      define_proc ("%make-future", "kawa.standard.make_future");
      define_proc ("sleep", "kawa.standard.sleep");
   }

  /** Evalutate Scheme expressions from string.
   * @param string the string constaining Scheme expressions
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Interpreter.voidObject if none. */
  public static Object eval (String string, Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return eval (call_with_input_string.open_input_string (string), env);
  }

  /** Evalutate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Interpreter.voidObject if none. */
  public static Object eval (InPort port, Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Translator tr = new Translator (env);
    return Eval.eval (CompileFile.read (port, tr), tr, env);
  }

  /** Evalutate Scheme expressions from an "S expression."
   * @param sexpr the S expression to evaluate
   * @param env the Environment to evaluate the string in
   * @return result of the expression. */
  public static Object eval (Object sexpr, Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    return Eval.eval (sexpr, env);
  }

}
