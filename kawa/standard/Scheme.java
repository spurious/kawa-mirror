package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.bytecode.ArrayType;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.Hashtable;
import gnu.text.SourceMessages;
import gnu.kawa.lispexpr.*;
import gnu.lists.AbstractFormat;
import gnu.kawa.functions.DisplayFormat;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.reflect.ClassMethods;
import gnu.math.Unit;

public class Scheme extends LispLanguage
{

  protected void define_proc (Named proc)
  {
    define (proc.getName (), proc);
  }

  protected void define_proc (String name, Named proc)
  {
    if (proc.getName() == null)
      proc.setName(name);
    define(name, proc);
  }

  /** Define a procedure to be autoloaded. */
  protected void define_proc (String name, String className)
  {
    define (name, new AutoloadProcedure (name, className, this));
  }

  public static final Environment nullEnvironment;
  public static final Environment r4Environment;
  public static final Environment r5Environment;
  protected static final SimpleEnvironment kawaEnvironment;

  public static LangPrimType booleanType;
  static final Scheme instance;

  public static final gnu.kawa.reflect.InstanceOf instanceOf;
  public static final not not;
  public static final kawa.standard.map map;
  public static final kawa.standard.map forEach;
  public static final gnu.kawa.functions.IsEq isEq;
  public static final gnu.kawa.functions.IsEqv isEqv;
  public static final gnu.kawa.functions.IsEqual isEqual;
  public static final kawa.repl repl;

  public static final NumberCompare numEqu;
  public static final NumberCompare numGrt;
  public static final NumberCompare numGEq;
  public static final NumberCompare numLss;
  public static final NumberCompare numLEq;

  static {
    // (null-environment)
    nullEnvironment = Environment.make("null-environment");
    r4Environment = Environment.make("r4rs-environment", nullEnvironment);
    r5Environment = Environment.make("r5rs-environment", r4Environment);
    kawaEnvironment = Environment.make("kawa-environment", r5Environment);

    instance = new Scheme(kawaEnvironment);
    instanceOf = new gnu.kawa.reflect.InstanceOf(instance, "instance?");
    not = new not(instance, "not");
    map = new map(true);
    forEach = new map(false);
    isEq = new gnu.kawa.functions.IsEq(instance, "eq?");
    isEqv = new gnu.kawa.functions.IsEqv(instance, "eqv?", isEq);
    isEqual = new gnu.kawa.functions.IsEqual(instance, "equal?");
    numEqu = NumberCompare.make(instance, "=",
                                NumberCompare.TRUE_IF_EQU);
    numGrt = NumberCompare.make(instance, ">",
                                NumberCompare.TRUE_IF_GRT);
    numGEq = NumberCompare.make(instance, ">=",
                                NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU);
    numLss = NumberCompare.make(instance, "<",
                                NumberCompare.TRUE_IF_LSS);
    numLEq = NumberCompare.make(instance, "<=",
                                NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU);
    repl = new kawa.repl(instance);
    instance.initScheme();
  }

  public static Scheme getInstance()
  {
    return instance;
  }

  public static Environment builtin ()
  {
    return kawaEnvironment;
  }

  public static final Lambda lambda = new kawa.lang.Lambda();
  static { lambda.setKeywords(Special.optional, Special.rest, Special.key); }

  private void initScheme ()
  {
      environ = nullEnvironment;

      defSntxStFld("lambda", "kawa.standard.Scheme", "lambda");

      //-- Section 4.1  -- complete
      defSntxStFld(LispLanguage.quote_sym, "kawa.lang.Quote", "plainQuote");
      defSntxStFld("%define", "kawa.standard.define", "defineRaw");
      defSntxStFld("define", "kawa.lib.prim_syntax");

      defSntxStFld("if", "kawa.lib.prim_syntax");
      defSntxStFld ("set!", "kawa.standard.set_b", "set");

      // Section 4.2  -- complete
      defSntxStFld("cond", "kawa.lib.std_syntax");
      defSntxStFld("case", "kawa.lib.std_syntax");
      defSntxStFld("and", "kawa.lib.std_syntax");
      defSntxStFld("or", "kawa.lib.std_syntax");
      defSntxStFld("%let", "kawa.standard.let", "let");
      defSntxStFld("let", "kawa.lib.std_syntax");
      defSntxStFld("let*", "kawa.lib.std_syntax");
      defSntxStFld("letrec", "kawa.lib.std_syntax");

      defSntxStFld("begin", "kawa.standard.begin", "begin");
      defSntxStFld("do", "kawa.lib.std_syntax");
      defSntxStFld("delay", "kawa.lib.std_syntax");
      defProcStFld("%make-promise", "kawa.lib.std_syntax");
      defSntxStFld("quasiquote", "kawa.lang.Quote", "quasiQuote");

      //-- Section 5  -- complete [except for internal definitions]

      // Appendix (and R5RS)
      defSntxStFld("define-syntax", "kawa.lib.prim_syntax");
      defSntxStFld("let-syntax", "kawa.standard.let_syntax", "let_syntax");
      defSntxStFld("letrec-syntax", "kawa.standard.let_syntax",
                   "letrec_syntax");
      defSntxStFld("syntax-rules", "kawa.standard.syntax_rules",
                   "syntax_rules");

      nullEnvironment.setLocked();
      environ = r4Environment;

      //-- Section 6.1  -- complete
      defProcStFld("not", "kawa.standard.Scheme");
      defProcStFld("boolean?", "kawa.lib.misc");

      //-- Section 6.2  -- complete
      defProcStFld("eq?", "kawa.standard.Scheme", "isEq");
      defProcStFld("eqv?", "kawa.standard.Scheme", "isEqv");
      defProcStFld("equal?", "kawa.standard.Scheme", "isEqual");

      //-- Section 6.3  -- complete
      defProcStFld("pair?", "kawa.lib.lists");
      defProcStFld("cons", "kawa.lib.lists");
      defProcStFld("car", "kawa.lib.lists");
      defProcStFld("cdr", "kawa.lib.lists");
      defProcStFld("set-car!", "kawa.lib.lists");
      defProcStFld("set-cdr!", "kawa.lib.lists");

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
      defProcStFld("null?", "kawa.lib.lists");
      defProcStFld("list?", "kawa.lib.lists");
      defProcStFld("list", "gnu.kawa.functions.MakeList");
      defProcStFld("length", "kawa.lib.lists");
      defProcStFld("append", "kawa.standard.append", "append");
      defProcStFld("reverse", "kawa.lib.lists");
      defProcStFld("reverse!", "kawa.lib.lists");  // Not R5RS.
      defProcStFld("list-tail", "kawa.lib.lists");
      defProcStFld("list-ref", "kawa.lib.lists");

      defProcStFld("memq", "kawa.lib.lists");
      defProcStFld("memv", "kawa.lib.lists");
      defProcStFld("member", "kawa.lib.lists");
      defProcStFld("assq", "kawa.lib.lists");
      defProcStFld("assv", "kawa.lib.lists");
      defProcStFld("assoc", "kawa.lib.lists");

      //-- Section 6.4  -- complete, including slashified read/write
      
      defProcStFld("symbol?", "kawa.lib.misc");
      defProcStFld("symbol->string", "kawa.lib.misc");
      defProcStFld("string->symbol", "kawa.lib.misc");

      //-- Section 6.5
      defProcStFld("number?", "kawa.lib.numbers");
      defProcStFld("quantity?", "kawa.lib.numbers");
      defProcStFld("complex?", "kawa.lib.numbers");
      defProcStFld("real?", "kawa.lib.numbers");
      defProcStFld("rational?", "kawa.lib.numbers");
      define_proc ("integer?", "kawa.standard.integer_p");
      defProcStFld("exact?", "kawa.lib.numbers");
      defProcStFld("inexact?", "kawa.lib.numbers");
      defProcStFld("=", "kawa.standard.Scheme", "numEqu");
      defProcStFld("<", "kawa.standard.Scheme", "numLss");
      defProcStFld(">", "kawa.standard.Scheme", "numGrt");
      defProcStFld("<=", "kawa.standard.Scheme", "numLEq");
      defProcStFld(">=", "kawa.standard.Scheme", "numGEq");
      defProcStFld("zero?", "kawa.lib.numbers");
      defProcStFld("positive?", "kawa.lib.numbers");
      defProcStFld("negative?", "kawa.lib.numbers");
      defProcStFld("odd?", "kawa.lib.numbers");
      defProcStFld("even?", "kawa.lib.numbers");
      define_proc ("max", "kawa.standard.max");
      define_proc ("min", "kawa.standard.min");
      defProcStFld("+", "gnu.kawa.functions.AddOp", "$Pl");
      defProcStFld("-", "gnu.kawa.functions.AddOp", "$Mn");
      defProcStFld("*", "gnu.kawa.functions.MultiplyOp", "$St");
      defProcStFld("/", "gnu.kawa.functions.DivideOp", "$Sl");
      defProcStFld("abs", "kawa.lib.numbers");
      defProcStFld("quotient", "kawa.lib.numbers");
      defProcStFld("remainder", "kawa.lib.numbers");
      defProcStFld("modulo", "kawa.lib.numbers");
      define_proc ("gcd", "kawa.standard.gcd");
      define_proc ("lcm", "kawa.standard.lcm");
      defProcStFld("numerator", "kawa.lib.numbers");
      defProcStFld("denominator", "kawa.lib.numbers");
      defProcStFld("floor", "kawa.lib.numbers");
      defProcStFld("ceiling", "kawa.lib.numbers");
      defProcStFld("truncate", "kawa.lib.numbers");
      defProcStFld("round", "kawa.lib.numbers");
      defProcStFld("rationalize", "kawa.lib.numbers");
      defProcStFld("exp", "kawa.lib.numbers");
      defProcStFld("log", "kawa.lib.numbers");
      defProcStFld("sin", "kawa.lib.numbers");
      defProcStFld("cos", "kawa.lib.numbers");
      defProcStFld("tan", "kawa.lib.numbers");
      defProcStFld("asin", "kawa.lib.numbers");
      defProcStFld("acos", "kawa.lib.numbers");
      define_proc ("atan", "kawa.standard.atan");
      defProcStFld("sqrt", "kawa.lib.numbers");
      defProcStFld("expt", "kawa.standard.expt");
      defProcStFld("make-rectangular", "kawa.lib.numbers");
      defProcStFld("make-polar", "kawa.lib.numbers");
      defProcStFld("real-part", "kawa.lib.numbers");
      defProcStFld("imag-part", "kawa.lib.numbers");
      defProcStFld("magnitude", "kawa.lib.numbers");
      defProcStFld("angle", "kawa.lib.numbers");
      defProcStFld("exact->inexact", "kawa.lib.numbers");
      defProcStFld("inexact->exact", "kawa.lib.numbers");
      defProcStFld("number->string", "kawa.lib.numbers");
      define_proc ("string->number", "kawa.standard.string2number");

      //-- Section 6.6  -- complete
      defProcStFld("char?", "kawa.lib.characters");
      defProcStFld("char=?", "kawa.lib.characters");
      defProcStFld("char<?", "kawa.lib.characters");
      defProcStFld("char>?", "kawa.lib.characters");
      defProcStFld("char<=?", "kawa.lib.characters");
      defProcStFld("char>=?", "kawa.lib.characters");
      defProcStFld("char-ci=?", "kawa.lib.characters");
      defProcStFld("char-ci<?", "kawa.lib.characters");
      defProcStFld("char-ci>?", "kawa.lib.characters");
      defProcStFld("char-ci<=?", "kawa.lib.characters");
      defProcStFld("char-ci>=?", "kawa.lib.characters");
      defProcStFld("char-alphabetic?", "kawa.lib.characters");
      defProcStFld("char-numeric?", "kawa.lib.characters");
      defProcStFld("char-whitespace?", "kawa.lib.characters");
      defProcStFld("char-upper-case?", "kawa.lib.characters");
      defProcStFld("char-lower-case?", "kawa.lib.characters");
      defProcStFld("char->integer", "kawa.lib.characters");
      defProcStFld("integer->char", "kawa.lib.characters");
      defProcStFld("char-upcase", "kawa.lib.characters");
      defProcStFld("char-downcase", "kawa.lib.characters");
      
      //-- Section 6.7  -- complete
      defProcStFld("string?", "kawa.lib.strings");
      defProcStFld("make-string", "kawa.lib.strings");
      define_proc ("string", "kawa.standard.string_v");
      defProcStFld("string-length", "kawa.lib.strings");
      defProcStFld("string-ref", "kawa.lib.strings");
      defProcStFld("string-set!", "kawa.lib.strings");

      defProcStFld("string=?", "kawa.lib.strings");
      defProcStFld("string-ci=?", "kawa.lib.strings");
      defProcStFld("string<?", "kawa.lib.strings");
      defProcStFld("string>?", "kawa.lib.strings");
      defProcStFld("string<=?", "kawa.lib.strings");
      defProcStFld("string>=?", "kawa.lib.strings");

      defProcStFld("string-ci<?", "kawa.lib.strings");
      defProcStFld("string-ci>?", "kawa.lib.strings");
      defProcStFld("string-ci<=?", "kawa.lib.strings");
      defProcStFld("string-ci>=?", "kawa.lib.strings");

      defProcStFld("substring", "kawa.lib.strings");
      defProcStFld("string-append", "kawa.lib.strings");
      defProcStFld("string-append/shared", "kawa.lib.strings");
      defProcStFld("string->list", "kawa.lib.strings");
      defProcStFld("list->string", "kawa.lib.strings");
      defProcStFld("string-copy", "kawa.lib.strings");
      defProcStFld("string-fill!", "kawa.lib.strings");

      //-- Section 6.8  -- complete
      defProcStFld("vector?", "kawa.lib.vectors");
      defProcStFld("make-vector", "kawa.lib.vectors");
      defProcStFld("vector", "kawa.lib.vectors");
      defProcStFld("vector-length", "kawa.lib.vectors");
      defProcStFld("vector-ref", "kawa.lib.vectors");
      defProcStFld("vector-set!", "kawa.lib.vectors");
      defProcStFld("list->vector", "kawa.lib.vectors");
      defProcStFld("vector->list", "kawa.lib.vectors");
      defProcStFld("vector-fill!", "kawa.lib.vectors");
      // Extension:
      defProcStFld("vector-append", "kawa.standard.vector_append", "vectorAppend");
      defProcStFld("values-append", "gnu.kawa.functions.AppendValues",
		   "appendValues");

      //-- Section 6.9  -- complete [except restricted call/cc]
      defProcStFld("procedure?", "kawa.lib.misc");
      defProcStFld("apply", "gnu.kawa.functions.Apply", "apply");
      defProcStFld("map", "kawa.standard.Scheme", "map");
      defProcStFld("for-each", "kawa.standard.Scheme", "forEach");
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");
      define_proc ("call/cc", "kawa.standard.callcc");
      defProcStFld("force", "kawa.lib.misc");

      //-- Section 6.10  -- complete
      defProcStFld("call-with-input-file", "kawa.lib.ports");
      defProcStFld("call-with-output-file", "kawa.lib.ports");
      defProcStFld("input-port?", "kawa.lib.ports");
      defProcStFld("output-port?", "kawa.lib.ports");
      defProcStFld("current-input-port", "kawa.lib.ports");
      defProcStFld("current-output-port", "kawa.lib.ports");
      defProcStFld("with-input-from-file", "kawa.lib.ports");
      defProcStFld("with-output-to-file", "kawa.lib.ports");
      defProcStFld("open-input-file", "kawa.lib.ports");
      defProcStFld("open-output-file", "kawa.lib.ports");
      defProcStFld("close-input-port", "kawa.lib.ports");
      defProcStFld("close-output-port", "kawa.lib.ports");
      define_proc ("read", "kawa.standard.read");
      define_proc ("read-line", "kawa.standard.read_line");
      defProcStFld("read-char", "kawa.standard.readchar", "readChar");
      defProcStFld("peek-char", "kawa.standard.readchar", "peekChar");
      defProcStFld("eof-object?", "kawa.lib.ports");
      defProcStFld("char-ready?", "kawa.lib.ports");
      defProcStFld("write", "kawa.lib.ports");
      defProcStFld("display", "kawa.lib.ports");
      defProcStFld("print-as-xml", "gnu.xquery.lang.XQuery", "writeFormat");
      defProcStFld("write-char", "kawa.lib.ports");
      defProcStFld("newline", "kawa.lib.ports");
      defProcStFld("load", "kawa.standard.load", "load");
      defProcStFld("load-relative", "kawa.standard.load", "loadRelative");
      defProcStFld("transcript-off", "kawa.lib.ports");
      defProcStFld("transcript-on", "kawa.lib.ports");
      defProcStFld("call-with-input-string", "kawa.lib.ports");  // Extension
      defProcStFld("open-input-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("open-output-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("get-output-string", "kawa.lib.ports");  // SRFI-6
      defProcStFld("call-with-output-string", "kawa.lib.ports"); // Extension
      defProcStFld("force-output", "kawa.lib.ports");  // Extension

      defProcStFld("port-line", "kawa.lib.ports");
      defProcStFld("set-port-line!", "kawa.lib.ports");
      defProcStFld("port-column", "kawa.lib.ports");
      defProcStFld("current-error-port", "kawa.lib.ports");
      defProcStFld("input-port-line-number", "kawa.lib.ports");  // Extension
      defProcStFld("set-input-port-line-number!", "kawa.lib.ports");
      defProcStFld("input-port-column-number", "kawa.lib.ports");
      defProcStFld("input-port-read-state", "kawa.lib.ports");
      defProcStFld("default-prompter", "kawa.lib.ports");
      defProcStFld("input-port-prompter", "kawa.lib.ports");
      defProcStFld("set-input-port-prompter!", "kawa.lib.ports");
      defProcStFld("base-uri", "kawa.lib.misc");

      defProcStFld("%syntax-error", "kawa.standard.syntax_error",
                   "syntax_error");
      defProcStFld("syntax-error", "kawa.lib.prim_syntax");

      r4Environment.setLocked();
      environ = r5Environment;

      defProcStFld("values", "kawa.lib.misc");
      defProcStFld("call-with-values", "kawa.standard.call_with_values",
		   "callWithValues");
      defSntxStFld("let-values", "kawa.lib.syntax");
      defSntxStFld("let*-values", "kawa.lib.syntax");
      defSntxStFld("case-lambda", "kawa.lib.syntax");
      defSntxStFld("receive", "kawa.lib.syntax");
      defProcStFld("eval", "kawa.lang.Eval");
      defProcStFld("repl", "kawa.standard.Scheme", "repl");
      defProcStFld("scheme-report-environment", "kawa.lib.misc");
      defProcStFld("null-environment", "kawa.lib.misc");
      defProcStFld("interaction-environment", "kawa.lib.misc");
      defProcStFld("dynamic-wind", "kawa.lib.misc");

      r5Environment.setLocked();
      environ = kawaEnvironment;
 
      defSntxStFld("define-private", "kawa.lib.prim_syntax");
      defSntxStFld("define-constant", "kawa.lib.prim_syntax");

      defSntxStFld("define-autoload",
                   "kawa.standard.define_autoload", "define_autoload");
      defSntxStFld("define-autoloads-from-file",
                   "kawa.standard.define_autoload",
                   "define_autoloads_from_file");

      defProcStFld("exit", "kawa.lib.thread");

      defProcStFld("arithmetic-shift", "kawa.lib.numbers");
      defProcStFld("ash", "kawa.lib.numbers", "arithmetic$Mnshift");
      define_proc ("logand", "kawa.standard.logand");
      define_proc ("logior", "kawa.standard.logior");
      define_proc ("logxor", "kawa.standard.logxor");
      defProcStFld("lognot", "kawa.lib.numbers");
      defProcStFld("logop", "kawa.lib.numbers");
      defProcStFld("logbit?", "kawa.lib.numbers");
      defProcStFld("logtest", "kawa.lib.numbers");
      defProcStFld("logcount", "kawa.lib.numbers");
      defProcStFld("bit-extract", "kawa.lib.numbers");
      defProcStFld("integer-length", "kawa.lib.numbers");

      // These are from SLIB.
      defProcStFld("string-upcase!", "kawa.lib.strings");
      defProcStFld("string-downcase!", "kawa.lib.strings");
      defProcStFld("string-capitalize!", "kawa.lib.strings");
      defProcStFld("string-upcase", "kawa.lib.strings");
      defProcStFld("string-downcase", "kawa.lib.strings");
      defProcStFld("string-capitalize", "kawa.lib.strings");
      defSntxStFld("primitive-virtual-method",
                   "kawa.standard.prim_method", "virtual_method");
      defSntxStFld("primitive-static-method",
                   "kawa.standard.prim_method", "static_method");
      defSntxStFld("primitive-interface-method",
                   "kawa.standard.prim_method", "interface_method");
      defSntxStFld("primitive-constructor", "kawa.lib.reflection");
      defSntxStFld("primitive-op1",
                   "kawa.standard.prim_method", "op1");
      defSntxStFld("primitive-get-field", "kawa.lib.reflection");
      defSntxStFld("primitive-set-field", "kawa.lib.reflection");
      defSntxStFld("primitive-get-static", "kawa.lib.reflection");
      defSntxStFld("primitive-set-static", "kawa.lib.reflection");
      defSntxStFld("primitive-array-new", "kawa.lib.reflection");
      defSntxStFld("primitive-array-get", "kawa.lib.reflection");
      defSntxStFld("primitive-array-set", "kawa.lib.reflection");
      defSntxStFld("primitive-array-length", "kawa.lib.reflection");
      defProcStFld("subtype?", "kawa.lib.reflection");
      defProcStFld("primitive-throw", "kawa.standard.prim_throw", "primitiveThrow");
      defSntxStFld("try-finally", "kawa.lib.syntax");
      defSntxStFld("try-catch", "kawa.lib.prim_syntax");
      defProcStFld("throw", "kawa.standard.throw_name", "throwName");
      defProcStFld("catch", "kawa.lib.syntax");
      defProcStFld("error", "kawa.lib.misc");
      defProcStFld("as", "gnu.kawa.functions.Convert", "as");
      defProcStFld("instance?", "kawa.standard.Scheme", "instanceOf");
      defSntxStFld("synchronized", "kawa.lib.syntax");
      defSntxStFld("object", "kawa.standard.object", "objectSyntax");
      defSntxStFld("define-class", "kawa.standard.define_class",
                   "define_class");
      defSntxStFld("define-simple-class", "kawa.standard.define_class",
                   "define_simple_class");
      defSntxStFld("this", "kawa.standard.thisRef", "thisSyntax");
      defProcStFld("make", "gnu.kawa.reflect.Invoke", "make");
      defProcStFld("slot-ref", "gnu.kawa.reflect.SlotGet", "field");
      defProcStFld("slot-set!", "gnu.kawa.reflect.SlotSet", "set$Mnfield$Ex");
      defProcStFld("field", "gnu.kawa.reflect.SlotGet");
      defProcStFld("class-methods", "gnu.kawa.reflect.ClassMethods",
		   "classMethods");
      defProcStFld("static-field", "gnu.kawa.reflect.SlotGet",
		   "staticField");
      defProcStFld("invoke", "gnu.kawa.reflect.Invoke", "invoke");
      defProcStFld("$lookup$", "gnu.kawa.functions.GetNamedPart",
                     "getNamedPart");

      defProcStFld("invoke-static", "gnu.kawa.reflect.Invoke", "invokeStatic");
      defProcStFld("invoke-special", "gnu.kawa.reflect.Invoke", "invokeSpecial");

      defSntxStFld("define-macro", "kawa.lib.syntax");
      defSntxStFld("%define-macro", "kawa.standard.define_syntax",
                   "define_macro");
      defSntxStFld("syntax-case", "kawa.standard.syntax_case", "syntax_case");
      defSntxStFld("%define-syntax", "kawa.standard.define_syntax",
                   "define_syntax");
      defSntxStFld("syntax", "kawa.standard.syntax", "syntax");
      defSntxStFld("quasisyntax", "kawa.standard.syntax", "quasiSyntax");
      defProcStFld("syntax-object->datum", "kawa.lib.std_syntax");
      defProcStFld("datum->syntax-object", "kawa.lib.std_syntax");
      defProcStFld("syntax->expression", "kawa.lib.prim_syntax");
      defProcStFld("syntax-body->expression", "kawa.lib.prim_syntax");
      defProcStFld("generate-temporaries", "kawa.lib.std_syntax");
      defSntxStFld("with-syntax", "kawa.lib.std_syntax");
      defProcStFld("syntax-source", "kawa.lib.syntax");
      defProcStFld("syntax-line", "kawa.lib.syntax");
      defProcStFld("syntax-column", "kawa.lib.syntax");
      defSntxStFld("define-for-syntax", "kawa.lib.prim_syntax");
      defSntxStFld("include", "kawa.lib.files");
      defSntxStFld("include-relative", "kawa.lib.files");

      defProcStFld("file-exists?", "kawa.lib.files");
      defProcStFld("file-directory?", "kawa.lib.files");
      defProcStFld("file-readable?", "kawa.lib.files");
      defProcStFld("file-writable?", "kawa.lib.files");
      defProcStFld("delete-file", "kawa.lib.files");
      defProcStFld("system-tmpdir", "kawa.lib.files");
      defProcStFld("make-temporary-file", "kawa.lib.files");
      defProcStFld("rename-file", "kawa.lib.files");
      defProcStFld("copy-file", "kawa.lib.files");
      defProcStFld("create-directory", "kawa.lib.files");
      defProcStFld("->pathname", "kawa.lib.files");
      define("port-char-encoding", Boolean.TRUE);
      define("symbol-read-case", "P");

      defProcStFld("system", "kawa.lib.system");
      defProcStFld("make-process", "kawa.lib.system");
      defProcStFld("tokenize-string-to-string-array", "kawa.lib.system");
      defProcStFld("tokenize-string-using-shell", "kawa.lib.system");
      defProcStFld("command-parse", "kawa.lib.system");
      
      defProcStFld("record-accessor", "kawa.lib.reflection");
      defProcStFld("record-modifier", "kawa.lib.reflection");
      defProcStFld("record-predicate", "kawa.lib.reflection");
      defProcStFld("record-constructor", "kawa.lib.reflection");
      defProcStFld("make-record-type", "kawa.lib.reflection");
      defProcStFld("record-type-descriptor", "kawa.lib.reflection");
      defProcStFld("record-type-name", "kawa.lib.reflection");
      defProcStFld("record-type-field-names", "kawa.lib.reflection");
      defProcStFld("record?", "kawa.lib.reflection");
      defSntxStFld("define-record-type", "gnu.kawa.slib.DefineRecordType");

      defSntxStFld("when", "kawa.lib.syntax"); //-- (when cond exp ...)
      defSntxStFld("unless", "kawa.lib.syntax"); //-- (unless cond exp ...)
      defSntxStFld("fluid-let", "kawa.standard.fluid_let", "fluid_let");
      defSntxStFld("constant-fold", "kawa.standard.constant_fold",
                   "constant_fold");
      defProcStFld("make-parameter", "kawa.lib.parameters");
      defSntxStFld("parameterize", "kawa.lib.parameters");

      defProcStFld("compile-file", "kawa.lib.system");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
      defProcStFld("environment-bound?", "kawa.lib.misc");
      defProcStFld("scheme-implementation-version", "kawa.lib.misc");
      defProcStFld("scheme-window", "kawa.lib.windows");
      defSntxStFld("define-procedure", "kawa.lib.syntax");
      defProcStFld("add-procedure-properties", "kawa.lib.syntax");
      defProcStFld("make-procedure",
                   "gnu.kawa.functions.MakeProcedure", "makeProcedure");
      defProcStFld("procedure-property", "kawa.lib.misc");
      defProcStFld("set-procedure-property!", "kawa.lib.misc");
      defSntxStFld("provide", "kawa.lib.misc");
      defSntxStFld("test-begin", "kawa.lib.misc");
      defProcStFld("namespace", "kawa.lib.misc");

      define_proc ("quantity->number", "kawa.standard.quantity2number");
      define_proc ("quantity->unit", "kawa.standard.quantity2unit");
      define_proc ("make-quantity", "kawa.standard.make_quantity");
      defSntxStFld("define-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_namespace");
      defSntxStFld("define-xml-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_xml_namespace");
      defSntxStFld("define-private-namespace", "gnu.kawa.lispexpr.DefineNamespace",
                   "define_private_namespace");
      defSntxStFld("define-unit", "kawa.standard.define_unit", "define_unit");
      defSntxStFld("define-base-unit", "kawa.standard.define_unit",
                   "define_base_unit");

      defProcStFld("gentemp", "kawa.lib.syntax");
      defSntxStFld("defmacro", "kawa.lib.syntax");
      defProcStFld("setter", "gnu.kawa.functions.Setter", "setter");
      defSntxStFld("resource-uri", "kawa.lib.files");

      defProcStFld("URI", "kawa.lib.files");
      defProcStFld("resolve-uri", "kawa.lib.files");
      defSntxStFld("module-uri", "kawa.lib.files");

      defSntxStFld("future", "kawa.lib.thread");
      defProcStFld("sleep", "kawa.lib.thread");
      defProcStFld("runnable", "kawa.lib.thread");

      defSntxStFld("trace", "kawa.lib.trace");
      defSntxStFld("untrace", "kawa.lib.trace");

      defProcStFld("format", "gnu.kawa.functions.Format");
      defProcStFld("parse-format", "gnu.kawa.functions.ParseFormat", "parseFormat");

      defProcStFld("make-element", "gnu.kawa.xml.MakeElement", "makeElement");
      defProcStFld("make-attribute", "gnu.kawa.xml.MakeAttribute", "makeAttribute");
      defProcStFld("map-values", "gnu.kawa.functions.ValuesMap", "valuesMap");
      defProcStFld("children", "gnu.kawa.xml.Children", "children");
      defProcStFld("attributes", "gnu.kawa.xml.Attributes");
      defProcStFld("unescaped-data", "gnu.kawa.xml.MakeUnescapedData",
		   "unescapedData");
      defProcStFld("keyword?", "kawa.lib.keywords");
      defProcStFld("keyword->string", "kawa.lib.keywords");
      defProcStFld("string->keyword", "kawa.lib.keywords");
      defSntxStFld("location", "kawa.standard.location", "location");
      defSntxStFld("define-alias", "kawa.standard.define_alias",
                   "define_alias");
      defSntxStFld("define-variable", "kawa.standard.define_variable",
                   "define_variable");
      defSntxStFld("define-member-alias", "kawa.standard.define_member_alias",
                   "define_member_alias");
      defSntxStFld("require", "kawa.standard.require", "require");
      defSntxStFld("module-name", "kawa.standard.module_name",
                   "module_name");
      defSntxStFld("module-extends", "kawa.standard.module_extends",
                   "module_extends");
      defSntxStFld("module-implements", "kawa.standard.module_implements",
                   "module_implements");
      defSntxStFld("module-static", "kawa.standard.module_static",
                   "module_static");
      defSntxStFld("module-export", "kawa.standard.export", "module_export");
      defSntxStFld("module-compile-options",
                   "kawa.standard.module_compile_options",
                   "module_compile_options");
      defSntxStFld("with-compile-options",
                   "kawa.standard.with_compile_options",
                   "with_compile_options");

      defProcStFld("array?", "kawa.lib.arrays");
      defProcStFld("array-rank", "kawa.lib.arrays");
      defProcStFld("make-array", "kawa.lib.arrays");
      defProcStFld("array", "kawa.lib.arrays");
      defProcStFld("array-start", "kawa.lib.arrays");
      defProcStFld("array-end", "kawa.lib.arrays");
      defProcStFld("shape", "kawa.lib.arrays");
      defProcStFld("array-ref", "gnu.kawa.functions.ArrayRef", "arrayRef");
      defProcStFld("array-set!", "gnu.kawa.functions.ArraySet", "arraySet");
      defProcStFld("share-array", "kawa.lib.arrays");

      defProcStFld("s8vector?", "kawa.lib.uniform");
      defProcStFld("make-s8vector", "kawa.lib.uniform");
      defProcStFld("s8vector", "kawa.lib.uniform");
      defProcStFld("s8vector-length", "kawa.lib.uniform");
      defProcStFld("s8vector-ref", "kawa.lib.uniform");
      defProcStFld("s8vector-set!", "kawa.lib.uniform");
      defProcStFld("s8vector->list", "kawa.lib.uniform");
      defProcStFld("list->s8vector", "kawa.lib.uniform");
      defProcStFld("u8vector?", "kawa.lib.uniform");
      defProcStFld("make-u8vector", "kawa.lib.uniform");
      defProcStFld("u8vector", "kawa.lib.uniform");
      defProcStFld("u8vector-length", "kawa.lib.uniform");
      defProcStFld("u8vector-ref", "kawa.lib.uniform");
      defProcStFld("u8vector-set!", "kawa.lib.uniform");
      defProcStFld("u8vector->list", "kawa.lib.uniform");
      defProcStFld("list->u8vector", "kawa.lib.uniform");

      defProcStFld("s16vector?", "kawa.lib.uniform");
      defProcStFld("make-s16vector", "kawa.lib.uniform");
      defProcStFld("s16vector", "kawa.lib.uniform");
      defProcStFld("s16vector-length", "kawa.lib.uniform");
      defProcStFld("s16vector-ref", "kawa.lib.uniform");
      defProcStFld("s16vector-set!", "kawa.lib.uniform");
      defProcStFld("s16vector->list", "kawa.lib.uniform");
      defProcStFld("list->s16vector", "kawa.lib.uniform");
      defProcStFld("u16vector?", "kawa.lib.uniform");
      defProcStFld("make-u16vector", "kawa.lib.uniform");
      defProcStFld("u16vector", "kawa.lib.uniform");
      defProcStFld("u16vector-length", "kawa.lib.uniform");
      defProcStFld("u16vector-ref", "kawa.lib.uniform");
      defProcStFld("u16vector-set!", "kawa.lib.uniform");
      defProcStFld("u16vector->list", "kawa.lib.uniform");
      defProcStFld("list->u16vector", "kawa.lib.uniform");

      defProcStFld("s32vector?", "kawa.lib.uniform");
      defProcStFld("make-s32vector", "kawa.lib.uniform");
      defProcStFld("s32vector", "kawa.lib.uniform");
      defProcStFld("s32vector-length", "kawa.lib.uniform");
      defProcStFld("s32vector-ref", "kawa.lib.uniform");
      defProcStFld("s32vector-set!", "kawa.lib.uniform");
      defProcStFld("s32vector->list", "kawa.lib.uniform");
      defProcStFld("list->s32vector", "kawa.lib.uniform");
      defProcStFld("u32vector?", "kawa.lib.uniform");
      defProcStFld("make-u32vector", "kawa.lib.uniform");
      defProcStFld("u32vector", "kawa.lib.uniform");
      defProcStFld("u32vector-length", "kawa.lib.uniform");
      defProcStFld("u32vector-ref", "kawa.lib.uniform");
      defProcStFld("u32vector-set!", "kawa.lib.uniform");
      defProcStFld("u32vector->list", "kawa.lib.uniform");
      defProcStFld("list->u32vector", "kawa.lib.uniform");

      defProcStFld("s64vector?", "kawa.lib.uniform");
      defProcStFld("make-s64vector", "kawa.lib.uniform");
      defProcStFld("s64vector", "kawa.lib.uniform");
      defProcStFld("s64vector-length", "kawa.lib.uniform");
      defProcStFld("s64vector-ref", "kawa.lib.uniform");
      defProcStFld("s64vector-set!", "kawa.lib.uniform");
      defProcStFld("s64vector->list", "kawa.lib.uniform");
      defProcStFld("list->s64vector", "kawa.lib.uniform");
      defProcStFld("u64vector?", "kawa.lib.uniform");
      defProcStFld("make-u64vector", "kawa.lib.uniform");
      defProcStFld("u64vector", "kawa.lib.uniform");
      defProcStFld("u64vector-length", "kawa.lib.uniform");
      defProcStFld("u64vector-ref", "kawa.lib.uniform");
      defProcStFld("u64vector-set!", "kawa.lib.uniform");
      defProcStFld("u64vector->list", "kawa.lib.uniform");
      defProcStFld("list->u64vector", "kawa.lib.uniform");

      defProcStFld("f32vector?", "kawa.lib.uniform");
      defProcStFld("make-f32vector", "kawa.lib.uniform");
      defProcStFld("f32vector", "kawa.lib.uniform");
      defProcStFld("f32vector-length", "kawa.lib.uniform");
      defProcStFld("f32vector-ref", "kawa.lib.uniform");
      defProcStFld("f32vector-set!", "kawa.lib.uniform");
      defProcStFld("f32vector->list", "kawa.lib.uniform");
      defProcStFld("list->f32vector", "kawa.lib.uniform");
      defProcStFld("f64vector?", "kawa.lib.uniform");
      defProcStFld("make-f64vector", "kawa.lib.uniform");
      defProcStFld("f64vector", "kawa.lib.uniform");
      defProcStFld("f64vector-length", "kawa.lib.uniform");
      defProcStFld("f64vector-ref", "kawa.lib.uniform");
      defProcStFld("f64vector-set!", "kawa.lib.uniform");
      defProcStFld("f64vector->list", "kawa.lib.uniform");
      defProcStFld("list->f64vector", "kawa.lib.uniform");

      defSntxStFld("cut", "gnu.kawa.slib.cut");
      defSntxStFld("cute", "gnu.kawa.slib.cut");

      defSntxStFld("cond-expand", "kawa.lib.syntax");
      defSntxStFld("%cond-expand", "kawa.lib.syntax");

      defAliasStFld("*print-base*", "gnu.kawa.functions.DisplayFormat",
                    "outBase");
      defAliasStFld("*print-radix*", "gnu.kawa.functions.DisplayFormat",
                    "outRadix");
      defAliasStFld("*print-right-margin*",
                    "gnu.text.PrettyWriter", "lineLengthLoc");
      defAliasStFld("*print-miser-width*",
                    "gnu.text.PrettyWriter", "miserWidthLoc");

      define((Language.NAMESPACE_PREFIX+"html").intern(),
             "http://www.w3.org/1999/xhtml");
      environ.define(Symbol.make("http://www.w3.org/1999/xhtml",
                                 DefineNamespace.XML_NAMESPACE_MAGIC),
                                 null, "html");
      kawaEnvironment.setLocked();
  }

  public Scheme ()
  {
    environ = kawaEnvironment;
    userEnv = getNewEnvironment();
  }

  protected Scheme (Environment env)
  {
    environ = env;
  }

  public String getName()
  {
    return "Scheme";
  }

  public NamedLocation lookupBuiltin (Symbol name, Object property, int hash)
  {
    NamedLocation loc = super.lookupBuiltin(name, property, hash);
    if (loc == null && property == null)
      {
        // Special handling for names of the form "<TYPE>".  I.e. if an
        // identifier of the form is unbound, then get a matching Type.
        // Also, handles U$unit by doing Unit.lookup("U").  (The Scheme reader
        // translates a quantity like 2in to (* 2 in$unit).  The advantage is
        // is that we can have clean scoping rules for unit names;  the downside
        // is that 2in is no longer a literal.)
        String nam = name.getName();
	Object val = null;
	String uri = name.getNamespaceURI();
        int len = nam.length();
	if (nam.endsWith("$unit"))
	  val = Unit.lookup(nam.substring(0, nam.length()-5));
	else if (len > 2 && nam.charAt(0) == '<' && nam.charAt(len-1) == '>')
          {
            String tname = nam.substring(1, len-1);
            val = Scheme.string2Type(tname);
          }
	if (val != null)
          {
            loc = new PlainLocation(name, null);
            loc.set(val);
          }
      }
    return loc;
  }

  /** Evalutate Scheme expressions from string.
   * @param string the string constaining Scheme expressions
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (String string, Environment env)
  {
    return eval (new CharArrayInPort(string), env);
  }

  /** Evalutate Scheme expressions from stream.
   * @param port the port to read Scheme expressions from
   * @param env the Environment to evaluate the string in
   * @return result of last expression, or Language.voidObject if none. */
  public static Object eval (InPort port, Environment env)
  {
    SourceMessages messages = new SourceMessages();
    try
      {
	LispReader lexer = (LispReader)
	  Language.getDefaultLanguage().getLexer(port, messages);
	Object body = ReaderParens.readList(lexer, 0, 1, -1);
        if (messages.seenErrors())
          throw new gnu.text.SyntaxException(messages);
	return Eval.evalBody(body, env, messages);
      }
    catch (gnu.text.SyntaxException e)
      {
	// The '\n' is because a SyntaxException includes a line number,
	// and it is better if that starts the line.  FIXME OBSOLETE
	throw new RuntimeException("eval: errors while compiling:\n"
				   +e.getMessages().toString(20));
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException("eval: I/O exception: "
				   + e.toString ());
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  /** Evalutate Scheme expressions from an "S expression."
   * @param sexpr the S expression to evaluate
   * @param env the Environment to evaluate the string in
   * @return result of the expression. */
  public static Object eval (Object sexpr, Environment env)
  {
    try
      {
	return Eval.eval (sexpr, env);
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return ScmRead.readObject(in);
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new ScmRead(inp, messages);
  }

  public static final AbstractFormat writeFormat = new DisplayFormat(true, 'S');
  public static final AbstractFormat displayFormat = new DisplayFormat(false, 'S');
  public AbstractFormat getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  public int getNamespaceOf (Declaration decl)
  {
    return FUNCTION_NAMESPACE+VALUE_NAMESPACE;
  }

  /** If exp is a "constant" Type, return that type, otherwise return null. */
  public static Type getTypeValue (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  static Hashtable types;

  public static Type getNamedType (String name)
  {
    if (types == null)
      {
	booleanType
	  = new LangPrimType(Type.boolean_type, Scheme.getInstance());
	types = new Hashtable ();
	types.put ("void", LangPrimType.voidType);
	types.put ("int", LangPrimType.intType);
	types.put ("char", LangPrimType.charType);
	types.put ("boolean", booleanType);
	types.put ("byte", LangPrimType.byteType);
	types.put ("short", LangPrimType.shortType);
	types.put ("long", LangPrimType.longType);
	types.put ("float", LangPrimType.floatType);
	types.put ("double", LangPrimType.doubleType);
	types.put ("never-returns", Type.neverReturnsType);

	types.put ("Object", Type.pointer_type);
	types.put ("java.lang.Object", Type.pointer_type);
	types.put ("String", Type.tostring_type);

	types.put ("object", Type.pointer_type);
	types.put ("number", ClassType.make("gnu.math.Numeric"));
	types.put ("quantity", ClassType.make("gnu.math.Quantity"));
	types.put ("complex", ClassType.make("gnu.math.Complex"));
	types.put ("real", ClassType.make("gnu.math.RealNum"));
	types.put ("rational", ClassType.make("gnu.math.RatNum"));
	types.put ("integer", ClassType.make("gnu.math.IntNum"));
	types.put ("symbol", ClassType.make("java.lang.String"));
	types.put ("keyword", ClassType.make("gnu.expr.Keyword"));
	types.put ("list", ClassType.make("gnu.lists.LList"));
	types.put ("pair", ClassType.make("gnu.lists.Pair"));
	types.put ("pair-with-position",
		   ClassType.make("gnu.lists.PairWithPosition"));
	types.put ("string", ClassType.make("gnu.lists.FString"));
	types.put ("abstract-string", ClassType.make("gnu.lists.CharSeq"));
	types.put ("character", ClassType.make("gnu.text.Char"));
	types.put ("vector", ClassType.make("gnu.lists.FVector"));
	types.put ("function", ClassType.make("gnu.mapping.Procedure"));
	types.put ("procedure", ClassType.make("gnu.mapping.Procedure"));
	types.put ("input-port", ClassType.make("gnu.mapping.InPort"));
	types.put ("output-port", ClassType.make("gnu.mapping.OutPort"));
	types.put ("string-output-port",
                   ClassType.make("gnu.mapping.CharArrayOutPort"));
	types.put ("record", ClassType.make("kawa.lang.Record"));
	types.put ("type", ClassType.make("gnu.bytecode.Type"));
	types.put ("class-type", ClassType.make("gnu.bytecode.ClassType"));

        types.put ("s8vector", ClassType.make("gnu.lists.S8Vector"));
        types.put ("u8vector", ClassType.make("gnu.lists.U8Vector"));
        types.put ("s16vector", ClassType.make("gnu.lists.S16Vector"));
        types.put ("u16vector", ClassType.make("gnu.lists.U16Vector"));
        types.put ("s32vector", ClassType.make("gnu.lists.S32Vector"));
        types.put ("u32vector", ClassType.make("gnu.lists.U32Vector"));
        types.put ("s64vector", ClassType.make("gnu.lists.S64Vector"));
        types.put ("u64vector", ClassType.make("gnu.lists.U64Vector"));
        types.put ("f32vector", ClassType.make("gnu.lists.F32Vector"));
        types.put ("f64vector", ClassType.make("gnu.lists.F64Vector"));
        types.put ("document", ClassType.make("gnu.lists.TreeList"));
        types.put ("readtable", ClassType.make("gnu.kawa.lispexpr.ReadTable"));
        /* #ifdef use:java.net.URI */
        types.put ("URI", ClassType.make("java.net.URI"));
        /* #else */
        // types.put ("URI", ClassType.make("java.lang.String"));
        /* #endif */
      }
    Type type = (Type) types.get(name);
    if (type == null
	&& (name.startsWith("elisp:") || name.startsWith("clisp:")))
      {
	int colon = name.indexOf(':');
	Class clas = getNamedType(name.substring(colon+1)).getReflectClass();
	String lang = name.substring(0,colon);
	Language interp = Language.getInstance(lang);
	if (interp == null)
	    throw new RuntimeException("unknown type '" + name
				       + "' - unknown language '"
				       + lang + '\'');
	type = interp.getTypeFor(clas);
	if (type != null)
	  types.put(name, type);
      }
    return type;
  }

  public Type getTypeFor (Class clas)
  {
    String name = clas.getName();
    if (clas.isPrimitive())
      return getNamedType(name);
    if ("java.lang.String".equals(name))
      return Type.tostring_type;
    return Type.make(clas);
  }

  public static Type string2Type (String name)
  {
    Type t;
    if (name.endsWith("[]"))
      {
	t = string2Type(name.substring(0, name.length()-2));
	if (t != null)
	  t = ArrayType.make(t);
      }
    else
      t = getNamedType (name);
    if (t != null)
      return t;
    t = Language.string2Type(name);
    if (t != null)
      types.put (name, t);
    return t;
  }

  public Type getTypeFor(String name)
  {
    return string2Type(name);
  }

  /** Convert expression to a Type.
   * Allow "TYPE" or 'TYPE or <TYPE>.
   */
  public static Type exp2Type (Expression exp)
  {
    return getInstance().getTypeFor(exp);
  }

  public ReadTable createReadTable ()
  {
    ReadTable tab = ReadTable.getInitial();
    tab.postfixLookupOperator = '\\';
    ReaderDispatch dispatchTable = (ReaderDispatch) tab.lookup('#');
    dispatchTable.set('\'', new ReaderQuote("syntax"));
    dispatchTable.set('`', new ReaderQuote("quasisyntax"));
    dispatchTable.set(',', ReaderDispatchMisc.getInstance());
    tab.putReaderCtorFld("URI", "kawa.lib.files", "URI");
    tab.putReaderCtorFld("namespace", "kawa.lib.misc", "namespace");
    return tab;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(getInstance());
  }
}
