package kawa.standard;
import kawa.lang.*;

public class StandardInterpreter extends Interpreter
{
  final void define_proc (Named proc)
  {
    define (proc.name, proc);
  }

  /* Define a procedure to be autoloaded. */
  final void define_proc (String name, String className)
  {
    Symbol symbol = Symbol.make (name);
    define (symbol, new AutoloadProcedure (symbol, className));
  }

  public kawa.standard.StandardInterpreter(InPort i, OutPort o, OutPort e)
  {
      super(i,o,e);

      kawa.lang.Named proc;
      kawa.lang.Named syn;

      kawa.lang.Procedure2 eqv;
      kawa.lang.Procedure2 eq;
      kawa.lang.Procedure2 equal;

      //-- Section 4.1
      define("if", new kawa.standard.ifp());
      define("set!", new kawa.standard.set_b());

      // Section 4.2
      define("cond",new kawa.standard.cond());

      //-- Section 6.1
      define_proc ("not", "kawa.standard.not");
      define_proc ("boolean?", "kawa.standard.boolean_p");

      //-- Section 6.2
      eqv = new kawa.standard.eqv_p();
      define(eqv.name,eqv);
      eq = new kawa.standard.eq_p();
      define(eq.name,eq);
      equal = new kawa.standard.equal_p();
      define(equal.name,equal);

      //-- Section 6.3
      define_proc ("pair?", "kawa.standard.pair_p");
      define_proc ("cons", "kawa.standard.cons");
      define_proc ("car", "kawa.standard.car");
      define_proc ("cdr", "kawa.standard.cdr");
      define_proc ("set-car!", "kawa.standard.setcar_b");
      define_proc ("set-cdr", "kawa.standard.setcdr_b");

      define_proc ("caar", "kawa.standard.caar");
      define_proc ("cadr", "kawa.standard.cadr");
      define_proc ("cdar", "kawa.standard.cdar");
      define_proc ("cddr", "kawa.standard.cddr");
      define_proc ("caaar", "kawa.standard.caaar");
      define_proc ("caadr", "kawa.standard.caadr");
      define_proc ("cadar", "kawa.standard.cadar");
      define_proc ("caddr", "kawa.standard.caddr");
      define_proc ("cdaar", "kawa.standard.cdaar");
      define_proc ("cdadr", "kawa.standard.cdadr");
      define_proc ("cddar", "kawa.standard.cddar");
      define_proc ("cdddr", "kawa.standard.cdddr");
      define_proc ("caaaar", "kawa.standard.caaaar");
      define_proc ("caaadr", "kawa.standard.caaadr");
      define_proc ("caadar", "kawa.standard.caadar");
      define_proc ("caaddr", "kawa.standard.caaddr");
      define_proc ("cadaar", "kawa.standard.cadaar");
      define_proc ("cadadr", "kawa.standard.cadadr");
      define_proc ("caddar", "kawa.standard.caddar");
      define_proc ("cadddr", "kawa.standard.cadddr");
      define_proc ("cdaaar", "kawa.standard.cdaaar");
      define_proc ("cdaadr", "kawa.standard.cdaadr");
      define_proc ("cdadar", "kawa.standard.cdadar");
      define_proc ("cdaddr", "kawa.standard.cdaddr");
      define_proc ("cddaar", "kawa.standard.cddaar");
      define_proc ("cddadr", "kawa.standard.cddadr");
      define_proc ("cdddar", "kawa.standard.cdddar");
      define_proc ("cddddr", "kawa.standard.cddddr");
      define_proc ("null?", "kawa.standard.null_p");
      define_proc ("list?", "kawa.standard.list_p");
      define_proc ("list", "kawa.standard.list");
      define_proc ("length", "kawa.standard.length");
      define_proc ("append", "kawa.standard.append");
      define_proc ("list-tail", "kawa.standard.list_tail");
      define_proc ("list-ref", "kawa.standard.list_ref");

      proc = new kawa.standard.mem("memq",eq);
      define(proc.name,proc);
      proc = new kawa.standard.mem("memv",eqv);
      define(proc.name,proc);
      proc = new kawa.standard.mem("member",equal);
      define(proc.name,proc);
      proc = new kawa.standard.ass("assq",eq);
      define(proc.name,proc);
      proc = new kawa.standard.ass("assv",eqv);
      define(proc.name,proc);
      proc = new kawa.standard.ass("assoc",equal);
      define(proc.name,proc);

      //-- Section 6.4
      
      define_proc ("symbol?", "kawa.standard.symbol_p");
      define_proc ("symbol->string", "kawa.standard.symbol2string");
      define_proc ("string->symbol", "kawa.standard.string2symbol");

      //-- Section 6.5
      define_proc ("number?", "kawa.standard.number_p");
      define_proc ("real?", "kawa.standard.real_p");
      define("inexact?",proc);
      proc = new kawa.standard.integer_p();
      define(proc.name,proc);
      define("exact?",proc);
      define_proc ("zero?", "kawa.standard.zero_p");
      define_proc ("positive?", "kawa.standard.positive_p");
      define_proc ("negative?", "kawa.standard.negative_p");
      define_proc ("=", "kawa.standard.equal_oper");
      define_proc ("<", "kawa.standard.less_oper");
      define_proc ("<=", "kawa.standard.lessequal_oper");
      define_proc (">", "kawa.standard.greater_oper");
      define_proc (">=", "kawa.standard.greaterequal_oper");
      define_proc ("+", "kawa.standard.plus_oper");
      define_proc ("-", "kawa.standard.minus_oper");
      define_proc ("*", "kawa.standard.multiply_oper");
      define_proc ("/", "kawa.standard.divide_oper");
      define_proc ("abs", "kawa.standard.abs");

      //-- Section 6.6
      proc = new kawa.standard.char_p();
      define(proc.name,proc);
      proc = new kawa.standard.char_equal_p();
      define(proc.name,proc);
      proc = new kawa.standard.char_less_p();
      define(proc.name,proc);
      proc = new kawa.standard.char_less_equal_p();
      define(proc.name,proc);
      proc = new kawa.standard.char_greater_p();
      define(proc.name,proc);
      proc = new kawa.standard.char_greater_equal_p();
      define(proc.name,proc);
      
      //-- Section 6.7
      proc = new kawa.standard.string_p();
      define(proc.name,proc);
      proc = new kawa.standard.make_string();
      define(proc.name,proc);
      proc = new kawa.standard.string();
      define(proc.name,proc);
      proc = new kawa.standard.string_length();
      define(proc.name,proc);
      proc = new kawa.standard.string_ref();
      define(proc.name,proc);
      proc = new kawa.standard.string_set_b();
      define(proc.name,proc);
      proc = new kawa.standard.list2string();
      define(proc.name,proc);
      proc = new kawa.standard.string2list();
      define(proc.name,proc);
      proc = new kawa.standard.string_append();
      define(proc.name,proc);
      proc = new kawa.standard.string_ci_equal_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_ci_greaterthan_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_ci_greaterthanequal_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_ci_lessthan_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_ci_lessthanequal_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_copy();
      define(proc.name,proc);
      proc = new kawa.standard.string_fill_b();
      define(proc.name,proc);
      proc = new kawa.standard.string_equal_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_greaterthan_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_greaterthanequal_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_lessthan_p();
      define(proc.name,proc);
      proc = new kawa.standard.string_lessthanequal_p();
      define(proc.name,proc);
      proc = new kawa.standard.substring();
      define(proc.name,proc);

      //-- Section 6.8
      define_proc ("vector?", "kawa.standard.vector_p");
      define_proc ("make-vector", "kawa.standard.make_vector");
      define_proc ("vector", "kawa.standard.vector");
      define_proc ("vector-length", "kawa.standard.vector_length");
      define_proc ("vector-ref", "kawa.standard.vector_ref");
      define_proc ("vector-set!", "kawa.standard.vector_set_b");
      define_proc ("list->vector", "kawa.standard.list2vector");
      define_proc ("vector->list", "kawa.standard.vector2list");
      define_proc ("vector-fill!", "kawa.standard.vector_fill_b");

      //-- Section 6.9
      define_proc ("procedure?", "kawa.standard.procedure_p");
      define_proc ("apply", "kawa.standard.apply");
      define_proc (new map (true));        // map
      define_proc (new map (false));       // for-each
      define_proc ("call-with-current-continuation", "kawa.standard.callcc");

      //-- Section 6.10
      define_proc ("read", "kawa.standard.read");
      define_proc (new readchar (false));  // read-char
      define_proc (new readchar (true));   // peek-char
      define_proc (new write(true));       // write
      define_proc (new write(false));      // display
      define_proc ("write-char", "kawa.standard.writechar");
      define_proc ("newline", "kawa.standard.newline");
      define_proc ("load", "kawa.standard.load");

      //-- (let ((n obj)...) e1 ... )
      define("let", new kawa.standard.let());
      //-- (let* ((n obj)...) e1 ... )
      define("let*", new kawa.standard.letstar());
      //-- (letrec ((n obj)...) e1 ... )
      define("letrec", new letrec());
      //-- (define sym obj)
      define("define", new kawa.standard.define());
      //-- (apply sym obj)
      //-- (begin obj ...)
      define("begin", new kawa.standard.begin());
      //-- (if cond then-exp else-exp)
      //-- (when cond exp ...)
      syn = new kawa.standard.when();
      define(syn.name,syn);
      //-- (unless cond exp ...)
      syn = new kawa.standard.unless();
      define(syn.name,syn);
      //-- (and obj ...)
      proc = new kawa.standard.and();
      define(proc.name,proc);
      //-- (or obj ...)
      proc = new kawa.standard.or();
      define(proc.name,proc);

      
      //-- (exit)
      proc = new kawa.standard.exit();
      define(proc.name,proc);

      define_proc ("compile-func", "kawa.lang.compilefune");
      define_proc ("compile-file", "kawa.lang.CompileFile");
      define_proc ("load-compiled", "kawa.lang.loadcompiled");
   }
}
