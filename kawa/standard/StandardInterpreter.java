package kawa.standard;
import kawa.lang.*;

public class StandardInterpreter extends Interpreter
{
  final void define_proc (Named proc)
  {
    define (proc.name, proc);
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

      //-- Section 6.1
      proc = new kawa.standard.not();
      define(proc.name,proc);
      proc = new kawa.standard.boolean_p();
      define(proc.name,proc);

      //-- Section 6.2
      eqv = new kawa.standard.eqv_p();
      define(eqv.name,eqv);
      eq = new kawa.standard.eq_p();
      define(eq.name,eq);
      equal = new kawa.standard.equal_p();
      define(equal.name,equal);

      //-- Section 6.3
      proc = new kawa.standard.pair_p();
      define(proc.name,proc);
      proc = new kawa.standard.cons();
      define(proc.name,proc);
      proc = new kawa.standard.car();
      define(proc.name,proc);
      proc = new kawa.standard.cdr();
      define(proc.name,proc);
      proc = new kawa.standard.setcar_b();
      define(proc.name,proc);
      proc = new kawa.standard.setcdr_b();
      define(proc.name,proc);
      proc = new kawa.standard.caar();
      define(proc.name,proc);
      proc = new kawa.standard.cadr();
      define(proc.name,proc);
      proc = new kawa.standard.cdar();
      define(proc.name,proc);
      proc = new kawa.standard.cddr();
      define(proc.name,proc);
      proc = new kawa.standard.caaar();
      define(proc.name,proc);
      proc = new kawa.standard.caadr();
      define(proc.name,proc);
      proc = new kawa.standard.cadar();
      define(proc.name,proc);
      proc = new kawa.standard.caddr();
      define(proc.name,proc);
      proc = new kawa.standard.cdaar();
      define(proc.name,proc);
      proc = new kawa.standard.cdadr();
      define(proc.name,proc);
      proc = new kawa.standard.cddar();
      define(proc.name,proc);
      proc = new kawa.standard.cdddr();
      define(proc.name,proc);
      proc = new kawa.standard.caaaar();
      define(proc.name,proc);
      proc = new kawa.standard.caaadr();
      define(proc.name,proc);
      proc = new kawa.standard.caadar();
      define(proc.name,proc);
      proc = new kawa.standard.caaddr();
      define(proc.name,proc);
      proc = new kawa.standard.cadaar();
      define(proc.name,proc);
      proc = new kawa.standard.cadadr();
      define(proc.name,proc);
      proc = new kawa.standard.caddar();
      define(proc.name,proc);
      proc = new kawa.standard.cadddr();
      define(proc.name,proc);
      proc = new kawa.standard.cdaaar();
      define(proc.name,proc);
      proc = new kawa.standard.cdaadr();
      define(proc.name,proc);
      proc = new kawa.standard.cdadar();
      define(proc.name,proc);
      proc = new kawa.standard.cdaddr();
      define(proc.name,proc);
      proc = new kawa.standard.cddaar();
      define(proc.name,proc);
      proc = new kawa.standard.cddadr();
      define(proc.name,proc);
      proc = new kawa.standard.cdddar();
      define(proc.name,proc);
      proc = new kawa.standard.cddddr();
      define(proc.name,proc);
      proc = new kawa.standard.null_p();
      define(proc.name,proc);
      proc = new kawa.standard.list_p();
      define(proc.name,proc);
      proc = new kawa.standard.list();
      define(proc.name,proc);
      proc = new kawa.standard.length();
      define(proc.name,proc);
      proc = new kawa.standard.append();
      define(proc.name,proc);
      proc = new kawa.standard.list_tail();
      define(proc.name,proc);
      proc = new kawa.standard.list_ref((kawa.lang.Procedure2)proc);
      define(proc.name,proc);
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
      proc = new kawa.standard.symbol_p();
      define(proc.name,proc);
      proc = new kawa.standard.symbol2string();
      define(proc.name,proc);
      proc = new kawa.standard.string2symbol();
      define(proc.name,proc);

      //-- Section 6.5
      proc = new kawa.standard.number_p();
      define(proc.name,proc);
      proc = new kawa.standard.real_p();
      define(proc.name,proc);
      define("inexact?",proc);
      proc = new kawa.standard.integer_p();
      define(proc.name,proc);
      define("exact?",proc);
      proc = new kawa.standard.zero_p();
      define(proc.name,proc);
      proc = new kawa.standard.positive_p();
      define(proc.name,proc);
      proc = new kawa.standard.negative_p();
      define(proc.name,proc);
      proc = new kawa.standard.equal_oper();
      define(proc.name,proc);
      proc = new kawa.standard.less_oper();
      define(proc.name,proc);
      proc = new kawa.standard.lessequal_oper();
      define(proc.name,proc);
      proc = new kawa.standard.greater_oper();
      define(proc.name,proc);
      proc = new kawa.standard.greaterequal_oper();
      define(proc.name,proc);
      proc = new kawa.standard.plus_oper();
      define(proc.name,proc);
      proc = new kawa.standard.minus_oper();
      define(proc.name,proc);
      proc = new kawa.standard.multiply_oper();
      define(proc.name,proc);
      proc = new kawa.standard.divide_oper();
      define(proc.name,proc);
      proc = new kawa.standard.abs();
      define(proc.name,proc);

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

      //-- Section 6.8
      proc = new kawa.standard.vector_p();
      define(proc.name,proc);
      proc = new kawa.standard.make_vector();
      define(proc.name,proc);
      proc = new kawa.standard.vector();
      define(proc.name,proc);
      proc = new kawa.standard.vector_length();
      define(proc.name,proc);
      proc = new kawa.standard.vector_ref();
      define(proc.name,proc);
      proc = new kawa.standard.vector_set_b();
      define(proc.name,proc);

      //-- Section 6.9
      proc = new kawa.standard.procedure_p();
      define(proc.name,proc);
      proc = new kawa.standard.apply();
      define(proc.name,proc);

      //-- Section 6.10
      define_proc (new read ());           // read
      define_proc (new readchar (false));  // read-char
      define_proc (new readchar (true));   // peek-char
      define_proc (new eof_object_p ());   // eof-object?
      define_proc (new write(true));       // write
      define_proc (new write(false));      // display
      define_proc (new writechar ());      // write-char

      define_proc (new load ());           // load

      //-- (let ((n obj)...) e1 ... )
      define("let", new kawa.standard.let());
      //-- (let* ((n obj)...) e1 ... )
      define("let*", new kawa.standard.letstar());
      //-- (letrec ((n obj)...) e1 ... )
      syn = new kawa.standard.letrec();
      define(syn.name,syn);
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

   }

}
