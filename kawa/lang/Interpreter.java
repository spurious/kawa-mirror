package kawa.lang;

//-- Java dependancies

import java.util.Hashtable;
import java.util.Vector;

//-- kawa Primitives
import kawa.lang.Printable; 
import kawa.lang.Executable; 
import kawa.lang.Syntaxable; 
import kawa.lang.Symbol;
import kawa.lang.Exit;

// Exceptions
import kawa.lang.UnboundSymbol;
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

public class Interpreter extends Object
{
  static public Boolean  trueObject = new Boolean(true);
  static public Boolean  falseObject = new Boolean(false);

  static public List nullObject = List.Empty;
  static public Undefined undefinedObject = new kawa.lang.Undefined();
  static public Undefined voidObject = new kawa.lang.Undefined();
  static public Symbol eofObject = Symbol.makeUninterned ("#EOF");

  // Global environment.
  // Currently maps String -> Object;  should probably be Symbol -> Object.
  protected java.util.Hashtable globals;
  
  static protected Symbol lambda_sym = Symbol.intern ("lambda");
  static protected Lambda lambda = new Lambda ();
  static public Symbol quote_sym = Symbol.make ("quote");
  static public Symbol unquote_sym = Symbol.make ("unquote");
  static public Symbol unquotesplicing_sym = Symbol.make ("unquote-splicing");
  static public Symbol quasiquote_sym = Symbol.intern ("quasiquote");
  static protected Quote quote = new Quote();

  // Map name to Declaration.
  public java.util.Hashtable current_decls;
  ScopeExp current_scope;

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  protected QuasiQuote      quasiquote;
  protected Unquote         unquote;
  protected UnquoteSplicing unquotesplicing;

  public InPort in;
  public OutPort out;
  public OutPort err;

  static Interpreter curInterpreter;

  static public Interpreter current ()
  {
    return curInterpreter;
  }

  public Interpreter(InPort i, OutPort o, OutPort e)
  {
      in = i;
      out = o;
      err = e;

      curInterpreter = this;

      globals         = new java.util.Hashtable();
      current_decls   = new java.util.Hashtable();
      quasiquote      = new kawa.lang.QuasiQuote();
      unquote         = new kawa.lang.Unquote();
      unquotesplicing = new kawa.lang.UnquoteSplicing();

      define(lambda_sym, lambda);
      define(quote_sym, quote);
      define(quasiquote.name,quasiquote);
      define(unquote.name,unquote);
      define(unquotesplicing.name,unquotesplicing);
   }

  public void define(String name, Object p)
  {
    globals.put(name,p);
  }

  public void define(Symbol sym, Object p)
  {
    globals.put(sym.toString (),p);
  }

  public static Object lookup_global (Symbol name)
  {
    Object result = curInterpreter.lookup (name);
    if (result == null)
      new UnboundSymbol(name.toString ());
    return result;
  }

  public static void define_global (Symbol name, Object new_value)
  {
    curInterpreter.define (name, new_value);
  }

  public Object lookup(java.lang.String name)
  {
    return globals.get(name);
  }

  public Object lookup(Symbol name)
  {
    return globals.get(name.toString ());
  }

   public Pair copy(Pair list) {
      Pair newlist = new Pair(list.car,list.cdr);
      Pair current = newlist;
      while (list.cdr instanceof Pair) {
         list = (Pair)list.cdr;
         Pair Pair = new Pair(list.car,list.cdr);
         current.cdr = Pair;
         current = Pair;
      }
      current.cdr = list.cdr;
      return newlist;
   }

   public Pair lastPair(Pair list) {
      while (list.cdr instanceof Pair) {
         list = (Pair)list.cdr;
      }
      return list;
   }

   public Object apply(Object rator,Object rands,java.util.Vector frames)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (rator instanceof kawa.lang.Executable) {
         Executable proc = (Executable)rator;
         return proc.execute(this,frames,rands);
      } else if (rator instanceof kawa.lang.Syntaxable) {
         Syntaxable s = (Syntaxable)rator;
         return s.execute(this,frames,rands);
      } else {
         //-- TODO: Must handle closures and continuations
         return undefinedObject;
      }
   }

   public Object eval(Object obj) 
      throws kawa.lang.UnboundSymbol,
             kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
     throw new GenericError ("interp.eval called!");
   }

   public Object eval(Object obj,java.util.Vector frames) 
      throws kawa.lang.UnboundSymbol,
             kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
     throw new GenericError ("interp.eval called!");
   }
 
  public Object read()
      throws java.io.IOException, SyntaxError
  {
    return in.readSchemeObject ();
  }

  /**
   * Re-write a Scheme <body> in S-expression format into internal form.
   * Does not yet handle internal defines.
   */

  public Expression rewrite_body (Object exp)
       throws kawa.lang.WrongArguments
  {
    int count = kawa.standard.length.length (exp);
    if (count == 1)
      return rewrite (((Pair)exp).car);
    else if (count == 0)
      throw new WrongArguments ("<body>", 1, "body with no expressions");
    else
      {
	Expression[] exps = new Expression [count];
	for (int i = 0; i < count; i++)
	  {
	    Pair exp_pair = (Pair) exp;
	    exps[i] = rewrite (exp_pair.car);
	    exp = exp_pair.cdr;
	  }
	return new BeginExp (exps);
      }
  }

  Syntax current_syntax;
  Object current_syntax_args;
  Expression errorExp = new ErrorExp ("unknown syntax error");

  /**
   * Apply a Syntax object.
   * @param syntax the Syntax object whose rewrite method we call
   * @param args the syntax arguments (the cdr of the syntax form)
   * @return the re-written form as an Expression object
   */
  Expression apply_rewrite (Syntax syntax, Object args)
       throws WrongArguments
  {
    Syntax save_syntax = syntax;
    Object save_args = args;
    current_syntax = syntax;
    current_syntax_args = args;
    Expression exp = errorExp;
    try
      {
	exp = syntax.rewrite (args, this);
      }
    finally
      {
	current_syntax = save_syntax;
	current_syntax_args = save_args;
      }
    return exp;
  }

  /** Count of errors seen (at compile time). */
  public int errors;

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
  {
    errors++;
    System.err.print ("syntax error: ");
    System.err.println (message);
    return new ErrorExp (message);
  }

  /** Resolve a symbol to a Declaration.
   * May cause the Declaration to be "captured" in a closure ("heapFrame").
   * @param sym the symbol whose Declaration we want
   * @return the Declaration, or null if there no lexical Declaration */
  public Declaration resolve (Symbol sym)
  {
    Declaration decl = (Declaration) current_decls.get (sym);
    if (decl != null)
      {
	LambdaExp curLambda = currentLambda ();
	LambdaExp declLambda = decl.context.currentLambda ();
	if (curLambda != declLambda || declLambda == null)
	  {
	    decl.setSimple (false);
	    LambdaExp lambda = curLambda;
	    for (; lambda != declLambda; lambda = lambda.outerLambda ())
	      {
		if (lambda.staticLink == null)
		  lambda.staticLink
		    = lambda.add_decl (Symbol.make ("staticLink"),
				       Compilation.objArrayType);
		if (lambda != curLambda)
		  lambda.staticLink.setSimple (false);
	      }
	  }
      }
    return decl;
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp)
       throws WrongArguments
  {
    if (exp instanceof Pair)
      {
	Pair p = (Pair)exp;
	Object car = p.car;
	Object cdr = p.cdr;
	if (car instanceof Syntax)
	  return apply_rewrite ((Syntax)car, cdr);

	if (car instanceof Symbol)
	  {
	    Object binding = lookup ((Symbol) car);
	    if (binding instanceof Syntax)
	      return apply_rewrite ((Syntax) binding, cdr);
	  }

	int cdr_length = kawa.standard.length.length (cdr);

	Expression[] args = new Expression[cdr_length];
	for (int i = 0; i < cdr_length; i++)
	  {
	    Pair cdr_pair = (Pair) cdr;
	    args[i] = rewrite (cdr_pair.car);
	    cdr = cdr_pair.cdr;
	  }
	return new ApplyExp (rewrite (car), args);
      }
    else if (exp instanceof Symbol)
      {
	Symbol sym = (Symbol) exp;
	return new ReferenceExp (sym, resolve (sym));
      }
    else if (exp instanceof Expression)
      return (Expression) exp;
    else if (exp == null)
      return null;
    else
      return new QuoteExp (exp);
  }

}
