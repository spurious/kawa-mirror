package kawa.lang;

//-- Java dependancies
import java.util.Hashtable;

public class Interpreter extends Object
{
  static public Boolean  trueObject = new Boolean(true);
  static public Boolean  falseObject = new Boolean(false);

  static public List nullObject = List.Empty;
  static public Undefined undefinedObject = new kawa.lang.Undefined();
  static public Undefined voidObject = new kawa.lang.Undefined();

  static public Symbol quote_sym = Symbol.make ("quote");
  static public Symbol unquote_sym = Symbol.make ("unquote");
  static public Symbol unquotesplicing_sym = Symbol.make ("unquote-splicing");
  static public Symbol quasiquote_sym = Symbol.intern ("quasiquote");

  // Map name to Declaration.
  public java.util.Hashtable current_decls;
  ScopeExp current_scope;

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public InPort in;
  public OutPort out;
  public OutPort err;

  static Interpreter curInterpreter;

  public static final Boolean boolObject (boolean b)
  {
    return b ? trueObject : falseObject;
  }

  static public Interpreter current ()
  {
    return curInterpreter;
  }

  // transitional hack FIXME
  protected Environment env;
  public static Environment curEnvironment ()
  {
    return curInterpreter.env;
  }

  public Interpreter(InPort i, OutPort o, OutPort e)
  {
      in = i;
      out = o;
      err = e;

      curInterpreter = this;
      current_decls   = new java.util.Hashtable();
   }

  public void define(String name, Object p)
  {
    env.put (Symbol.make (name), p);
  }

  public void define(Symbol sym, Object p)
  {
    env.define (sym, p);
  }

  public static Object lookup_global (Symbol name)
       throws UnboundSymbol
  {
    Object result = curInterpreter.lookup (name);
    if (result == null)
      throw new UnboundSymbol(name);
    return result;
  }

  public static void define_global (Symbol name, Object new_value)
  {
    curInterpreter.define (name, new_value);
  }

  public Object lookup(java.lang.String name)
  {
    return env.get (Symbol.make (name));
  }

  public Object lookup(Symbol name)
  {
    return env.get (name);
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

  /**
   * Re-write a Scheme <body> in S-expression format into internal form.
   * Does not yet handle internal defines.
   */

  public Expression rewrite_body (Object exp)
  {
    int count = List.length (exp);
    if (count == 1)
      return rewrite (((Pair)exp).car);
    else if (count == 0)
      return syntaxError ("body with no expressions");
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
  String current_filename;
  int current_line;
  int current_column;

  final Expression rewrite_car (Pair pair)
  {
    if (pair instanceof PairWithPosition)
      return rewrite_with_position (pair.car, (PairWithPosition) pair);
    else
      return rewrite (pair.car);
  }

  /**
   * Apply a Syntax object.
   * @param syntax the Syntax object whose rewrite method we call
   * @param args the syntax arguments (the cdr of the syntax form)
   * @return the re-written form as an Expression object
   */
  Expression apply_rewrite (Syntax syntax, Object args)
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
    if (current_line > 0)
      {
	if (current_filename != null)
	  System.err.print (current_filename);
	System.err.print (':');
	System.err.print (current_line);
	if (current_column > 1)
	  {
	    System.err.print (':');
	    System.err.print (current_column);
	  }
	System.err.print (": ");
      }
    System.err.println (message);
    return new ErrorExp (message);
  }

  /** Resolve a symbol to a Declaration.
   * May cause the Declaration to be "captured" in a closure ("heapFrame").
   * @param sym the symbol whose Declaration we want
   * @param decl the current lexical binding, if any
   * @return the Declaration, or null if there no lexical Declaration */
  public Declaration resolve (Symbol sym, Declaration decl)
  {
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

  public Expression rewrite_pair (Pair p)
  {
    Object car = p.car;
    Object cdr = p.cdr;
    if (car instanceof Syntax)
      return apply_rewrite ((Syntax)car, cdr);

    if (car instanceof Symbol)
      {
	Symbol sym = (Symbol) car;
	Object binding = current_decls.get (sym);
        if (binding != null)
	  {
	    // Hygenic macro expansion may bind a renamed (uninterned) Symbol
	    // to the original Symbol.  Here, use the original Symbol.
	    if (binding instanceof Symbol)
	      binding = lookup ((Symbol) binding);
	  }
	else
	  binding = lookup ((Symbol) car);
	if (binding instanceof Syntax)
	  return apply_rewrite ((Syntax) binding, cdr);
      }

    int cdr_length = List.length (cdr);

    Expression func = rewrite_car (p);
    Expression[] args = new Expression[cdr_length];
    for (int i = 0; i < cdr_length; i++)
      {
	Pair cdr_pair = (Pair) cdr;
	args[i] = rewrite_car (cdr_pair);
	cdr = cdr_pair.cdr;
      }
    return new ApplyExp (func, args);
  }

  /**
   * Re-write a Scheme expression in S-expression format into internal form.
   */
  public Expression rewrite (Object exp)
  {
    if (exp instanceof PairWithPosition)
      return rewrite_with_position (exp, (PairWithPosition) exp);
    else if (exp instanceof Pair)
      return rewrite_pair ((Pair) exp);
    else if (exp instanceof Symbol)
      {
	Symbol sym = (Symbol) exp;
	Object binding = current_decls.get (sym);
	// Hygenic macro expansion may bind a renamed (uninterned) Symbol
	// to the original Symbol.  Here, use the original Symbol.
	if (binding != null && binding instanceof Symbol)
	  return new ReferenceExp ((Symbol) binding);
	return new ReferenceExp (sym, resolve (sym, (Declaration) binding));
      }
    else if (exp instanceof Expression)
      return (Expression) exp;
    else if (exp == null)
      return null;
    else
      return new QuoteExp (exp);
  }

  public Expression rewrite_with_position (Object exp, PairWithPosition pair)
  {
    String save_filename = current_filename;
    int save_line = current_line;
    int save_column = current_column;
    Expression result;
    try
      {
	String exp_file = pair.getFile ();
	int exp_line = pair.getLine ();
	int exp_column = pair.getColumn ();
	current_filename = exp_file;
	current_line = exp_line;
	current_column = exp_column;
	if (exp == pair)
	  result = rewrite_pair (pair);  // To avoid a cycle
	else
	  result = rewrite (exp);
	if (result.getFile () == null)
	  result.setFile (exp_file);
	if (result.getLine () == 0)
	  result.setLine (exp_line, exp_column);
      }
    finally
      {
	current_filename = save_filename;
	current_line = save_line;
	current_column = save_column;
      }
    return result;
  }

}
