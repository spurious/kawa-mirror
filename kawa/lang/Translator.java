package kawa.lang;

/** Used to translate from source for to Expression.
 * The result has macros expanded, lexical names bound, etc, and is
 * ready for code generation.
 * This is sometimes called a "compilation environment",
 * but we modify it as we go along - there is a single Translator for
 * each top-level environment.
 */

public class Translator extends Object
{
  // Map name to Declaration.
  public java.util.Hashtable current_decls;
  ScopeExp current_scope;

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  private Syntax current_syntax;
  private Object current_syntax_args;
  private static Expression errorExp = new ErrorExp ("unknown syntax error");
  String current_filename;
  int current_line;
  int current_column;

  // Global environment used to look for syntax/macros.
  Environment env;

  public Translator (Environment env)
  {
    this.env = env;
    current_decls = new java.util.Hashtable();
  }

  public Translator ()
  {
    this (Environment.user());
  }

  /**
   * Re-write a Scheme <body> in S-expression format into internal form.
   * Does not yet handle internal defines.
   */

  public Expression rewrite_body (Object exp)
  {
    int count = List.list_length (exp);
    if (count == 1)
      return rewrite (((Pair)exp).car);
    else if (count <= 0)
      return syntaxError (count == 0 ? "body with no expressions"
			   : "body is not a proper list");
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
	      binding = Environment.user().get ((Symbol) binding);
	  }
	else
	  binding = Environment.user().get (sym);
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
