package kawa.standard;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the Scheme "and" and "or" primitives.
 * (Now only used for "or".)
 * @author	Per Bothner
 */

public class and_or extends Syntax implements Printable
{
  boolean is_and;
  
  and_or (boolean is_and)
  {
    this.is_and = is_and;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (obj == List.Empty)
      return new QuoteExp (Scheme.boolObject (is_and));
    if (! (obj instanceof Pair))
      return tr.syntaxError ("non-list arguments to and/or");
    Pair pair = (Pair) obj;
    if (pair.cdr == List.Empty)
      return tr.rewrite (pair.car);
    Expression[] inits = new Expression[1];
    LetExp let = new LetExp (inits);
    String temp_name = Symbol.generate ();
    Declaration temp_decl = let.addDeclaration (temp_name);
    inits[0] = tr.rewrite (pair.car);
    temp_decl.noteValue (inits[0]);
    let.push (tr);
    Expression temp_exp = new ReferenceExp (temp_name, temp_decl);
    Expression rest = rewrite (pair.cdr, tr); // self-recurse
    Expression then_clause, else_clause;
    if (is_and)
      {
	then_clause = rest;
	else_clause = temp_exp;
      }
    else
      {
	then_clause = temp_exp;
	else_clause = rest;
      }
    let.body = new IfExp (temp_exp, then_clause, else_clause);
    let.pop (tr);
    return let;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print(is_and ? "#<builtin and>" : "#<builtin or>");
  }
};
