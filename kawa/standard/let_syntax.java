package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implementation of he standard Scheme let-syntax and letrec-syntax forms.
 * Not quite working yet. */

public class let_syntax extends Syntax implements Printable
{
  boolean recursive;

  public let_syntax(boolean recursive)
  {
    this.recursive = recursive;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let-syntax arguments");
    Pair pair = (Pair) obj;
    Object bindings = pair.car;
    Object body = pair.cdr;
    int decl_count = LList.length (bindings);
    Expression[] inits = new Expression[decl_count];
    Macro[] decls = new Macro[decl_count];
    LetExp let = new LetExp (inits);
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
	if (! (bind_pair.car instanceof Pair))
	  return tr.syntaxError ("let binding is not a pair");
	Pair binding = (Pair) bind_pair.car;
	if (! (binding.car instanceof String))
	  return tr.syntaxError("variable in let binding is not a symbol");
	String name = (String) binding.car;
	if (! (binding.cdr instanceof Pair))
	  return tr.syntaxError("let has no value for `"+name+"'");
	binding = (Pair) binding.cdr;
	if (binding.cdr != LList.Empty)
	  return tr.syntaxError("let binding for `"+name+"' is improper list");
        decls[i] = new Macro(name, binding.car);
        let.addDeclaration(decls[i]);
	inits[i] = QuoteExp.nullExp;
	bindings = bind_pair.cdr;
      }
    if (recursive)
      tr.push(let);
    for (int i = 0; i < decl_count; i++)   
      {
        inits[i] = tr.rewrite(decls[i].transformer);
        decls[i].noteValue(inits[i]);
      }
    if (! recursive)
      tr.push(let);
    Expression result = tr.rewrite_body(body);
    tr.pop(let);
    return result;
  }
}
