package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.kawa.functions.Setter;

/**
 * The Syntax transformer that re-writes the Scheme "set!" primitive.
 * @author	Per Bothner
 */

public class set_b extends Syntax implements Printable
{
  static final ClassType setterType = ClassType.make("gnu.kawa.functions.Setter");
  static final Field setterField = setterType.getDeclaredField("setter");
  static final Declaration setterDecl = new Declaration("setter", setterField);
  static { setterDecl.noteValue(new QuoteExp(Setter.setter)); }

  static private Pattern pattern = new ListPat (2, 2);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing or extra arguments to set!");

    if (match[0] instanceof Pair)
      {
	// FIXME use location.rewrite_arg.
	// rewrite (set! (proc . args) rhs) => ((setter proc) args ... rhs)
	Pair pair = (Pair) match[0];
	Object proc = pair.car;
	Object args = pair.cdr;

	int nargs = LList.length(args);
	Expression[] xargs = new Expression[nargs+1];
	for (int i = 0; i < nargs; i++)
	  {
	    pair = (Pair) args;
	    xargs[i] = tr.rewrite(pair.car);
	    args = pair.cdr;
	  }
	xargs[nargs] = tr.rewrite(match[1]);
        Expression[] setterArgs = { tr.rewrite(proc) };
	return new ApplyExp(new ApplyExp(new ReferenceExp(setterDecl),
                                         setterArgs), xargs);
      }

    Object sym = match[0];
    if (! (sym instanceof String)  && ! (sym instanceof Symbol))
      return tr.syntaxError ("first set! argument is not a variable name");
    Expression value = tr.rewrite (match[1]);
    Declaration decl = tr.lexical.lookup(sym, Interpreter.VALUE_NAMESPACE);
    // Hygenic macro expansion may bind a renamed (uninterned) symbol
    // to the original symbol.  Here, use the original symbol.
    if (decl != null  && decl.isAlias()
	&& decl.getValue() instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) decl.getValue();
	decl = rexp.getBinding();
	if (decl == null)
	  return new SetExp(rexp.getSymbol(), value);
      }
    SetExp sexp = new SetExp (sym, value);
    if (decl != null)
      {
	sexp.binding = decl;
	decl = Declaration.followAliases(decl);
	if (decl != null)
	  decl.noteValue (value);
	if (decl.getFlag(Declaration.IS_CONSTANT))
	  return tr.syntaxError ("constant variable is set!");
      }
    return sexp;
  }
}
