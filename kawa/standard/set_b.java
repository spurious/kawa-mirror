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

  public static final set_b set = new set_b();
  static { set.setName("set!"); }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object o1 = form.cdr;
    SyntaxForm syntax = null;
    while (o1 instanceof SyntaxForm)
      {
	syntax = (SyntaxForm) o1;
	o1 = syntax.form;
      }
    if (! (o1 instanceof Pair))
      return tr.syntaxError ("missing name");
    Pair p1 = (Pair) o1;
    Expression name = tr.rewrite_car(p1, syntax);
    Object o2 = p1.cdr;
    while (o2 instanceof SyntaxForm)
      {
	syntax = (SyntaxForm) o2;
	o2 = syntax.form;
      }
    Pair p2;
    if (! (o2 instanceof Pair)
	|| (p2 = (Pair) o2).cdr != LList.Empty)
      return tr.syntaxError ("missing or extra arguments to set!");
    Expression value = tr.rewrite_car(p2, syntax);

    if (name instanceof ApplyExp)
      {
	// rewrite (set! (proc . args) rhs) => ((setter proc) args ... rhs)

	ApplyExp aexp = (ApplyExp) name;
	int nargs = aexp.getArgCount();
	Expression[] xargs = new Expression[nargs+1];
	System.arraycopy(aexp.getArgs(), 0, xargs, 0, nargs);
	xargs[nargs] = value;
        Expression[] setterArgs = { aexp.getFunction() };
	return new ApplyExp(new ApplyExp(new ReferenceExp(setterDecl),
                                         setterArgs), xargs);
      }
    else if (! (name instanceof ReferenceExp))
      return tr.syntaxError ("first set! argument is not a variable name");    

    ReferenceExp ref = (ReferenceExp) name;
    Declaration decl = ref.getBinding();
    SetExp sexp = new SetExp (ref.getSymbol(), value);
    if (decl != null)
      {
	sexp.setBinding(decl);
	decl = Declaration.followAliases(decl);
	if (decl != null)
	  decl.noteValue (value);
	if (decl.getFlag(Declaration.IS_CONSTANT))
	  return tr.syntaxError ("constant variable is set!");
      }
    return sexp;
  }
}
