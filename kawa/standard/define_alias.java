package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof Declaration && p1.cdr instanceof Pair)
	  {
            Declaration decl = (Declaration) p1.car;
            String name = decl.getName();
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		Expression arg = location.rewriteArg(p2.car, tr);
		Expression loc = location.rewrite(arg, tr);
		SetExp sexp = new SetExp(name, loc);
		sexp.binding = decl;
		decl.noteValue(loc);
		sexp.setDefining (true);
		return sexp;
	      }
	  }
      }
    return tr.syntaxError ("invalid syntax for define-alias");
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair)
        || ! (((Pair) st.cdr).car instanceof String))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    Type typeLocation = ClassType.make("gnu.mapping.Location");
    Declaration decl = defs.addDeclaration((String) name, typeLocation);
    decl.setIndirectBinding(true);
    decl.setAlias(true);
    st = tr.makePair(st, this, tr.makePair(p, decl, p.cdr));
    forms.addElement(st);
    return true;
  }
}
