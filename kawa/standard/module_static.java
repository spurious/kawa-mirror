package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class module_static extends Syntax
{
  public static final module_static module_static = new module_static();
  static { module_static.setName("module-static"); }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object list = st.cdr;
    if (! (defs instanceof ModuleExp))
      {
	tr.error('e', "\'" + getName() + "\' not at module level");
	return true;
      }
    if (list instanceof Pair
	&& (st = (Pair) list).cdr == LList.Empty
	&& st.car instanceof Boolean)
      {
	if (st.car == Boolean.FALSE)
	  ((ModuleExp) defs).setFlag(ModuleExp.NONSTATIC_SPECIFIED);
	else
	  ((ModuleExp) defs).setFlag(ModuleExp.STATIC_SPECIFIED);
      }
    else
      {
	((ModuleExp) defs).setFlag(ModuleExp.NONSTATIC_SPECIFIED);


	while (list != LList.Empty)
	  {
	    if (! (list instanceof Pair)
		|| ! ((st = (Pair) list).car instanceof String))
	      {
		tr.error('e', "invalid syntax in '" + getName() + '\'');
		return false;
	      }
	    String symbol = (String) st.car;
	    Declaration decl = defs.getNoDefine(symbol);
	    if (decl.getFlag(Declaration.NOT_DEFINING))
	      Translator.setLine(decl, st);
	    decl.setFlag(Declaration.STATIC_SPECIFIED);
	    list = st.cdr;
	  }
      }
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
