package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class export extends Syntax
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object list = st.cdr;
    if (defs instanceof ModuleExp)
      {
	((ModuleExp) defs).setFlag(ModuleExp.EXPORT_SPECIFIED);
      }
    else
      {
	tr.error('e', "\'" + getName() + "\' not at module level");
	return true;
      }
    while (list != LList.Empty)
      {
	if (! (st instanceof Pair)
	    || ! ((st = (Pair) list).car instanceof String))
	  {
	    tr.error('e', "invalid syntax in '" + getName() + '\'');
	    return false;
	  }
	String symbol = (String) st.car;
	Declaration decl = defs.getNoDefine(symbol);
	if (decl.getFlag(Declaration.NOT_DEFINING))
	  Translator.setLine(decl, st);
	decl.setFlag(Declaration.EXPORT_SPECIFIED);
	list = st.cdr;
      }
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
