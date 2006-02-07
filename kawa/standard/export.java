package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class export extends Syntax
{
  public static final export module_export = new export();
  static { module_export.setName("module-export"); }

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
        Object symbol;
	if (! (st instanceof Pair)
	    || ! ((symbol = (st = (Pair) list).car) instanceof String
                  || (symbol instanceof gnu.mapping.Symbol)))
	  {
	    tr.error('e', "invalid syntax in '" + getName() + '\'');
	    return false;
	  }
        if (symbol instanceof String)
          {
            String str = (String) symbol;
            if (str.startsWith("namespace:"))
              {
                tr.error('w', "'namespace:' prefix ignored");
                symbol = str.substring(10).intern();
              }
          }
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
