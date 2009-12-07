package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class export extends Syntax
{
  public static final export module_export = new export();
  static { module_export.setName("module-export"); }

  public static final export export = new export();
  static { module_export.setName("export"); }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object list = st.getCdr();
    Object savePos = tr.pushPositionOf(st);
    try
      {
        if (defs instanceof ModuleExp)
          ((ModuleExp) defs).setFlag(ModuleExp.EXPORT_SPECIFIED);
        else
          {
            tr.error('e', "\'" + getName() + "\' not at module level");
            return true;
          }
        SyntaxForm restSyntax = null;
        while (list != LList.Empty)
          {
            tr.pushPositionOf(list);
            while (list instanceof SyntaxForm)
              {
                restSyntax = (SyntaxForm) list;
                list = restSyntax.getDatum();
              }
            SyntaxForm nameSyntax = restSyntax;
            if (list instanceof Pair)
              {
                st = (Pair) list;
                Object symbol = st.getCar();
                while (symbol instanceof SyntaxForm)
                  {
                    nameSyntax = (SyntaxForm) symbol;
                    symbol = nameSyntax.getDatum();
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
                if (symbol instanceof String
                    || symbol instanceof gnu.mapping.Symbol)
                  {
                    if (nameSyntax != null)
                      {
                        // Difficult to implement correctly.  And probably
                        // not much point in doing so.  FIXME.
                      }
                    Declaration decl = defs.getNoDefine(symbol);
                    if (decl.getFlag(Declaration.NOT_DEFINING))
                      Translator.setLine(decl, st);
                    decl.setFlag(Declaration.EXPORT_SPECIFIED);
                    list = st.getCdr();
                    continue;
                  }
              }
            tr.error('e', "invalid syntax in '" + getName() + '\'');
            return false;
          }
        return true;
      }
    finally
      {
        tr.popPositionOf(savePos);
      }
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
