package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.expr.*;

public class defgroup extends Syntax
{
  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    // ignore, for now.  FIXME!
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
